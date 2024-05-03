## 03_create_analytic_dataset.R ----
## 
## Create analytic dataset.

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(foreach)
library(doParallel)
library(narcan)  # remotes::install_github("mkiang/narcan")
library(future)
library(furrr)
source(here::here("codes", "utils.R"))

## Bug workaround
## See: https://github.com/rstudio/rstudio/issues/6692
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

## Constants ----
DATA_FOLDER <- here::here("inputs", "data_public")

## Data ----
all_pops_long <- readRDS(here::here(
    DATA_FOLDER,
    "pop_est_national_single_year_age_1990to2020_long.RDS"
))
summarized_deaths <- readRDS(here::here(DATA_FOLDER, "summarized_deaths_1990_2020.RDS"))

## Create the actual analytic dataset ----
pop_skeleton <- all_pops_long %>% 
    dplyr::mutate(race_eth = dplyr::case_when(
        hispanic == 1 ~ "hispanic",
        TRUE ~ race
    ),
    sex = dplyr::case_when(female == 1 ~ "female",
                    female == 0 ~ "male",
                    TRUE ~ NA_character_)) %>% 
    dplyr::rename(age_years = age) %>% 
    dplyr::group_by(year, race_eth, sex, age_years) %>% 
    dplyr::summarize(pop = sum(pop_est)) %>% 
    dplyr::ungroup()

analytic_df <- pop_skeleton %>% 
    dplyr::left_join(summarized_deaths) %>% 
    dplyr::mutate(n_deaths = tidyr::replace_na(n_deaths, 0),
           n_opioid = tidyr::replace_na(n_opioid, 0),
           n_drug = tidyr::replace_na(n_drug, 0),
           n_firearm = tidyr::replace_na(n_firearm, 0)) |> 
    dplyr::mutate(n_opioid = ifelse(year < 1999, NA, n_opioid), 
           n_drug = ifelse(year < 1999, NA, n_drug), 
           n_firearm = ifelse(year < 1999, NA, n_firearm))

## Save ----
saveRDS(
    analytic_df, 
    here::here(
        DATA_FOLDER,
        "national_year_age-sex-race_drug-opioid-mortality.RDS"
    ),
    compress = "xz"
)
