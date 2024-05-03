## 01_download_process_deaths.R ----
## 
## Download and process the public use NCHS multiple cause of death data. 
## NOTE: Requires narcan. Use `remotes::intall_github("mkiang/narcan")`.

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(foreach)
library(haven)
library(doParallel)
library(narcan)  # remotes::install_github("mkiang/narcan")
library(future)
library(furrr)
source(here::here("codes", "utils.R"))

## Bug workaround
## See: https://github.com/rstudio/rstudio/issues/6692
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" &&
    !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

## Constants ----
START_YEAR <- 1990
END_YEAR <-  2020
NCORES <-  8
RAW_FOLDER <- here::here("inputs", "data_raw")
DATA_FOLDER <- here::here("inputs", "data_public")
FORCE_REFRESH <- FALSE
KEEP_ZIPS <- TRUE
years <- START_YEAR:END_YEAR

## Download raw files ----
## NOTE: We need the raw files because CDC WONDER will only provide 5-year
## age bins. Also, I download the files sequentially to be nice to their
## poor poor server. Do not parallelize the file download.
## https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Mortality_Multiple

## NBER server is slow -- up the timeout limit
options(timeout = 30 * 60 * 60)

for (y in years) {
    download_url <-
        paste0(
            "https://ftp.cdc.gov/pub/Health_Statistics/",
            "NCHS/Datasets/DVS/mortality/",
            sprintf("mort%sus.zip", y)
        )
    
    ## For some reason, the 2020 zip file from NCHS/NBER gives me errors.
    ## Download the dta from NBER and then zip it manually.
    if (y == 2020) {
        if (!fs::file_exists(here::here(RAW_FOLDER, "mort2020us.dta.zip"))) {
            download_url <- paste0(
                "https://data.nber.org/nvss/",
                "mortality/dta/Mort2020US.PubUse.dta"
            )
            
            utils::download.file(download_url,
                                 here::here(RAW_FOLDER, basename(download_url)))
            utils::zip(
                here::here(RAW_FOLDER, "mort2020us.dta.zip"),
                here::here(RAW_FOLDER, basename(download_url))
            )
            fs::file_delete(here::here(RAW_FOLDER, basename(download_url)))
        }
    } else {
        if (!fs::file_exists(here::here(RAW_FOLDER, basename(download_url)))) {
            utils::download.file(download_url,
                                 here::here(RAW_FOLDER, basename(download_url)))
        } else {
            print(sprintf("Skipping: %s", basename(download_url)))
        }
    }
}

## Process raw files ----
doParallel::registerDoParallel(cores = NCORES)
holder <- foreach::foreach(i = 1:NROW(years), .inorder = FALSE) %dopar% {
    f_path <- here::here(DATA_FOLDER,
                         sprintf("processed_mcod_%i.RDS", years[i]))
    
    if (!fs::file_exists(f_path) | FORCE_REFRESH) {
        if (years[i] == 2020) {
            temp_df <-
                haven::read_dta(here::here(RAW_FOLDER, sprintf("mort%sus.dta.zip", years[i]))) |>
                dplyr::mutate(year = years[i])
        } else {
            temp_df <-
                narcan:::.import_restricted_data(here::here(RAW_FOLDER, sprintf("mort%sus.zip", years[i])),
                                                 year_x = years[i]) |>
                dplyr::mutate(year = years[i])
        }
        
        ### Subset to US residents ----
        temp_df <- narcan::subset_residents(temp_df)
        
        ### Unite all 20 contributory cause columns ----
        ### For 1990 to 1998, it is ICD-9 and we only need all cause mortality
        ### so don't need to unite records.
        ###
        ### For 2020, we are using a DTA that has weird characters so trim the
        ### white space.
        if (years[i] >= 1999) {
            temp_df <- narcan::unite_records(temp_df)
            temp_df$f_records_all <-
                trimws(temp_df$f_records_all)
        }
        
        temp_df <- temp_df %>%
            dplyr::select(
                -dplyr::starts_with("econd"),
                -dplyr::starts_with("enicon"),
                -dplyr::starts_with("record_"),
                -dplyr::starts_with("rnifla_"),
                -dplyr::starts_with("entity"),
                -dplyr::starts_with("eniflag"),
                -dplyr::one_of(c("eanum", "ranum"))
            )
        
        ## Recode detailed age age_years ----
        if (years[i] %in% 1990:2002) {
            temp_df <- temp_df %>%
                dplyr::mutate(
                    age_years = dplyr::case_when(
                        age %in% c(299, 399, 499, 599, 699, 999) ~ NA_real_,
                        age > 199 ~ 0,
                        age <= 199 ~ age,
                        TRUE ~ NA_real_
                    )
                )
        } else {
            temp_df <- temp_df %>%
                dplyr::mutate(
                    age_years = dplyr::case_when(
                        age %in% c(9999, 1999, 2999, 4999, 5999, 6999) ~ NA_real_,
                        age > 1999 ~ 0,
                        age < 1999 ~ age - 1000,
                        TRUE ~ NA_real_
                    )
                )
        }
        
        ### Fix sex category ----
        temp_df <- temp_df %>%
            dplyr::mutate(
                sex = dplyr::case_when(
                    sex == 1 ~ "male",
                    sex == "M" ~ "male",
                    sex == 2 ~ "female",
                    sex == "F" ~ "female",
                    sex == "Male" ~ "male",
                    sex == "Female" ~ "female",
                    TRUE ~ as.character(sex)
                )
            )
        
        ### Fix race/ethnicity ----
        temp_df <- temp_df  %>%
            narcan::convert_ager27(.) %>%
            create_big_race() %>%
            dplyr::mutate(
                hispanic_cat = narcan::categorize_hspanicr(hspanicr),
                age_cat  = narcan::categorize_age_5(age)
            )
        
        ### Reorder columns ----
        temp_df <- temp_df %>%
            dplyr::select(
                dplyr::one_of(
                    "year",
                    "age",
                    "age_cat",
                    "age_years",
                    "race",
                    "race_bridged",
                    "hispanic_cat",
                    "sex",
                    "ucod",
                    "f_records_all"
                ),
                dplyr::everything()
            )
        
        ### Save processed files ----
        saveRDS(temp_df, f_path, compress = "xz")
    }
}

### Close out ----
doParallel::stopImplicitCluster()
closeAllConnections()

## Summarize deaths ----
### Set your max number of cores ----
if (!fs::file_exists(here::here("data", "summarized_deaths_1990_2020.RDS"))) {
    future::plan(future::multisession(workers = NCORES))
    summarized_deaths <- furrr::future_map_dfr(
        .x = years,
        .f = ~ {
            ### Read in processed death file ----
            temp_x <-
                readRDS(here::here(DATA_FOLDER,
                                   sprintf("processed_mcod_%i.RDS", .x)))
            
            ### Recode race to match population data ----
            temp_x <-
                temp_x %>%
                dplyr::mutate(race_eth = dplyr::case_when(hspanicr %in% c(1:5, 9) ~ "hispanic",
                                                          TRUE ~ race_bridged))
            
            ### Top code age to 85 to match population ----
            temp_x <-
                temp_x %>%
                dplyr::mutate(age_years = dplyr::case_when(age_years > 85 ~ 85,
                                                           TRUE ~ age_years))
            
            ### Flag death types ----
            ### ICD codes for firearm mortality from:
            ### https://www.nejm.org/doi/suppl/10.1056/NEJMc2201761/suppl_file/nejmc2201761_appendix.pdf
            if (.x %in% 1999:2020) {
                temp_x <- temp_x %>%
                    narcan::flag_drug_deaths(.x) %>%
                    dplyr::mutate(firearm_death = 0 + grepl(
                        paste0(
                            "\\<W3[234]{1}|\\<X7[234]{1}|",
                            "\\<X9[345]{1}|\\<Y2[234]{1}|",
                            "\\<Y350|\\<U014"
                        ),
                        ucod
                    )) %>%
                    narcan::flag_opioid_deaths(.x) %>%
                    dplyr::mutate(all_deaths = 1)
            } else {
                temp_x <- temp_x %>%
                    dplyr::mutate(
                        drug_death = NA,
                        firearm_death = NA,
                        opioid_death = NA,
                        all_deaths = 1
                    )
            }
            
            ### Summarize ----
            temp_x %>%
                dplyr::group_by(year, age_years, sex, race_eth) %>%
                dplyr::summarize(
                    n_deaths = sum(all_deaths),
                    n_opioid = sum(opioid_death),
                    n_drug = sum(drug_death),
                    n_firearm = sum(firearm_death)
                ) %>%
                dplyr::ungroup() %>%
                dplyr::arrange(year, race_eth, sex, age_years)
        })
    
    ### Close out ----
    doParallel::stopImplicitCluster()
    closeAllConnections()
    
    ## Save ----
    saveRDS(
        summarized_deaths,
        here::here("data", "summarized_deaths_1990_2020.RDS"),
        compress = "xz"
    )
}