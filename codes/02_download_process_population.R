## 02_download_process_population.R ----
## Population estimates from CDC. 

## Imports ----
library(tidyverse)
library(here)
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
NCORES <-  8
RAW_FOLDER <- here::here("inputs", "data_raw")
DATA_FOLDER <- here::here("inputs", "data_public")
FORCE_REFRESH <- FALSE
KEEP_ZIPS <- TRUE

## (1) Downloading files ----
### 1990 to 1999 national data ----
if (!file.exists(here::here(RAW_FOLDER, "icen_2000_09_y0004.zip"))) {
    utils::download.file(
        url = paste0(
            "https://ftp.cdc.gov/pub/Health_Statistics/", 
            "NCHS/datasets/nvss/bridgepop/icen_natA1.txt"
        ),
        destfile = here::here(RAW_FOLDER, "icen_natA1.txt")
    )
}

### 2000-2004 data ----
if (!file.exists(here::here(RAW_FOLDER, "icen_2000_09_y0004.zip"))) {
    utils::download.file(
        url = paste0(
            "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/",
            "datasets/nvss/bridgepop/2000_09/",
            "icen_2000_09_y0004.zip"
        ),
        destfile = here::here(RAW_FOLDER, "icen_2000_09_y0004.zip")
    )
}

### 2005-2009 data ----
if (!file.exists(here::here(RAW_FOLDER, "icen_2000_09_y0509.zip"))) {
    utils::download.file(
        url = paste0(
            "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/",
            "datasets/nvss/bridgepop/2000_09/",
            "icen_2000_09_y0509.zip"
        ),
        destfile = here::here(RAW_FOLDER, "icen_2000_09_y0509.zip")
    )
}

### 2010-2020 data (get the 2020 Vintage) ----
if (!file.exists(here::here(RAW_FOLDER, "pcen_v2020_y1020_txt.zip"))) {
    utils::download.file(
        url = paste0(
            "https://www.cdc.gov/nchs/nvss/bridged_race/", 
            "pcen_v2020_y1020_txt.zip"
        ),
        destfile = here::here(RAW_FOLDER, "pcen_v2020_y1020_txt.zip")
    )
}

## (2) Reshape population data ----
if (!file.exists(here::here(DATA_FOLDER,
                      "pop_est_national_single_year_age_1990to2020_long.RDS"))) {
    
    ### 1990s ----
    widths_90s <- c(2, 3, 2, 1, 1, rep(8, 10))
    pop1990s <- readr::read_fwf(here::here(RAW_FOLDER, "icen_natA1.txt"),
                         readr::fwf_widths(
                             widths_90s,
                             c(
                                 "fipst",
                                 "fipsct",
                                 "age",
                                 "racesex",
                                 "hispanic",
                                 "pop_est_1990",
                                 "pop_est_1991",
                                 "pop_est_1992",
                                 "pop_est_1993",
                                 "pop_est_1994",
                                 "pop_est_1995",
                                 "pop_est_1996",
                                 "pop_est_1997",
                                 "pop_est_1998",
                                 "pop_est_1999"
                             )
                         ))
    
    pop1990s <- pop1990s %>%
        dplyr::select(-fipsct, -fipst) %>%
        dplyr::group_by(age, racesex, hispanic) %>%
        dplyr::summarize_all(sum) %>%
        dplyr::ungroup()
    
    ### 2000s ----
    width_00s <- c(8, 2, 3, 2, 1, 1, 8, 8, 8, 8, 8)
    
    ## We can left_join() here because 2000-2009 have the same FIPS, but this
    ## is not generally true -- be careful with joining different pop files.
    pop2000s <- dplyr::left_join(
        readr::read_fwf(
            here::here(RAW_FOLDER, "icen_2000_09_y0004.zip"),
            readr::fwf_widths(
                width_00s,
                c(
                    "series",
                    "fipst",
                    "fipsct",
                    "age",
                    "racesex",
                    "hispanic",
                    "pop_est_2000",
                    "pop_est_2001",
                    "pop_est_2002",
                    "pop_est_2003",
                    "pop_est_2004"
                )
            )
        ),
        readr::read_fwf(
            here::here(RAW_FOLDER, "icen_2000_09_y0509.zip"),
            readr::fwf_widths(
                width_00s,
                c(
                    "series",
                    "fipst",
                    "fipsct",
                    "age",
                    "racesex",
                    "hispanic",
                    "pop_est_2005",
                    "pop_est_2006",
                    "pop_est_2007",
                    "pop_est_2008",
                    "pop_est_2009"
                )
            )
        ))
    
    pop2000s <- pop2000s %>%
        dplyr::select(-series, -fipsct, -fipst) %>%
        dplyr::group_by(age, racesex, hispanic) %>%
        dplyr::summarize_all(sum) %>%
        dplyr::ungroup()
    
    ### 2010s ----
    pop2010s <- readr::read_fwf(here::here(RAW_FOLDER, "pcen_v2020_y1020_txt.zip"),
                         readr::fwf_widths(
                             c(4, 2, 3, 2, 1, 1, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8),
                             c(
                                 "series",
                                 "fipst",
                                 "fipsct",
                                 "age",
                                 "racesex",
                                 "hispanic",
                                 "pop_est_2010april",
                                 "pop_est_2010",
                                 "pop_est_2011",
                                 "pop_est_2012",
                                 "pop_est_2013",
                                 "pop_est_2014",
                                 "pop_est_2015",
                                 "pop_est_2016",
                                 "pop_est_2017",
                                 "pop_est_2018",
                                 "pop_est_2019",
                                 "pop_est_2020"
                             )
                         ))
    
    pop2010s <- pop2010s %>%
        dplyr::select(-series, -fipsct, -fipst) %>%
        dplyr::group_by(age, racesex, hispanic) %>%
        dplyr::summarize_all(sum) %>%
        dplyr::ungroup()
    
    ## Now modify each population dataframe with better race/sex/hispanic columns
    ## such that race and sex are separated and hispanic/female are indicators.
    clean_pop_data <- function(df) {
        newdf <- df %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
                race = dplyr::case_when(
                    racesex %in% 1:2 ~ "white",
                    racesex %in% 3:4 ~ "black",
                    racesex %in% 5:6 ~ "aian",
                    racesex %in% 7:8 ~ "api"
                ),
                female = ifelse(racesex %% 2 == 0, 1, 0),
                hispanic = hispanic - 1
            )
        return(newdf)
    }
    
    pop1990s <- clean_pop_data(pop1990s)
    pop2000s <- clean_pop_data(pop2000s)
    pop2010s <- clean_pop_data(pop2010s)
    
    ## Now collapse over the IHME FIPS codes ----
    ## Then combine into a single file.
    collapse_df <- function(df) {
        newdf <- df %>%
            dplyr::select(female,
                   age,
                   race,
                   hispanic,
                   dplyr::starts_with("pop_est_")) %>%
            dplyr::group_by(age, race, hispanic, female) %>%
            dplyr::summarize_all(sum, na.rm = TRUE) %>%
            dplyr::ungroup()
        return(newdf)
    }
    
    pop1990s <- collapse_df(pop1990s)
    pop2000s <- collapse_df(pop2000s)
    pop2010s <- collapse_df(pop2010s)
    
    all_pops_wide <- pop1990s |> 
        dplyr::left_join(pop2000s) %>%
        dplyr::left_join(pop2010s) %>%
        dplyr::ungroup()
    
    ## Convert to long
    all_pops_long <- all_pops_wide %>%
        dplyr::ungroup() %>%
        dplyr::select(-dplyr::ends_with("april")) %>%
        tidyr::gather(year, pop_est, pop_est_1990:pop_est_2020) %>%
        dplyr::mutate(year = as.integer(gsub("pop_est_", "", year)))
    
    ## Save ----
    saveRDS(
        all_pops_long,
        here::here(DATA_FOLDER,
             "pop_est_national_single_year_age_1990to2020_long.RDS"),
        compress = "xz"
    )
} 

## Clean up ----
if (!KEEP_ZIPS) {
    unlink(paste0(
        RAW_FODER,
        c(
            "icen_natA1.txt", 
            "/icen_2000_09_y0004.zip",
            "/icen_2000_09_y0509.zip",
            "/pcen_v2020_y1020_txt.zip"
        )
    ))
}
