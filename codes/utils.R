library(tidyverse)
library(parallel)

create_big_race <- function(df) {
    df %>% 
        mutate(race_bridged = case_when(
            race %in% c(1) ~ "white",
            race %in% c(2) ~ "black",
            race %in% c(3) ~ "aian",
            race %in% c(4:7, 18, 28, 38, 48, 58, 68, 78) ~ "api",
            TRUE ~ NA_character_
        ))
}

recode_age <- function(df) {
    df %>% 
        mutate(age_years = case_when(
            age %in% c(9999, 1999, 2999, 4999, 5999, 6999) ~ NA_real_, 
            age > 1999 ~ 0,
            age < 1999 ~ age - 1000,
            TRUE ~ NA_real_
        ))
}

categorize_race <- function(df) {
    df |>
        mutate(race_cat = factor(
            race,
            levels = c("hispanic", "black", "white", "total"),
            labels = c(
                "Hispanic",
                "Non-Hispanic Black",
                "Non-Hispanic White",
                "Total"
            ),
            ordered = TRUE
        )) |>
        mutate(race_cat_rev = factor(
            race,
            levels = rev(c("hispanic", "black", "white", "total")),
            labels = rev(c(
                "Hispanic",
                "Non-Hispanic Black",
                "Non-Hispanic White",
                "Total"
            )),
            ordered = TRUE
        )) |>
        arrange(race_cat)
}

## Extra data ----
## Dataframe of state abbreviation, name, fips, and division mapping ----
## Early years of MCOD files use NCHS-specific state codes.
nchs_state_codes <-
    list(
        "AL" = "01",
        "AK" = "02",
        "AZ" = "03",
        "AR" = "04",
        "CA" = "05",
        "CO" = "06",
        "CT" = "07",
        "DE" = "08",
        "DC" = "09",
        "FL" = "10",
        "GA" = "11",
        "HI" = "12",
        "ID" = "13",
        "IL" = "14",
        "IN" = "15",
        "IA" = "16",
        "KS" = "17",
        "KY" = "18",
        "LA" = "19",
        "ME" = "20",
        "MD" = "21",
        "MA" = "22",
        "MI" = "23",
        "MN" = "24",
        "MS" = "25",
        "MO" = "26",
        "MT" = "27",
        "NE" = "28",
        "NV" = "29",
        "NH" = "30",
        "NJ" = "31",
        "NM" = "32",
        "NY" = "33",
        "NC" = "34",
        "ND" = "35",
        "OH" = "36",
        "OK" = "37",
        "OR" = "38",
        "PA" = "39",
        "RI" = "40",
        "SC" = "41",
        "SD" = "42",
        "TN" = "43",
        "TX" = "44",
        "UT" = "45",
        "VT" = "46",
        "VA" = "47",
        "WA" = "48",
        "WV" = "49",
        "WI" = "50",
        "WY" = "51"
    )

return_st_info <- function() {
    st_info <- dplyr::tibble(
        abbrev   = datasets::state.abb,
        division = as.character(datasets::state.division),
        st_lat   = datasets::state.center$y,
        st_lon   = datasets::state.center$x
    ) %>%
        ## We have to add DC because it's not a state
        dplyr::add_row(
            abbrev = "DC",
            division = "South Atlantic",
            st_lat = 38.9072,
            st_lon = -77.0369
        ) %>%
        dplyr::left_join(
            narcan::st_fips_map %>%
                mutate(st_fips = sprintf("%02d", fips),
                       nchs_fips = sprintf("%02d", nchs)),
            by = "abbrev"
        ) %>% 
        ## Add in whole US and NA
        dplyr::add_row(
            abbrev = "US",
            name = "zzWhole US",
            division = "Whole US",
            st_lat = 0,
            st_lon = 200,
            st_fips = "999",
            nchs_fips = "999"
        ) %>%
        dplyr::add_row(
            abbrev = NA,
            name = "zzzUnknown State",
            division = "Unknown",
            st_lat = 0,
            st_lon = 199,
            st_fips = NA_character_,
            nchs_fips = NA_character_
        ) %>%
        dplyr::arrange(st_lon) %>%
        dplyr::mutate(
            lon_rank = dplyr::dense_rank(st_lon),
            alpha_rank = dplyr::dense_rank(name)
        ) %>%
        dplyr::mutate(name = gsub("zz|zzz", "", name))
    
    st_info <- st_info %>%
        dplyr::mutate(
            st_cat = factor(
                abbrev,
                levels = st_info %>%
                    dplyr::arrange(lon_rank) %>%
                    dplyr::pull(abbrev),
                ordered = TRUE
            ),
            name_cat = factor(
                name,
                levels = st_info %>%
                    dplyr::arrange(name) %>%
                    dplyr::pull(name),
                ordered = TRUE
            ),
            name_cat_alpha = factor(
                name,
                levels = st_info %>%
                    dplyr::arrange(alpha_rank) %>%
                    dplyr::pull(name),
                ordered = TRUE
            )
        )
    
    st_info %>%
        arrange(nchs_fips)
}

return_ihme_fips <- function() {
    narcan::ihme_fips %>%
        ## Nansemond County, Virginia (51-123):
        ## Combined into Suffolk (independent) city (51-800)
        ## effective July 1, 1972.
        add_case(
            state = "Virginia",
            group = 16,
            orig_fips = "51123",
            ihme_fips = "51800"
        ) %>%
        add_case(
            state = "Virginia",
            group = 16,
            orig_fips = "51800",
            ihme_fips = "51800"
        ) %>% 
        ## Poquoson (independent) city, Virginia (51-735):
        ## Changed from a town to a city and became independent of 
        ## York County (51-199) effective June 1, 1975
        add_case(
            state = "Virginia",
            group = 15,
            orig_fips = "51735",
            ihme_fips = "51199"
        ) 
}

## Clean up ----
rm(nchs_state_codes)

saveRDS_xz <- function(object, file, threads = parallel::detectCores() - 1) {
    ## https://gist.github.com/ShanSabri/b1bdf0951efa0dfee0edeb5509f87e88
    ## If xz version 5 or higher is installed, parallelize
    if (any(grepl("(XZ Utils) 5.", system("xz -V", intern = TRUE), fixed = TRUE))) {
        con <- pipe(paste0("xz -T", threads, " > ", file), "wb")
        saveRDS(object, file = con)
        close(con)
    } else {
        saveRDS(object, file = file, compress = "xz")
    }
}

readRDS_xz <- function(file, threads = parallel::detectCores() - 1) {
    ## https://gist.github.com/ShanSabri/b1bdf0951efa0dfee0edeb5509f87e88
    ## If xz version 5 or higher is installed, parallelize
    ## This does not appear to actually run multiple cores (?). Maybe try
    ## to fix later. 
    # if (any(grepl("(XZ Utils) 5.", x, fixed = TRUE))) {
    #     con <- pipe(paste0("xz -d -k -c -T", threads, " ", file), "rb")
    #     readRDS(file = con)
    #     close(con)
    # } else {
        readRDS(file = file)
    # }
}
