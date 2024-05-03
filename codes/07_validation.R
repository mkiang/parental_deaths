
##==============================================================================
##
##  Project title: Parental loss from drugs/firearms
##
##  Validation by comparing orphanhood prevalences from NSCH against the ones
##  obtain from the model.
##
## 
##  Author: Benjamin Schlüter
##  Date: April 2024
##==============================================================================
##
##  Notes
## ------
## 1. 
## 2.
## 
##==============================================================================





## LOAD PACKAGES ---------------------------------------------------------------

# Install/load packages
packages <- c("tidyverse", "here", "rvest", "stringr", "MASS")
for(p in packages){
        if(!require(p,character.only = TRUE)) install.packages(p)
        library(p,character.only = TRUE)
}





## FUNCTIONS -------------------------------------------------------------------

# Function to obtain quantiles of quantities of interest
get_quantiles <- function(x, var, ...) {
        
        qs <- quantile(x[[var]],
                       probs = c(0.025, 0.5, 0.975))
        out <- tibble(
                median = qs[2],
                lower95 = qs[1],
                upper95 = qs[3]
        )
}


## Note I do the file check earlier than normal because a_x_t takes so
## long to load that it's worth checking early. 
if (!file.exists(here("outputs",
                      "data_public",
                      "df_prevalence_parental_loss.rds"))) {
    ## LOAD DATA -------------------------------------------------------------------
    # Overdispersion parameters for NBin in MC
    theta.dth <- readRDS(here("outputs",
                              "data_public",
                              "overdisp_dth.rds")) |>
        dplyr::select(-c(mu, var))
    # Mortality data
    lt.US <- readRDS(here("outputs",
                          "data_public",
                          "lt_US.rds"))  %>%
        filter(race_eth %in% c("black", "white", "hispanic", "total")) |>
        left_join(theta.dth,
                  by = c("race_eth", "sex", "age"))
    
    # Fertility data (new input for male fertility)
    fx.US <- readRDS(here("outputs",
                          "data_public",
                          "fx_US.rds"))
    
    # Outputs from kin projection
    a_x_t <- readRDS(here("outputs",
                          "data_public",
                          "a_x_t_scenario100.rds"))
    
    
    ## KEY DIMENSIONS OF CODE ------------------------------------------------------
    
    # Ages
    ages <- unique(lt.US$age)
    omega <- length(ages)
    
    # Sexes
    sexes <- unique(lt.US$sex)
    n.sexes <- length(sexes)
    
    # Years
    years <- unique(lt.US$year)
    n.years <- length(years)
    
    # Years with CoD information
    years.cod <- 1999:2020
    n.years.cod <- length(years.cod)
    
    # Races/ethnicities considered: only look at most populous
    races <- c("black", "white", "hispanic", "total")
    n.races <- length(races)
    
    # CoD considered in sensitivity
    cod <- c("other", "drug", "firearm")
    n.cod <- length(cod)
    
    # Nber dimensions output from projection
    n.dim <- length(dim(a_x_t))
    # Monte-Carlo simulations
    n.sim <- dim(a_x_t)[n.dim]
    
    
    ## ----- PREVALENCE ------------------------------------------------------------
    
    a_child_t <- apply(a_x_t, 2:7, sum)
    
    surv_x <- array(
        NA,
        dim = c(18,
                n.sexes,
                n.years.cod,
                n.races,
                n.sim),
        dimnames = list(
            "age" = 0:17,
            "sex" = c("female", "male"),
            "year" = 1999:2020,
            "race" = races,
            "sim" = 1:n.sim
        )
    )
    
    for (i in 1:n.sim) {
        lt.US.i <-
            lt.US |>
            mutate(# Generate deaths at each MC iteration
                D.sim = map2_int(n_deaths, theta, ~ suppressWarnings(MASS::rnegbin(1, .x, .y))))
        for (s in c("female", "male")) {
            for (r in races) {
                for (y in 1:n.years.cod) {
                    cat(s, " ", r, " ", y, "-", i, "\n")
                    
                    # Population counts
                    N_t <- lt.US |>
                        filter(sex == s,
                               year == 1998 + y,
                               race_eth == r) |>
                        pull(pop)
                    
                    # Death counts by CoD
                    D_t <- lt.US.i |>
                        filter(sex == s,
                               year == 1998 + y,
                               race_eth == r) |>
                        pull(D.sim)
                    
                    # Av. nber of child aged a per women age x (summing over sex of child)
                    # at the population level (MC simulations are all the same at the pop level,
                    # hence the indice of the last dimension does not matter)
                    child_per_ind <-
                        a_x_t[, , s, y, r, "all", i]
                    
                    num <- child_per_ind %*% D_t
                    denom <- child_per_ind %*% N_t
                    
                    # Merge female and male child
                    num <- num[1:18] + num[19:36]
                    denom <- denom[1:18] + denom[19:36]
                    
                    # Prob of parental survival
                    surv_x[, s, y, r, i] <-
                        1 - (num / denom)
                    
                }
            }
        }
    }
    
    df.prev <-
        as.data.frame.table(surv_x) |>
        rename("p" = Freq) |>
        mutate(across(c(age, year), ~ as.character(.x) |> as.numeric()),
               cohort = year - age) |>
        filter(# CoD data available from 1999
            cohort >= 1999) |>
        arrange(# Follow a cohort as it ages
            sim, sex, race, cohort, age) |>
        mutate(# Product of probs
            # of mother/father survival as the cohort ages
            .by = c(sim, sex, race, cohort),
            cum_surv = cumprod(p)) |>
        dplyr::select(!p) |>
        pivot_wider(names_from = sex,
                    values_from = cum_surv) |>
        mutate(# Losing at least one parent (1 - survival of both)
            p = 1 - (female * male)) |>
        # Add pop distribution to weight prevalences
        left_join(
            lt.US |>
                dplyr::select(year, race = race_eth, sex, age, pop) |>
                filter(age < 18,
                       year >= 2016) |>
                summarise(# sum over sexes
                    .by = c(year, race, age),
                    
                    pop = sum(pop)) |>
                ungroup() |>
                mutate(.by = c(year, race),
                       
                       pop_d = pop / sum(pop)),
            by = c("year", "race", "age")
        ) |>
        filter(# CoD data available from 1999
            year >= 2016) |>
        mutate(# Prevalence weighted by pop dist.
            prev = p * pop_d) |>
        summarise(# Sum prevalence over ages < 18 yo
            .by = c(sim, race, year),
            prev = sum(prev)) |>
        group_by(race, year) |>
        group_modify(get_quantiles,
                     var = "prev")
    
    saveRDS(
        df.prev,
        here(
            "outputs",
            "data_public",
            "df_prevalence_parental_loss.rds"
        ),
        compress = "xz"
    )
}
