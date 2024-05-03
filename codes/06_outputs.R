
##==============================================================================
##
##  Project title: Parental loss from drugs/firearms
##
##  Generate outputs from the kin projection.
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







## ----- LOAD PACKAGES ---------------------------------------------------------

# Install/load packages
packages <- c("tidyverse", "here", "MASS")
for(p in packages){
    # if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}





## ----- FUNCTIONS -------------------------------------------------------------

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





## ----- LOAD DATA -------------------------------------------------------------
## Script takes a long time. Only run it if we haven't already generated the
## desired output files. 
if (any(!file.exists(here(
    "outputs",
    "data_public",
    c(
        "df_n_loss_by_cause.rds",
        "df_incidence_parental_loss_by_cause.rds",
        "df_n_loss_by_cause_sex.rds",
         "df_incidence_parental_loss_by_cause_sex.rds"
    )
)))) {
    
    
    # Mortality data
    lt.US <- readRDS(here("outputs",
                          "data_public",
                          "lt_US.rds")) %>%
        filter(# No CoD data before
            year >= 1999,
            # Populous races/ethnicities
            race_eth %in% c("black", "white", "hispanic", "total")) |>
        mutate(# Death counts from "other"
            n_other = n_deaths - (n_drug + n_firearm))
    
    # Overdispersion parameters for NBin in MC
    theta.dth <- readRDS(here("outputs",
                              "data_public",
                              "overdisp_dth.rds")) |>
        dplyr::select(-c(mu, var))
    
    # Create lt.US in long format to generate deaths by cause and
    # add overdispersion for MC
    lt.US.long <-
        lt.US |>
        dplyr::select(year, race_eth, sex, age, n_other, n_drug, n_firearm) |>
        pivot_longer(n_other:n_firearm,
                     names_to = "cause",
                     values_to = "dth") |>
        mutate(cause = sub(".*_", "", cause)) |>
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
    
    
    
    
    
    ## ----- KEY DIMENSIONS OF CODE ------------------------------------------------
    
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
    
    # Fertility scenarii - expressed in % of pop fx
    # sc.fx <- seq(0.75, 1.25, 0.05)
    # n.sc.fx <- length(sc.fx)
    # Fertility level of the whole population
    # base.sc <- which(sc.fx == 1)
    
    # Nber dimensions output from projection
    n.dim <- length(dim(a_x_t))
    # Monte-Carlo simulations
    n.sim <- dim(a_x_t)[n.dim]
    
    
    
    
    
    ## ----- ORPHANS & INCIDENCE OF ORPHANHOOD -------------------------------------
    
    # We do not consider fx scenarii here so only look at the case when fx = 1 * fx
    # a_x_t <- a_x_t[, , , , ,  , ]
    
    # Average number of alive child (irrespective of sex) according to age of parent
    a_child_t <- apply(a_x_t, 2:7, sum)
    
    # Container for the nber of maternal/paternal orphans
    n_orphans <- array(
        NA,
        dim = c(n.sexes,
                n.years.cod,
                n.races,
                n.cod,
                n.sim),
        dimnames = list(
            "sex" = sexes,
            "year" = years.cod,
            "race" = races,
            "cause" = cod,
            "sim" = 1:n.sim
        )
    )
    # Container for the total number of children
    n_children <- array(
        NA,
        dim = c(n.sexes,
                n.years.cod,
                n.races,
                n.cod,
                n.sim),
        dimnames = list(
            "sex" = sexes,
            "year" = years.cod,
            "race" = races,
            "cause" = cod,
            "sim" = 1:n.sim
        )
    )
    
    # Loop on MC iterations
    for (i in 1:n.sim) {
        # Simulate cause-specific death counts
        lt.US.i <- lt.US.long %>%
            mutate(# Generate deaths at each MC iteration
                D.sim = map2_int(dth, theta, ~ suppressWarnings(rnegbin(1, .x, .y))))
        
        for (s in c("female", "male")) {
            for (r in races) {
                for (y in 1:n.years.cod) {
                    cat(r, " ", s, " - ", i, "\n")
                    
                    # Av. nber of child per individual age x for whole pop
                    child_per_ind <- a_child_t[, s, y, r, "all", i]
                    
                    # Population counts
                    N_t <- lt.US |>
                        filter(sex == s,
                               year == 1998 + y,
                               race_eth == r) |>
                        pull(pop)
                    
                    for (cod in c("other", "drug", "firearm")) {
                        # Av. nber of child per individual age x if parent die from cause i
                        child_per_ind_cod <-
                            a_child_t[, s, y, r, cod, i]
                        
                        # Death counts by CoD
                        D_t <- lt.US.i |>
                            filter(sex == s,
                                   year == 1998 + y,
                                   race_eth == r,
                                   cause == cod) |>
                            pull(D.sim)
                        
                        n_orphans[s, y, r, cod, i] <-
                            child_per_ind_cod %*% D_t
                        n_children[s, y, r, cod, i] <-
                            child_per_ind %*% N_t
                        
                    }
                }
            }
        }
    }
    
    # Number of orphans by parent sex and cause
    df.n.orphans <- as.data.frame.table(n_orphans) |>
        rename("n" = Freq) |>
        mutate(
            # Make categories coherent with code for figures
            year = as.character(year) |> as.numeric(),
            sex = ifelse(sex == "female", "mother", "father"),
            cause = case_when(
                cause == "drug" ~ "drugs",
                cause == "firearm" ~ "firearms",
                TRUE ~ "other"
            )
        ) |>
        group_by(sex, race, year, cause) |>
        group_modify(get_quantiles,
                     var = "n")
    
    # Orphanhood incidence by parent sex
    incidence <- n_orphans / n_children
    
    df.inc.orphans <- as.data.frame.table(incidence) |>
        rename("p" = Freq) |>
        mutate(
            # Make categories coherent with code for figures
            year = as.character(year) |> as.numeric(),
            sex = ifelse(sex == "female", "mother", "father"),
            cause = case_when(
                cause == "drug" ~ "drugs",
                cause == "firearm" ~ "firearms",
                TRUE ~ "other"
            )
        ) |>
        group_by(sex, race, year, cause) |>
        group_modify(get_quantiles,
                     var = "p")
    
    # Orphanhood incidence of at least one parent
    df.inc.orphans.at.least <- as.data.frame.table(incidence) |>
        rename("p" = Freq) |>
        mutate(
            year = as.character(year) |> as.numeric(),
            cause = case_when(
                cause == "drug" ~ "drugs",
                cause == "firearm" ~ "firearms",
                TRUE ~ "other"
            )
        ) |>
        pivot_wider(names_from = sex,
                    values_from = p) |>
        mutate(p = 1 - ((1 - female) * (1 - male))) |>
        group_by(race, year, cause) |>
        group_modify(get_quantiles,
                     var = "p")
    
    # Number of orphans (using incidence at least from above)
    df.n.orphans.at.least <-
        as.data.frame.table(incidence) |>
        rename("p" = Freq) |>
        mutate(
            year = as.character(year) |> as.numeric(),
            cause = case_when(
                cause == "drug" ~ "drugs",
                cause == "firearm" ~ "firearms",
                TRUE ~ "other"
            )
        ) |>
        pivot_wider(names_from = sex,
                    values_from = p) |>
        mutate(# Prob losing at least one parent
            p = 1 - ((1 - female) * (1 - male))) |>
        left_join(
            lt.US |>
                rename("race" = race_eth) |>
                filter(age < 18) |>
                summarise(.by = c(year, race),
                          
                          # Sum over sex and ages
                          pop = sum(pop)),
            by = c("year", "race")
        ) |>
        mutate(# Number of children that lose at least a parent
            n = p * pop) |>
        group_by(race, year, cause) |>
        group_modify(get_quantiles,
                     var = "n")
    
    # Store dfs
    saveRDS(
        df.n.orphans.at.least,
        here("outputs",
             "data_public",
             "df_n_loss_by_cause.rds"),
        compress = "xz"
    )
    
    saveRDS(
        df.inc.orphans.at.least,
        here(
            "outputs",
            "data_public",
            "df_incidence_parental_loss_by_cause.rds"
        ),
        compress = "xz"
    )
    
    saveRDS(
        df.inc.orphans,
        here(
            "outputs",
            "data_public",
            "df_incidence_parental_loss_by_cause_sex.rds"
        ),
        compress = "xz"
    )
    
    saveRDS(df.n.orphans,
            here("outputs",
                 "data_public",
                 "df_n_loss_by_cause_sex.rds"))
}
