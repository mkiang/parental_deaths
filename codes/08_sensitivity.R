
##==============================================================================
##
##  Project title: Parental loss from drugs/firearms
##
##  Sensitivity analysis: impact of fx level of individuals dying from 
##  drugs or firearms.
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
# NOTE: MASS will mask `select()` from dplyr so we should load MASS first. 
packages <- c("MASS", "tidyverse", "here")
for(p in packages){
    # if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

source(here("codes", "utils.R"))



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
        "df_cum_orphans_at_least.rds",
        "df_cum_orphans_total_at_least_combined.rds"
    )
)))) {
    
    # Mortality data
    lt.US <- readRDS(
        here("outputs",
             "data_public",
             "lt_US.rds")
    ) %>% 
        filter(
            # No CoD data before
            year >= 1999,
            # Populous races/ethnicities
            race_eth %in% c("black", "white", "hispanic", "total")
        ) |> 
        mutate(
            # Death counts from "other"
            n_other = n_deaths - (n_drug + n_firearm)
        )
    
    # Overdispersion parameters for NBin in MC
    theta.dth <- readRDS(
        here(
            "outputs",
            "data_public",
            "overdisp_dth.rds")
    ) |> 
        dplyr::select(
            -c(mu, var)
        )
    
    # Create lt.US in long format to generate deaths by cause and 
    # add overdispersion for MC
    lt.US.long <-
        lt.US |> 
        dplyr::select(
            year, race_eth, sex, age, n_other, n_drug, n_firearm
        ) |> 
        pivot_longer(
            n_other:n_firearm,
            names_to = "cause",
            values_to = "dth"
        ) |> 
        mutate(
            cause = sub(".*_", "", cause)
        ) |> 
        left_join(
            theta.dth,
            by = c("race_eth", "sex", "age")
        )
    
    # Fertility data (new input for male fertility)
    fx.US <- readRDS(here("outputs",
                          "data_public",
                          "fx_US.rds")
    ) 
    
    # Outputs from kin projection
    a_x_t <- readRDS_xz(here("outputs",
                             "data_public",
                             "a_x_t_scenario100.rds"),
                        threads = 8)
    
    
    
    
    
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
    sc.fx <- seq(0.75, 1.25, 0.05)
    n.sc.fx <- length(sc.fx)
    # Fertility level of the whole population
    base.sc <- which(sc.fx == 1)
    
    # Nber dimensions output from projection
    n.dim <- length(dim(a_x_t))
    # Monte-Carlo simulations
    n.sim <- dim(a_x_t)[n.dim]
    
    
    
    
    
    ## ----- ORPHANS & INCIDENCE OF ORPHANHOOD -------------------------------------
    # Average number of alive child (irrespective of sex) according to age of parent
    a_child_t.base <- apply(a_x_t, MARGIN = 2:7, FUN = sum)
    rm(a_x_t)
    gc()
    
    # Compute the number of maternal/paternal orphans and incidence 
    # every year by race and parent sex
    
    # Container for the nber of maternal/paternal orphans by year, race/ethnicity, sex 
    # of their parent and CoD
    n_orphans <- array(NA,
                       dim = c(n.sexes,
                               n.years.cod, 
                               n.races,
                               n.cod,
                               n.sc.fx,
                               n.sim),
                       dimnames = list("sex" = sexes,
                                       "year" = years.cod,
                                       "race" = races,
                                       "cause" = cod,
                                       "scenario.fx" = sc.fx,
                                       "sim" = 1:n.sim))
    # Container for the nber of living children by year, race/ethnicity, sex 
    # of their parent and CoD
    n_children <- array(NA,
                        dim = c(n.sexes,
                                n.years.cod, 
                                n.races,
                                # this object does not vary on these dimensions.
                                # It is added to ease the incidence computation.
                                n.cod,
                                n.sc.fx,
                                n.sim
                        ),
                        dimnames = list("sex" = sexes,
                                        "year" = years.cod,
                                        "race" = races,
                                        "cause" = cod,
                                        "scenario.fx" = sc.fx,
                                        "sim" = 1:n.sim))
    # Loop on fertility scenarii
    for (j in 1:n.sc.fx) {
        
        # Avoid loading a_x_t base twice
        if (j != base.sc) {
            # Load a_x_t corresponding to different fx scenarii
            a_x_t <- readRDS_xz(
                here(
                    "outputs",
                    "data_public",
                    sprintf("a_x_t_scenario%03d.rds", round(sc.fx[j] * 100))
                )
            )
            # Average number of alive child (irrespective of sex) according to age of parent
            a_child_t <- apply(a_x_t, 2:7, sum)
            
        } else {
            a_child_t <- a_child_t.base
        }
        ## Cleaning up a_x_t here saves memory and speeds up code when hitting
        ## the limit
        rm(a_x_t)
        gc()
        

        
        # Loop on MC iterations
        for (i in 1:n.sim) {
            # Simulate cause-specific death counts
            lt.US.i <- lt.US.long %>% 
                mutate(
                    # Generate deaths at each MC iteration
                    D.sim = map2_int(dth, theta, ~ suppressWarnings(rnegbin(1, .x, .y)))
                )
            
            for (s in c("female", "male")) {
                for (r in races) {
                    for (y in 1:n.years.cod) {
                        
                        if (i %% 250 == 0 & s == "female" & r == "total" & y == 1) {
                            ## Reduce the amount of output
                            cat(j, " ", r, " ", s, " - ", i, " ", as.character(Sys.time()), "\n")
                        }
                        
                        # Av. nber of child per individual age x for whole pop
                        child_per_ind <- a_child_t.base[, s, y, r, "all", i]
                        
                        # Population counts
                        N_t <- lt.US |> 
                            filter(
                                sex == s,
                                year == 1998 + y,
                                race_eth == r
                            ) |> 
                            pull(pop)
                        
                        for (cod in c("other", "drug", "firearm")) {
                            
                            # Av. nber of child per individual age x if parent die from cause i,
                            # accounting for different fx scenario
                            child_per_ind_cod <- a_child_t[, s, y, r, cod, i]
                            
                            # Death counts by CoD
                            D_t <- lt.US.i |> 
                                filter(
                                    sex == s,
                                    year == 1998 + y,
                                    race_eth == r,
                                    cause == cod
                                ) |> 
                                pull(D.sim)
                            
                            n_orphans[s, y, r, cod, j, i] <- child_per_ind_cod %*% D_t
                            n_children[s, y, r, cod, j, i] <- child_per_ind %*% N_t
                            
                        }
                    }
                }
            }
        }
    }
    
    # Orphanhood incidence
    incidence <- n_orphans / n_children
    
    # Cumlative number of orphans by drugs and firearms
    df.cum.orphans.at.least <- 
        as.data.frame.table(incidence) |> 
        rename("p" = Freq) |> 
        mutate(
            year = as.character(year) |> as.numeric(),
            scenario.fx = as.character(scenario.fx) |> as.numeric(),
            cause = case_when(
                cause == "drug" ~ "drugs",
                cause == "firearm" ~ "firearms",
                TRUE ~ "other"
            )
        ) |> 
        # Only consider orphans from drugs or firearms
        filter(
            cause != "other"
        ) |> 
        pivot_wider(
            names_from = sex,
            values_from = p
        ) |> 
        mutate(
            # Prob losing at least one parent
            p = 1 - ((1 - female) * (1 - male))
        ) |> 
        left_join(
            lt.US |> 
                rename("race" = race_eth) |> 
                filter(
                    age < 18
                ) |> 
                summarise(
                    .by = c(year, race),
                    
                    # Sum over sex and ages
                    pop = sum(pop)
                ),
            by = c("year", "race")
        ) |> 
        mutate(
            # Number of children that lose at least a parent
            n = p * pop
        ) |> 
        summarise(
            # Sum orphans over the years by cause
            .by = c(race, cause, scenario.fx, sim),
            
            cum_n = sum(n)
        ) |> 
        ungroup() |> 
        # Get quantiles from MC simulations
        group_by(
            race, scenario.fx, cause
        ) |> 
        group_modify(get_quantiles, 
                     var = "cum_n")
    
    # Cumlative number of orphans in whole population for drugs and firearms combined
    cum.orphans.at.least.combined.sc.fx <- 
        as.data.frame.table(incidence) |> 
        rename("p" = Freq) |> 
        mutate(
            year = as.character(year) |> as.numeric(),
            scenario.fx = as.character(scenario.fx) |> as.numeric(),
            cause = case_when(
                cause == "drug" ~ "drugs",
                cause == "firearm" ~ "firearms",
                TRUE ~ "other"
            )
        ) |> 
        # Only consider orphans in whole population from drugs or firearms
        filter(
            cause != "other",
            race == "total"
        ) |> 
        pivot_wider(
            names_from = sex,
            values_from = p
        ) |> 
        mutate(
            # Prob losing at least one parent
            p = 1 - ((1 - female) * (1 - male))
        ) |> 
        left_join(
            lt.US |> 
                rename("race" = race_eth) |> 
                filter(
                    age < 18
                ) |> 
                summarise(
                    .by = c(year, race),
                    
                    # Sum over sex and ages
                    pop = sum(pop)
                ),
            by = c("year", "race")
        ) |> 
        mutate(
            # Number of children that lose at least a parent
            n = p * pop
        ) |> 
        summarise(
            # Sum orphans in whole pop over the years
            .by = c(cause, scenario.fx, sim),
            
            cum_n = sum(n)
        ) |> 
        ungroup() 
    
    # Create df to generate a geom_tile at population level
    # for the cumulative orphans over 1999-2020
    df.grid.fx <-
        expand.grid(
            drug = seq(0.75, 1.25, 0.05),
            firearm = seq(0.75, 1.25, 0.05),
            sim = 1:n.sim
        ) |> 
        mutate(
            cum_n = NA
        )
    
    for (i in 1:dim(df.grid.fx)[1]) {
        
        mc.sim <- df.grid.fx$sim[i]
        fx.drug <- df.grid.fx$drug[i]
        fx.firearm <- df.grid.fx$firearm[i]
        
        orphans_drug <- 
            cum.orphans.at.least.combined.sc.fx |> 
            filter(
                cause == "drugs",
                scenario.fx == fx.drug,
                sim == mc.sim
            ) |> 
            pull(cum_n)
        
        orphans_firearm <- 
            cum.orphans.at.least.combined.sc.fx |> 
            filter(
                cause == "firearms",
                scenario.fx == fx.firearm,
                sim == mc.sim
            ) |> 
            pull(cum_n)
        
        df.grid.fx$cum_n[i] <- orphans_drug  + orphans_firearm
        
    }
    
    df.grid.fx <- 
        df.grid.fx |> 
        # Get quantiles from MC simulations
        group_by(
            drug, firearm
        ) |> 
        group_modify(get_quantiles, 
                     var = "cum_n")
    
    
    ## Store outputs
    saveRDS(
        df.cum.orphans.at.least,
        here(
            "outputs",
            "data_public",
            "df_cum_orphans_at_least.rds"
        ),
        compress = "xz"
    )
    
    saveRDS(
        df.grid.fx,
        here(
            "outputs",
            "data_public",
            "df_cum_orphans_total_at_least_combined.rds"
        ),
        compress = "xz"
    )
    
    
    
    ## ----- VISUALIZATION ---------------------------------------------------------
    
    # MATT, THE FLOOR IS YOURS
    
    df.cum.orphans.at.least |> 
        ggplot(aes(x = scenario.fx, 
                   y = median,
                   ymin = lower95,
                   ymax = upper95)) +
        facet_grid(race ~ cause, scales = "free_y") +
        geom_errorbar() +
        geom_point() +
        theme_bw() 
    
    df.cum.orphans.at.least |> 
        filter(race == "black",
               cause == "drugs") |> 
        ggplot(aes(x = scenario.fx, 
                   y = median,
                   ymin = lower95,
                   ymax = upper95)) +
        facet_grid(race ~ cause) +
        geom_errorbar() +
        geom_point() +
        theme_bw() 
    
    # Don't see how to represent uncertainty in here. I use median
    df.grid.fx |> 
        ggplot(aes(x = drug,
                   y = firearm,
                   fill = median)) +
        geom_tile() +
        theme_bw() +
        labs(
            y = "fx firearm",
            x = "fx drug"
        )
    
    
    
    
}
