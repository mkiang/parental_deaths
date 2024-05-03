
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
packages <- c("tidyverse", "here", "MASS", "doParallel", "future", "future.apply")
for(p in packages){
    # if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}





## ----- FUNCTIONS -------------------------------------------------------------

source(
    here(
        "codes",
        "functions",
        "fct_kin.R"
    )
)

source(here("codes", "utils.R"))



## ----- LOAD DATA -------------------------------------------------------------

# Mortality data
lt.US <- readRDS(
    here("outputs",
         "data_public",
         "lt_US.rds")
) %>% 
    filter(
        race_eth %in% c("black", "white", "hispanic", "total")
    ) |> 
    mutate(
        # Death counts from "other"
        n_other = n_deaths - (n_drug + n_firearm),
        # Cause-specific mortality rates
        mx_other = n_other / pop,
        mx_drug = n_drug / pop,
        mx_firearm = n_firearm /pop
    )

# Overdispersion parameters for NBin in MC
theta.bth <- readRDS(
    here(
        "outputs",
        "data_public",
        "overdisp_bth.rds"
    )
)

# Fertility data (new input for male fertility rates)
fx.US <- readRDS(here("outputs",
                      "data_public",
                      "fx_US.rds")
) 





## ----- KEY DIMENSIONS OF CODE ------------------------------------------------

# Ages 
ages <- unique(lt.US$age)
omega <- length(ages)

# Years 
years <- unique(lt.US$year)
n.years <- length(years)

# Years with CoD information
years.cod <- 1999:2020
n.years.cod <- length(years.cod)

# Races considered: only look at most populous 
races <- unique(lt.US$race_eth)
n.races <- length(races)

# Fertility scenarii - expressed in % of pop fx
## prioritize the main result first
sc.fx <- c(1, seq(.75, 1.25, .05)[-6])
n.sc.fx <- length(sc.fx)

# Number of Monte-Carlo (MC) simulations
n.sim <- 1000
n.cores <- 20




## ----- EXPOSURES FOR MONTE-CARLO SIMULATIONS ---------------------------------

# Create exposure for MC according to number of deaths:
# Scale population counts according to the number of 
# cause-specific deaths
scalar.exp.MC <-
    lt.US |> 
    # No cause-specific death counts before 1999
    filter(
        year >= 1999
    ) |> 
    mutate(
        .by = c(year, race_eth, sex),
        
        # Share of cause-specific deaths in total pop
        other = sum(n_other) / sum(pop),
        drug = sum(n_drug) / sum(pop),
        firearm = sum(n_firearm) / sum(pop),
        all  = sum(pop) / sum(pop) # scalar of one for pop level
    ) |> 
    ungroup() |> 
    dplyr::select(
        year, race_eth, sex, age, other, drug, firearm, all
    )

# Add it in fx by differentiating years </> 1999
fx.US.post.1999 <- 
    fx.US |> 
    filter(
        year >= 1999
    ) |> 
    # Add scaling factors
    left_join(
        scalar.exp.MC,
        by = c("year", "race_eth", "sex", "age")
    )

# Assume scaling factors of 1999 for years < 1999
fx.US.pre.1999 <- 
    fx.US |> 
    filter(
        year < 1999
    ) |> 
    left_join(
        scalar.exp.MC |> 
            filter(
                year == 1999
            ) |> 
            dplyr::select(
                -year
            ),
        by = c("race_eth", "sex", "age")
    ) 

# Merge both 
fx.US <- bind_rows(
    fx.US.pre.1999,
    fx.US.post.1999
) |> 
    pivot_longer(
        other:all,
        names_to = "cause",
        values_to = "scalar.exp"
    ) |> 
    # Add population counts to fx
    left_join(
        lt.US |> 
            dplyr::select(
                year, race_eth, sex, age, 
                pop # exposure for MC
            ),
        by = c("year", "race_eth", "sex", "age")
    ) |> 
    mutate(
        # Get exposures by cause of death by applying the scalars 
        exp = scalar.exp * pop,
        # Mean parameter for NB in MC
        mu_nb = fx * exp,
        # Mean parameter bounds for male fx that have been modeled.
        # If not modeled, the bounds are simply mu_nb
        mu_nb.l95 = ifelse(is.na(fx.l95), mu_nb, fx.l95 * exp), 
        mu_nb.u95 = ifelse(is.na(fx.u95), mu_nb, fx.u95 * exp)
        
    ) |> 
    left_join(
        theta.bth |> 
            dplyr::select(
                -c(mu, var)
            ),
        
        by = c("race_eth", "sex", "age")
    )





## ----- KINSHIP MATRIX MODEL --------------------------------------------------
# Allow to allocate fx according to age of parent
PHI <- list("female" = get_phi(ages, "female"),
            "male" = get_phi(ages, "male")) 


set.seed(28041992)
for (j in 1:n.sc.fx) {
    f_path <- here(
        "outputs",
        "data_public",
        sprintf("a_x_t_scenario%03d.rds", round(sc.fx[j] * 100))
    )
    
    if (file.exists(f_path)) {
        next
    }
    
    ## Run the simulations
    ## Set up parallel backend
    future::plan(future::multisession(workers = n.cores))
    
    holder <- future.apply::future_replicate(n.sim, {
        # Container for the average number of alive children age a
        # over age, year, sex, and race/ethnicity of their parent (=Focal)
        a_x_t <- array(
            NA,
            dim = c(# vector contains age dist. of children alive
                # (female and male: 2*omega)
                2 * omega,
                omega,
                2,
                n.years + 1,
                n.races,
                4,
                1),
            dimnames = list(
                "children" = 1:(2 * omega),
                "focal age" = ages,
                "focal sex" = c("female", "male"),
                "year" = c("stable", years),
                "race" = races,
                "cause" = c("other", "drug", "firearm", "all"),
                # need to consider all pop for incidence
                "scenario.fx" = sc.fx[j]
            )
        )
        
        # Simulates fx depending on the cause-of-death and pop case (all) (varying exposures)
        fx.US <- fx.US %>%
            mutate(
                # Generate mu_fx for modeled race-specific male fx
                mu_nb.sim = map2_dbl(mu_nb.l95, mu_nb.u95, ~ suppressWarnings(runif(1, .x, .y))),
                # Generate births at each MC iteration, considering different fx scenarii
                # by scaling mu_nb
                B.sim = map2_int(mu_nb.sim, theta, ~ suppressWarnings(rnegbin(
                    1, sc.fx[j] * .x, .y
                ))),
                # Compute the generated fx (avoiding 0/0)
                fx.sim = ifelse(exp == 0, 0, B.sim / exp)
            )
        
        # Start loop on cause
        for (cod in c("other", "drug", "firearm", "all")) {
            # Filter the generated fx according to the cause of death
            fx.US.i <- fx.US %>%
                filter(cause == cod) |>
                dplyr::select(# Use fx.sim instead of population fx
                    race_eth, year, age, sex, fx = fx.sim)
            
            # Start loop on sex of parent
            for (sex_parent in c("female", "male")) {
                # Start loop over the races/ethnicities
                for (r in races) {
                    # Stage of MC
                    # cat(r, " ", sex_parent, " ", cod, " - ", i, "\n")
                    
                    # Boundary conditions
                    # No child at birth in every year
                    a_x_t[, 1, sex_parent, , r, cod, 1] <- 0
                    
                    # Get a(x,0) for all x assuming stable pop in
                    # the year 1990 (Focal could be any age in the
                    # year 1990)
                    U_2sex <-
                        get_U_2sex(lt.US, min(years), r)
                    F_2sex <-
                        get_F_2sex(birth_female = 1 / 2.04,
                                   ages,
                                   fx.US.i,
                                   min(years),
                                   r)
                    
                    for (x in 2:omega) {
                        a_x_t[, x, sex_parent, 1, r, cod, 1] <-
                            U_2sex %*% a_x_t[, x - 1, sex_parent, 1, r, cod, 1] +
                            F_2sex %*% PHI[[sex_parent]][, x - 1] # index does not go until the end but no fertility at old ages so doesn't matter
                    }
                    
                    # Start loop over the years
                    for (y in 1:n.years) {
                        # Projection of children
                        U_2sex <-
                            get_U_2sex(lt.US, years[y], r)
                        F_2sex <-
                            get_F_2sex(birth_female = 1 / 2.04,
                                       ages,
                                       fx.US.i,
                                       years[y],
                                       r)
                        
                        a_x_t[, 2:omega, sex_parent, (y + 1), r, cod, 1] <-
                            U_2sex %*% a_x_t[, 1:(omega - 1), sex_parent, y, r, cod, 1] +
                            F_2sex %*% PHI[[sex_parent]][, 1:(omega - 1)]
                    }
                }
            }
        }
        # Do not consider years before 1999 (no death by cause)
        # and focus on children (both sexes) < 18 yo
        a_x_t[c(1:18, 87:104), , , which(years %in% years.cod) + 1, , , 1]
    })
    
    ## Close the parallel backend
    doParallel::stopImplicitCluster()
    closeAllConnections()
    
    ## Parallelized saveRDS 
    ## Single core save takes ~ 1h so prefer parallelized save if possible. 
    ## Note probably only works on modern macs/unix and requires xz version 5+.
    if (.Platform$OS.type == "unix") {
        saveRDS_xz(holder,
                   f_path,
                   threads = n.cores)
    } else {
        # Store object
        saveRDS(holder,
                f_path,
                compress = "xz")
    }
    
    # Clean up
    rm(holder)
    gc()
}
