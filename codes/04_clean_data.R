##==============================================================================
##
##  Project title: Parental death from drugs/firearms
##
##  This code loads, cleans up, and tidy the data used as inputs in the 
##  Kinship matrix model.
##
## 
##  Author: Benjamin Schlüter
##  Date: November 2023
##==============================================================================
##
##  Notes
## ------
## 1. In Life Tables, same n_a_x for all racial/ethnic groups.
## 2. Intrapolate fx with Hermitte cubic.
## 3. Compute fx for male by shifting female fx to the right by the 
##    difference between the mean age at childbearing.
##      - Same shift for all racial/ethnic groups.
## 
##==============================================================================

## LOAD PACKAGES ===============================================================

# Install/load packages
packages <- c("tidyverse", "ggplot2", "here", "openxlsx", 
              "pdftools", "Matrix", "pracma","rvest", "stringr", "renv",
              "sessioninfo")
for(p in packages){
        library(p,character.only = TRUE)
}





## FUNCTIONS ===================================================================

source(
        here(
                "codes", 
                "functions",
                "fct_intrapol_fx.R"
                )
        )




## LOAD DATA ===================================================================

##----- MORTALITY ----- 

# Mortality data from CDC, generated with code 00_get_nchs_data.R
df.dth <- readRDS(
        here(
                "inputs", 
                "data_public", 
                "national_year_age-sex-race_drug-opioid-mortality.RDS"
                )
        ) %>% 
        rename(
                "age" = age_years
                ) 

# Add "total" as an additional racial/ethnic group
df.dth.total <- 
        df.dth %>% 
        group_by(
                year, sex, age
                ) %>% 
        summarise_at(
                vars(pop:n_firearm), sum
                ) %>% 
        ungroup() %>% 
        mutate(
                race_eth = "total"
                )

df.dth <- rbind(df.dth,
                df.dth.total)


##----- FERTILITY -----

# Female fertility data from CDC
# https://www.cdc.gov/nchs/hus/data-finder.htm?year=2020-2021&table=Table%20Brth
# Downloaded on the 24/05/2023

# Table has a difficult format to import in R
# Careful: last age interval is 10 year wide (45-54)

# Excel cols of interest
columns.xlsx <- c(1, 4, 5, 8:13)
age.gp.xlsx <- seq(10, 45, 5)
columns.names <- c("year", as.character(age.gp.xlsx))
# Rows for each ethnic group from 1990 to 2019 (if available)
rows.total.xlsx <- 21:50 # All races
rows.nhw.xlsx <- 109:142 # White, not Hispanic or Latina
rows.nhb.xlsx <- 190:223 # Black or African American, not Hispanic or Latina
rows.aian.xlsx <- 262:285 # American Indian or Alaska Native
rows.api.xlsx <- 320:339 # Asian or Pacific Islander, not Hispanic or Latina
rows.h.xlsx <- 352:381 # Hispanic or Latina
# Same order as appearance in Excel file 
# and same labels as mortality data
ethnic.gp <- c("total", "white", "black", "aian", "api", "hispanic")
years.xlsx <- 1990:2019
n.years.xlsx <- length(years.xlsx)
# Load data from Excel files
fx <- read.xlsx(
        here(
                "inputs", 
                "data_public", 
                "brth1980_2019.xlsx"
        ),
        rows = c(
                rows.total.xlsx,
                rows.nhw.xlsx,
                rows.nhb.xlsx,
                rows.aian.xlsx,
                rows.api.xlsx,
                rows.h.xlsx
        ),
        cols = columns.xlsx,
        colNames = F
        ) %>% 
        ## Rename cols
        rename_at(paste0("X", 1:9), ~ columns.names) %>% 
        # Remove year concerning "single race"
        filter(!grepl("(single race)", year)) %>% 
        mutate(
                ## "*": rate based on <20 births not shown-> set to 0.
                across(all_of(columns.names), ~ sub("*", 0, .x, fixed = T)),
                ## Add ethnic group variable according to nber of years
                race_eth = rep(ethnic.gp, c(rep(n.years.xlsx, 3), 
                                            rep(n.years.xlsx-10, 2), 
                                            n.years.xlsx))
        ) %>% 
        ## Horizontal to vertical data frame
        pivot_longer(
                as.character(age.gp.xlsx), 
                values_to = "fx", names_to = "age"
        ) %>% 
        ## Define some variables as numeric
        mutate(
                across(c(year, age, fx), ~ as.numeric(.))
        ) 

# Fertility data from CDC stops in the year 2019
# Get fertility rates for 2020 from latest NVSS report (Jan 31 2023)
# Source: https://www.cdc.gov/nchs/data/nvsr/nvsr72/nvsr72-01.pdf
# Consulted on the 24/05/2023

# Age groups
age.fx <- unique(fx$age)
n.age.fx <- length(age.fx)

# Import pdf
pdf_doc <- pdf_text(
        here(
                "inputs",
                "data_public",
                "nvsr72-01.pdf"
        )
)
# Pages of Table 2
page_num <- 13:14
# Cols of table with fx values
cols_table <- c(3:4,7:12)
# Vector items corresponding to racial/ethnic groups
# for the year 2020
id_race <- c(16, 30, 37, 44, 16)
# Container
fx.latest <- tibble()
# Extract and tidy data looping on racial/ethnic groups
for (r in ethnic.gp[ethnic.gp != "api"]) { # Api not available
        if (r == "hispanic") {
                p <- 2
        } else {
                p <- 1
        }
        # Exctract table
        table_pdf <- pdf_doc[page_num[p]]
        # Create vector with elements before \n
        table_pdf <- strsplit(table_pdf, "\n")[[1]]
        # Extract fx for year 2020
        fx_2020 <- table_pdf[id_race[which(ethnic.gp[ethnic.gp != "api"] == r)]]
        # Cleaning
        fx_2020 <- strsplit(fx_2020, " ")[[1]]
        fx_2020 <- fx_2020[!fx_2020 %in% c("", ".")]
        fx_2020_out <- tibble(
                year = as.integer(fx_2020[1]),
                age = age.fx,
                fx = as.numeric(fx_2020[cols_table]),
                race_eth = r
        )
        fx.latest <- rbind(
                fx.latest, 
                fx_2020_out
        )
}
# Bind with CDC data
fx <- rbind(fx,
            fx.latest) %>% 
        # Fx expressed per 1000 women
        mutate(fx = fx/1000) 

# Male fx data at population level from HFC 
# https://www.fertilitydata.org/Country/Country?code=USA
fx.male <- read.table(
    here(
        "inputs",
        "data_public",
        "m_ASFR_USA.txt"
    ),
    header = TRUE
) |> 
    rename_with(
        tolower
    ) |> 
    filter(
        year >= 1990
    ) |> 
    mutate(
        sex = "male",
        race_eth = "total"
    )






## CODE'S KEY DIMENSIONS =======================================================

# Analysis focus on years 2000:2020
years <- unique(df.dth$year)
n.years <- length(years)

# Ages considered
ages <- unique(df.dth$age)
n.ages <- length(ages)

# Races considered
races <- unique(df.dth$race_eth)
n.races <- length(races)

# Radix for life tables
radix <- 100000





## LIFE TABLE COMPUTATION ======================================================

# From a mortality perspective,
# quantities needed for parent projection are px all causes
# and qx by causes as they allow to create the U and M matrices.

lt.US <- 
        df.dth %>%
        # Assume same ax for all races/ethnicities
        group_by(
                year, race_eth, sex
                ) %>% 
        mutate(
                mx = n_deaths/pop,
                ax = case_when(age == 0 ~ 0.07 + 1.7*mx[1],
                               age == max(ages) ~ Inf,
                               TRUE ~ 0.5),
                n = case_when(age == max(ages) ~ Inf,
                              TRUE ~ 1),
                qx = (n * mx)/(1 + (n - ax) * mx),
                qx = ifelse(age == max(ages), 1, qx),
                px = 1 - qx,
                lx = cumprod(c(1, head(px, -1)))*radix,
                dx = lx - lead(lx, 1),
                dx = ifelse(age == max(ages), lx, dx),
                Lx = lead(lx)*n + ax*dx,
                # Last age gp is NA due to lead()
                Lx = ifelse(age == max(ages), dx/mx, Lx),
                ex = rev(cumsum(rev(Lx)))/lx
               ) %>%
        ungroup()





## FERTILITY RATES COMPUTATION =================================================

# Interpolation of female fx by strata (race*year)
fx.female <- 
        fx %>% 
        # Mid age class
        mutate(
                age.m = case_when(age == 45 ~ age + 5, # Last age group=45-54 yo
                                  TRUE ~ age + 2.5)
                ) %>% 
        arrange(
                race_eth, year, age
                ) %>% 
        group_by(
                race_eth, year
                ) %>% 
        # Own defined function to intrapolate fx
        group_modify(
                ~ intra_fx(.x, sex = "female")
                ) |> 
    ungroup() |> 
    filter(
        # Only most populous race/ethnic groups
        race_eth %in% c("black", "hispanic", "white", "total")
    ) |> 
    dplyr::select(
        race_eth , year, age, asfr = fx, sex
    )

## TFR MALE VS FEMALE

# Compute tfr
tfr <- bind_rows(
    fx.female |> 
        filter(
            race_eth == "total"
        ) |> 
        summarise(
            .by = c(year),
            
            tfr = sum(asfr)
        ) |> 
        mutate(
            sex = "female"
        ),
    fx.male |> 
        ungroup() |> 
        filter(
            race_eth == "total"
        ) |> 
        summarise(
            .by = c(year),
            
            tfr = sum(asfr)
        ) |> 
        mutate(
            sex = "male"
        )
) 

# Compute ratio in tfr
tfr.ratio <- 
    tfr |> 
    pivot_wider(
        names_from = sex,
        values_from = tfr
    ) |> 
    mutate(
        ratio = male / female
    ) 
# In line with Schoumaker (2019), the ratio btw male to female TFR is often < 1.


# Linear model of tfr ratio on 2010-2015
ratio.lm <- 
    lm(ratio ~ 1 + year,
       data = tfr.ratio |> 
           filter(
               year >= 2010,
               !is.na(ratio)
           )
    )
# Get predicted tfr ratio for 2010-2015
ratio.tfr.2016_20 <-
    tibble(
        year = 2016:2020,
        ratio = predict(ratio.lm, tibble(year = 2016:2020))
    )

# Combine data with prediction
tfr.ratio.1990_2020 <- 
    bind_rows(
        tfr.ratio |> 
            filter(
                year < 2016
            ) |> 
            dplyr::select(
                year, ratio
            ),
        ratio.tfr.2016_20
    )

## FORECAST OF MALE FX

# Extract tfr female 2016-2020 to rescale forecast of normalized male fx
tfr.f.2016_20 <- 
    fx.female |> 
    filter(
        race_eth == "total",
        year > 2015
    ) |> 
    summarise(
        .by = c(year),
        
        tfr = sum(asfr)
    ) |> 
    # Add linearly forecasted tfr ratio 2016-2020
    mutate(
        ratio.tfr = ratio.tfr.2016_20$ratio
    )

# Prediction data to be used in code below
df.y.pred <- tibble(
    year = 2016:2020
)

fx.male.frcst <- 
    fx.male |> 
    filter(
        year >= 2010
    ) |> 
    # Forecast fx using 2010-2015 period, on each age
    group_by(
        age
    ) |> 
    nest() |> 
    mutate(
        # Model on log scale to avoid negative value
        model = map(data, function(df) lm(log(asfr) ~ 1 + year, data = df)),
        # Get prediction
        pred = map(model, function(.lm) predict(.lm, df.y.pred))
    ) |> 
    dplyr::select(
        -c(model, data)
    ) |> 
    unnest(
        cols = c(pred)
    ) |> 
    mutate(
        asfr = exp(pred),
        year = df.y.pred$year,
        sex = "male",
        race_eth = "total"
    ) |> 
    ungroup() |> 
    # Normalise forcasted male fx
    mutate(
        .by = (year),
        
        asfr.std = asfr / sum(asfr)
    ) |> 
    # Add female TFR and 2015 tfr ratio to scale forecast for male
    left_join(
        tfr.f.2016_20,
        by = c("year")
    ) |> 
    mutate(
        # Rescale normalized male fx
        asfr = asfr.std * tfr * ratio.tfr
    )

# Combine data with forecast
fx.male.1990_2020 <- 
    bind_rows(
        fx.male.frcst,
        fx.male
    )

## ESTIMATE RELATIONAL MODEL ON FEMALE

# Linear model on ratio between female fx race and female fx population
df.ratio.modeling <-
    fx.female |> 
    filter(
        race_eth != "total"
    ) |> 
    dplyr::select(
        -sex
    ) |> 
    rename(
        "asfr.race" = asfr
    ) |> 
    # Add female fx at population level
    left_join(
        fx.female |> 
            filter(
                race_eth == "total"
            ) |> 
            dplyr::select(
                -c(race_eth, sex)
            ),
        by = c("year", "age")
    ) |> 
    mutate(
        .by = c(race_eth, year),
        
        asfr.race.std = asfr.race / sum(asfr.race),
        asfr.std = asfr / sum(asfr)
    ) |> 
    ungroup() |> 
    mutate(
        ratio = asfr.race.std / asfr.std
    ) |> 
    filter(
        # Cases where 0/0 or 1/0
        !is.na(ratio),
        !is.infinite(ratio)
    ) |> 
    group_by(
        race_eth, age
    ) |> 
    nest() |> 
    mutate(
        # Linear model of the ratio between fx race and fx population
        model = map(data, function(df) lm(ratio ~ 1 + year, data = df))
    ) 

# Extract SE for model uncertainty propagation of race-specific male fx 
df.ratio.se <-
    df.ratio.modeling |> 
    mutate(
        pred = map2(model, data, function(.lm, .data) predict(.lm, .data)),
        se = map2(model, data, function(.lm, .data) predict(.lm, .data, se.fit = TRUE)$se.fit)
    ) |> 
    dplyr::select(
        -model
    ) |> 
    unnest(
        cols = c(data, pred, se)
    ) 

# Extract betas from model
df.ratio.pars <-
    df.ratio.modeling |>  
    mutate(
        lm_tidy = map(model, broom::tidy)
    ) %>%
    ungroup() %>%
    dplyr::select(
        -c(data, model)
    ) |> 
    unnest(cols = c(lm_tidy)
    )

## APPLY ESTIMATES ON MALE FX

years <- 1990:2020
n.years <- length(years)
ages <- unique(fx.male$age)


# Container
df.fx.male.fit <- tibble()

for (r in c("black", "hispanic", "white")) {
    
    for (x in ages) {
        
        # No ratio model above 55 yo since last age available for women
        # and 55 yo is characterize by high noise. 
        # Use 54 yo estimates for all ages >= 55
        if (x < 55) {
            beta <- df.ratio.pars |> 
                filter(
                    race_eth == r,
                    age == x
                ) |> 
                pull(estimate)
            
            # Get se for model uncertainty propagation
            prediction.se <- 
                df.ratio.se |> 
                filter(
                    race_eth == r, 
                    age == x
                    ) |> 
                arrange(year) |> 
                pull(se)
            
        } else {
            beta <- df.ratio.pars |> 
                filter(
                    race_eth == r,
                    age == 54
                ) |> 
                pull(estimate)
            
            prediction.se <- 
                df.ratio.se |> 
                filter(
                    race_eth == r, 
                    age == 54
                ) |> 
                arrange(year) |> 
                pull(se)
        }
        
        # Design matrix
        X <- matrix(cbind(
            rep(1, n.years), # intercept
            years # slope
        ),
        nrow = n.years,
        ncol = 2
        )
        # Apply female ratio estimates to male
        ratio <- X %*% beta
        
        # Store as df
        df.out <-
            tibble(
                race_eth = rep(r, n.years),
                age = rep(x, n.years),
                year = years,
                fit.r = as.numeric(ratio),
                se = prediction.se
            )
        
        df.fx.male.fit <- rbind(
            df.fx.male.fit,
            df.out
        )
    }
}


# Add known asfr from male at population level
df.fx.male.fit <- 
    df.fx.male.fit |> 
    left_join(
        fx.male.1990_2020 |> 
            dplyr::select(
                year, age, asfr.total = asfr, sex
            ),
        by = c("year", "age")
    ) |> 
    mutate(
        .by = c(race_eth, year),
        
        # standardize fx male population level
        asfr.std = asfr.total / sum(asfr.total)
    ) |> 
    ungroup() |>
    # Add race-specific TFR from female
    left_join(
        fx.female |> 
            filter(
                race_eth != "total"
            ) |> 
            summarise(
                .by = c(race_eth, year),
                
                tfr.female = sum(asfr)
            ),
        by = c("race_eth", "year")
    ) |> 
    # Add ratio between male and female tfr at pop level
    left_join(
        tfr.ratio.1990_2020,
        by = c("year")
    ) |> 
    mutate(
        # Compute fx for male of each race/ethnicity
        asfr.male.fit = fit.r * asfr.std * tfr.female * ratio,
        # 95% CI bounds for uncertainty propagation in MC
        asfr.male.fit.l95 = (fit.r - 1.96*se) * asfr.std * tfr.female * ratio,
        asfr.male.fit.u95 = (fit.r + 1.96*se) * asfr.std * tfr.female * ratio
    )

# Bind female and male fx
df.fx <-
    bind_rows(
        # Female section
        fx.female |> 
            rename(
                "fx" = asfr
            ),
        # Male section
        bind_rows(
            df.fx.male.fit |> 
                dplyr::select(
                    race_eth, sex, age, year, fx = asfr.male.fit, 
                    fx.l95 = asfr.male.fit.l95, fx.u95 = asfr.male.fit.u95
                ),
            # Add population level male fx
            fx.male.1990_2020 |> 
                dplyr::select(
                    race_eth, sex, age, year, fx = asfr
                )
        )
    ) 





## ----- OVERDISPERSION --------------------------------------------------------

# We will generate deaths and births from a NBin in the MC. 
# This distribution has variance sigma^2 = mu + mu^2 /theta. 
# We compute theta manually to impute its values in the MC.

# Mortality
theta.dth <- 
    lt.US |> 
    filter(year >= 1999,
           race_eth %in% c("total", "black", "white", "hispanic")) |> 
    dplyr::select(
        year, race_eth, sex, age, n_deaths
    ) |> 
    summarise(
        .by = c(race_eth, sex, age),
        
        mu = mean(n_deaths),
        var = var(n_deaths)
    ) |> 
    ungroup() |> 
    mutate(
        theta = (mu^2) / (var - mu),
        theta = ifelse(theta < 0, 1e6, theta) # negative values: underdispersion. Do not account for it, assume Poisson.
    )

theta.dth |> 
    filter(theta != 1e6) |> 
    ggplot(aes(x = age,
               y = theta,
               col = sex)) +
    facet_wrap( ~ race_eth) +
    geom_point() 



theta.bth <- 
    df.fx |> 
    # Add population counts
    left_join(
        lt.US |> 
            dplyr::select(
                year, race_eth, sex, age, pop
            ),
        by = c("year", "race_eth", "sex", "age")
    ) |>  
    mutate(
        birth = fx * pop
    ) |> 
    summarise(
        .by = c(race_eth, sex, age),
        
        mu = mean(birth),
        var = var(birth)
    ) |> 
    ungroup() |> 
    mutate(
        theta = (mu^2) / (var - mu),
        theta = case_when(
            is.na(theta) ~ 1e6, # no reproduction at these ages, theta values does not matter
            theta < 0 ~ 1e6, # negative values: underdispersion. Do not account for it, assume Poisson.
            TRUE ~ theta
        )
    )

theta.bth |> 
    filter(theta != 1e6) |> 
    ggplot(aes(x = age,
               y = theta,
               col = sex)) +
    facet_wrap(~ race_eth) +
    geom_point()

### Scrape NSCH data if we don't already have it ----
if (!file_exists(here("outputs",
                      "data_public",
                      "df_nsch.rds"))) {
    ## NSCH % about "Parent or guardian died"
    # Scrape html table
    # Url link
    urls_nat <- list(
        "2016" = "https://nschdata.org/browse/survey/results?q=4786&r=1",
        "2017" = "https://nschdata.org/browse/survey/results?q=6763&r=1",
        "2018" = "https://nschdata.org/browse/survey/results?q=7445&r=1",
        "2019" = "https://nschdata.org/browse/survey/results?q=8334&r=1",
        "2020" = "https://nschdata.org/browse/survey/results?q=9135&r=1"
    )
    urls_race <- list(
        "2016" = "https://nschdata.org/browse/survey/results?q=4786&r=1&g=606",
        "2017" = "https://nschdata.org/browse/survey/results?q=6763&r=1&g=675",
        "2018" = "https://nschdata.org/browse/survey/results?q=7445&r=1&g=757",
        "2019" = "https://nschdata.org/browse/survey/results?q=8334&r=1&g=832",
        "2020" = "https://nschdata.org/browse/survey/results?q=9135&r=1&g=935"
        
    )
    # Xpath associated to url link
    xpath_table <-
        '//*[@id="PageContent_PageContent_C001_tblResults"]'
    
    df.nsch <- tibble()
    for (y in as.character(2016:2020)) {
        # National level
        scraped_table <- urls_nat[[y]] |>
            read_html() |>
            html_nodes(xpath = xpath_table) |>
            html_table()
        scraped_table <- scraped_table[[1]]
        p_nat <- scraped_table[1, 2]
        lower95_nat <- substr(scraped_table[2, 2], 1, 3)
        upper95_nat <- substr(scraped_table[2, 2], 7, 9)
        
        # Race/ethnic level
        scraped_table_race <- urls_race[[y]] |>
            read_html() |>
            html_nodes(xpath = xpath_table) |>
            html_table()
        scraped_table_race <- scraped_table_race[[1]]
        names(scraped_table_race)[1:3] <-
            c("race", "metric", "Qtrue")
        p_race <-
            scraped_table_race |>
            filter(race %in% c(
                "Hispanic",
                paste0("White, ", c("n", "N"), "on-Hispanic"),
                paste0("Black, ", c("n", "N"), "on-Hispanic")
            ),
            metric == "%") |>
            pull(Qtrue)
        CI <-
            scraped_table_race |>
            filter(race %in% c(
                "Hispanic",
                paste0("White, ", c("n", "N"), "on-Hispanic"),
                paste0("Black, ", c("n", "N"), "on-Hispanic")
            ),
            metric == "C.I.") |>
            pull(Qtrue)
        lower95_race <- substr(CI, 1, 3)
        upper95_race <- substr(CI, 7, 9)
        
        
        df.out <- tibble(
            year = as.numeric(y),
            median = c(p_nat, p_race),
            # not sure it is the median but makes compatible with data of estimates
            lower95 = c(lower95_nat, lower95_race),
            upper95 = c(upper95_nat, upper95_race),
            race = c("Total", "Hispanic", "NH White", "NH Black"),
            source = "NSCH"
        ) |>
            mutate(across(median:upper95, ~ as.numeric(.x)))
        df.nsch <- rbind(df.nsch,
                         df.out)
        
    }
    
    # Store data
    saveRDS(df.nsch,
            here("outputs",
                 "data_public",
                 "df_nsch.rds"))
}



## SAVE TIDY DATA SETS =========================================================

# Life table
saveRDS(
        lt.US,
        here(
                "outputs", 
                "data_public", 
                "lt_US.rds"
                ),
        compress = "xz"
        )
# Fertility rates
saveRDS(
    df.fx,
    here(
        "outputs",
        "data_public",
        "fx_US.rds"
    ),
    compress = "xz"
) 

# Overdispersion deaths
saveRDS(
    theta.dth,
    here(
        "outputs",
        "data_public",
        "overdisp_dth.rds"
    ),
    compress = "xz"
)

# Overdispersion births
saveRDS(
    theta.bth,
    here(
        "outputs",
        "data_public",
        "overdisp_bth.rds"
    ),
    compress = "xz"
)
