
##==============================================================================
##
##  Project title: Parental death from drugs/firearms
##
##  Function to intrapolate age-specific fertility rates with Hermitte cubic.
##
## 
##  Author: Benjamin Schlüter
##  Date: November 2023
##==============================================================================
##
##  Notes
## ------
## 1. 
## 
##==============================================================================

# Install/load packages
packages <- c("pracma")
for(p in packages){
        if(!require(p,character.only = TRUE)) install.packages(p)
        library(p,character.only = TRUE)
}

# Intra/extrapolation of fx 
# Cubic Hermite interpolation to avoid wiggle. 

intra_fx <- function(df, sex) {
        
        if (sex == "female") { age.bd = c(9, 55) 
        } else { age.bd = c(9, 65) }
        
        fx.int <- interp1(c(age.bd[1], df$age.m, age.bd[2]), 
                          # Set fx=0 at ages 9 and 55 yo.    
                          c(0, df$fx, 0), 
                          age.bd[1]:age.bd[2],
                          method = "cubic")
        
        fx.int <- tibble(age = age.bd[1]:age.bd[2],
                         fx = fx.int) %>% 
                # Make sure interpolation are >0
                mutate(fx = ifelse(fx < 0, 0, fx),
                       sex = sex)
        return(fx.int)
}