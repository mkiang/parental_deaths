
##==============================================================================
##
##  Project title: Parental death from drugs/firearms
##
##  Compute the scaling factor for the population at risk in the MC simulation.
##  The population at risk consists of parents, represented by mothers as data
##  for father is not available.
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

# Function performing loess smoothing and
# constant extrapolation at first/last values
smooth_extd_mother_prop <- function(x, ...) {
        
        # Fit with loess curve
        # log transformation to ensure
        # positive values
        fit <- loess(log(ifelse(p == 0, p+1e-8, p) / (1 - ifelse(p == 0, p+1e-8, p))) ~ age, 
                     data = x, 
                     span = 0.6)
        
        # Predict from model 
        lo.fit <- predict(fit, 
                          data.frame(age = 15:44), 
                          se = FALSE)
        
        # convert back to prob scale
        lo.fit <- 1 / (1 + exp(-lo.fit))
        
        # Extrapolate with first and last values
        # and store in df
        # !! assume parents do not lose children !!
        out <- tibble(
                age = 0:85,
                p = c(rep(NA, 15), x$p, rep(NA, 41)),
                p.fit = c(rep(lo.fit[1], 15), lo.fit, rep(lo.fit[30], 41))
        )
        
        return(out)
        
}