
##------------- FUNCTIONS FOR MATRIX KINSHIP MODEL -----------------------------
## 
## 
##  Author: Benjamin Schlüter
##  Date: April 2023
##
##
## -----------------------------------------------------------------------------



## Load packages ---------------------------------------------------------------

packages <- c("tidyverse", "Matrix")
invisible( lapply(packages, library, character.only = TRUE))



## Functions -------------------------------------------------------------------

## Construct U matrix from a lifetable
get_U <- function(s, y, r, lifeTable) {
        
        ## Extract survival probs 
        px <- lifeTable %>% 
                filter(sex == s,
                       year == y,
                       race_eth == r) %>% 
                pull(px)
        ## Dim of A
        omega <- length(px)
        ## Creation of U
        U <- matrix(0, 
                    nrow = omega, 
                    ncol = omega)
        ## Survival prob on subdiagonal
        U[row(U)-col(U)==1] <- head(px,-1) # ASSUMPTION: Everybody die at last age
        
        return(U)
}

## Construct U for both sexes
get_U_2sex <- function(lifeTable, y, r) {
        
        
        ## U for both sexes
        U <- lapply(c("female", "male"), get_U, y, r, lifeTable)
        
        U_2sex <- bdiag(U)
        
        return(as.matrix(U_2sex))
}

## Construct F matrix from fx
get_F <- function(s, y, r, ages, asfr) {
        
        omega <- length(ages)
        ## Extract asfr
        fx <- asfr %>% 
                filter(sex == s,
                       year == y,
                       race_eth == r) %>% 
                pull(fx)
        ## Reproduction ages present in asfr
        ages.repro <- asfr %>% 
                filter(sex == s,
                       year == y,
                       race_eth == r) %>%
                pull(age)
        ## Creation of F
        F. <- matrix(0, 
                     nrow = omega, 
                     ncol = omega)
        ## ASFR on 1st row
        F.[1, (ages.repro + 1)] <- fx 
        
        return(F.)
}

## Construct F for both sexes
get_F_2sex <- function(birth_female = 1/2.04, ages, asfr, y, r) {
        
        
        ## U for both sexes
        F. <- sapply(c("female", "male"), function(s) {
                
                F. <- get_F(s, y, r, ages, asfr)
                
                return(F.)
        },
        simplify = FALSE,
        USE.NAMES = TRUE)
        
        # Create block matrix when focal is female or male
        block_f <- cbind(birth_female * F.[["female"]], birth_female * F.[["male"]])
        block_m <- cbind((1 - birth_female) * F.[["female"]], (1 - birth_female) * F.[["male"]])
        
        F_2sex <- rbind(block_f, 
                        block_m)
        
        return(as.matrix(F_2sex))
}

## Construct phi depending on Focal sex (ie mother or father)
get_phi <- function(ages, sex = "female") {
        
        omega <- length(ages)
        
        phi <- diag(rep(1, omega))
        zero <- diag(rep(0, omega))
        
        if (sex == "female") {
                PHI <- rbind(phi, zero)
        } else {
                PHI <- rbind(zero, phi)
        }
        return(PHI)
}