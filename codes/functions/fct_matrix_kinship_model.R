
##==============================================================================
##
##  Project title: Parental death from drugs/firearms
##
##  Functions to create vectors and matrices to run the matrix kinship model.
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



## Load packages 
packages <- c("Matrix")
for(p in packages){
        if(!require(p,character.only = TRUE)) install.packages(p)
        library(p,character.only = TRUE)
}

# Construct U matrix from a lifetable
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

# Construct \hat{M} matrix
get_M_hat <- function(s, y, r, lifeTable, MC=TRUE) {
        
        ## In Monte-Carlo, cause-specific mx is computed in script for 
        ## Binomial in Monte-Carlo simulation
        if (MC) {
                M <- lifeTable %>%
                        filter(sex == s,
                               year == y,
                               race_eth == r) %>%
                        dplyr::select(qx_other, qx_drug, qx_firearm) %>%
                        as.matrix %>%
                        t()
                
                
        ## In the validation, we don't run a Monte-Carlo simulation        
        } else {
                ## Extract death probs
                qx <- lifeTable %>%
                        filter(sex == s,
                               year == y,
                               race_eth == r) %>%
                        pull(qx)
                
                ## Create hazard matrix, dim(H)=(alpha*omega)
                H <- lifeTable %>%
                        filter(sex == s,
                               year == y,
                               race_eth == r) %>%
                        dplyr::select(mx_other, mx_drug, mx_firearm) %>%
                        as.matrix %>%
                        t()
                
                ## Create summed hazard
                h_tilde <- t(rep(1, nrow(H))) %*% H
                
                ## Compute M (Caswell et al (2023))
                M <- H %*% solve(diag(c(h_tilde))) %*% diag(qx)
        }
        
        ## Store columns of M as a list of vectors
        M.cols <- lapply(1:ncol(M), function(j) return(M[,j]))
        
        ## Create M_hat using the vectors as elements of the block diagonal
        M_hat <- bdiag(M.cols)
        
        return(M_hat)
}

# Construct U_tilde from U and M
get_U_tilde <- function(lifeTable, y, r, cum = F, MC=TRUE) {
        
        
        ## U for both sexes
        U <- lapply(c("female", "male"), get_U, y, r, lifeTable)
        
        ## M_hat for both sexes
        M_hat <- lapply(c("female", "male"), get_M_hat, y, r, lifeTable, MC)
        
        ## Dimensions
        omega <- dim(M_hat[[1]])[2]
        alphaomega <- dim(M_hat[[1]])[1]
        
        ## Construct block matrix, block by block
        ## Upper-left block: survival probs.
        block_UL <- bdiag(U)
        ## Lower-left block: death probs. by cause
        block_LL <- bdiag(M_hat)
        ## Upper-right block: death can't become alive 
        zeros <- matrix(0,nrow = omega, ncol = alphaomega)
        block_UR <- bdiag(list(zeros, zeros))
        ## Lower-right block: 
        if (cum) {
                I <- diag(alphaomega)
                block_LR <- bdiag(list(I, I))
        } else {
                zeros <- matrix(0, nrow = alphaomega, ncol = alphaomega)
                block_LR <- bdiag(list(zeros, zeros))
        }
        ## Combine
        block_U <- cbind(block_UL, block_UR)
        block_L <- cbind(block_LL, block_LR)
        
        U_tilde <- rbind(block_U, block_L)
        
        return(as.matrix(U_tilde))
}

# Construct F matrix from fx
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

## Dist. of the ages of the parents of offspring 
## NOTE: Using norm() for 1-norm does not lead to 
##      take the sum of the absolute values of a vector.
##      -> We compute it manually
get_pi <- function(ages, asfr, lifeTable, y, r) {
        
        
        PI <- sapply(c("female", "male"), function(s) {
                
                ## Fertility matrix
                F. <- get_F(s, y, r, ages, asfr)
                ## Population vector
                z <- lifeTable %>% 
                        filter(sex == s,
                               year == y,
                               race_eth == r) %>% 
                        arrange(age) %>% 
                        pull(pop)
                ## Population age structure
                z <- z/sum(z)
                ## Compute distribution of ages at offspring
                num <- t(F.[1, ]) * z
                denom <- sum( abs(num) )
                pi <- num / denom
                
                return(pi)
        },
        simplify = T,
        USE.NAMES = T)

        return(c(PI))
}

