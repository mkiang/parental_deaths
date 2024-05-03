
##------------- FUNCTION TO GET AGE OF PARENT OF OFFSPRING FROM CPS ------------
## 
## 
##  Author: Benjamin Schlüter
##  Date: November 2023
##
##
## -----------------------------------------------------------------------------

## Function allowing to obtain the parental age at offspring from CPS data

get_age_parent_cps <- function(d, ...) {
        
        
        df.out <- d
        ## Who is parent of minor
        find <- d$momloc != 0 | d$momloc2 != 0 | d$poploc != 0 | d$poploc2 != 0
        
        ## Only do something in HH with parents
        ## HH without parent can be filter with is.na(agebirth)
        if (sum(find) != 0) {
                
                ## How many time are they listed parent
                n <- table(c(d$momloc[find], d$momloc2[find], d$poploc[find], d$poploc2[find]))
                ## Do not account for 0 (construction of "find" will always contain a 0)
                n <- tail(n, -1)
                ## Identify parents
                id <- as.numeric(names(n))
                ## Select parent(s) row(s)
                parent <- d[d$pernum %in% id, ]
                ## Replace df.out. Rows equal to number of children
                df.out <- parent[rep(1:length(id), times = n), ]
                ## Compute age of parent(s) at offspring(s)
                age.birth <- unlist(
                        lapply(id, function(x) {
                                d$age[d$pernum == x] - d$age[d$momloc == x | d$poploc == x | d$momloc2 == x | d$poploc2 == x]
                        }
                        ))
                # age.birth <- unlist(
                #         map(d$age[d$pernum %in% id], `-`, d$age[find])
                #         )
                df.out$agebirth <- age.birth
        }
        
        
        return(df.out)
}