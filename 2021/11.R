#######################################################################
# Advent of Code 2021 - Day 11 - https://adventofcode.com/2021/day/11 #
#######################################################################

library(data.table)
y <- as.matrix(fread('./2021/11.txt', header = FALSE)[, tstrsplit(V1, split = '')][, lapply(.SD, as.integer)])

add_one <- function(y, i, j){
    yx <- matrix(rep(FALSE, nrow(y) * ncol(y)), ncol = ncol(y))
    yx[seq(max(0, i-1), min(nrow(y), i+1)), seq(max(0, j-1), min(ncol(y), j+1))] <- TRUE
    y + yx
}
ps <- 0
nf <- 0
repeat({
    ps <- ps + 1
    y <- y + 1
    yp <- matrix(rep(FALSE, nrow(y) * ncol(y)), ncol = ncol(y))
    done <- FALSE
    while(!done){
        done <- TRUE
        for(i in 1:nrow(y)){
            for(j in 1:ncol(y)){
                if(!yp[i, j] & y[i, j] > 9){
                    y <- add_one(y, i, j)
                    yp[i, j] <- TRUE
                    nf <- nf + 1
                    done <- FALSE
                }
            }
        }
    }
    y[y > 9] <- 0
    if(ps == 100) print(nf)
    if(sum(y) == 0){
        print(ps)
        break
    }
})
