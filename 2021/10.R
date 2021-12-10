#######################################################################
# Advent of Code 2021 - Day 10 - https://adventofcode.com/2021/day/10 #
#######################################################################

library(data.table)
y <- readLines('./2021/10.txt')
yp <- data.table(
        'op' = c('(', '[', '{', '<'), 
        'cl' = c(')', ']', '}', '>'),
        'nm' = c(3, 57, 1197, 25137)
)

ys <- 0
yi <- NULL
for(m in 1:length(y)){
    yc <- character(0)
    yx <- strsplit(y[m], '')[[1]]
    yis <- 0
    for(n in 1:length(yx)){
        if(yx[n] %in% yp$op){
            yc <- c(yc, yx[n])
        } else {
            if(yp[grepl(yx[n], cl), op] == yc[length(yc)]){
                yc <- head(yc, -1)
            } else {
                ys <- ys + yp[grepl(yx[n], cl), nm]
                break
            }
        }
    }
    if(n == length(yx)){
        for(n in length(yc):1){
            yis <- yis * 5 + which(yp$op == yc[n])
        }
        yi <- c(yi, yis)
    }
}

# answer part 1
ys

# answer part 2
median(yi)
