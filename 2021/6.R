#####################################################################
# Advent of Code 2021 - Day 6 - https://adventofcode.com/2021/day/6 #
#####################################################################

library(data.table)
y <- data.table(as.integer(unlist( strsplit( readLines('./2021/6.txt', 1), split = ',') )))
my_fun <- function(x, nd){
    y <- x[, .N, V1]
    yn <- 0
    for(idx in 1:nd){
        y <- y[, V1 := V1 - 1]
        if(length(yn) > 0) y <- rbindlist(list( y, data.table( V1 = 8, N = yn ) ))
        yn <- y[V1 == 0, N]
        y[V1 == -1, V1 := 6]
        y <- y[, .(N = sum(N)), V1]
    }
    print(sum(y$N), digits = 20)
}

# answer part 1
my_fun(y, 80)

# answer part 2
my_fun(y, 256)
