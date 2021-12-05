#####################################################################
# Advent of Code 2021 - Day 5 - https://adventofcode.com/2021/day/5 #
#####################################################################

library(data.table)
y <- fread("sed 's/ -> /,/g' ./2021/5.txt", sep = ',', col.names = c('xs', 'ys', 'xe', 'ye'))
my_fun <- function(x) 
                rbindlist(lapply(
                    1:nrow(x),
                    function(idx) data.table( x[idx, xs]:x[idx, xe], x[idx, ys]:x[idx, ye] )
                ))[, .N, .(V1, V2)][N > 1, .N]

# answer part 1
my_fun(y[xs == xe | ys == ye])

# answer part 2
my_fun(y)

# plot
y <- rbindlist(lapply(1:nrow(y), function(idx) data.table( y[idx, xs]:y[idx, xe], y[idx, ys]:y[idx, ye] ) ))
ggplot(y, aes(V1, V2)) + 
    geom_point(size = 0.001) + 
    geom_point(data = y[, .N, .(V1, V2)][N > 1], color = 'red', aes(size = 0.0001 * N)) +
    guides(size = 'none') + 
    theme_void()
