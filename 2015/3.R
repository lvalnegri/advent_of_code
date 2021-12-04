#####################################################################
# Advent of Code 2015 - Day 3 - https://adventofcode.com/2015/day/3 #
#####################################################################

library(data.table)
y <- data.table(readLines('./2015/3.txt'))
y <- y[, strsplit(V1, split = '')]
y <- y[, `:=`( 
          mX = ifelse(V1 == '>', 1, ifelse(V1 == '<', -1, 0)),
          mY = ifelse(V1 == '^', 1, ifelse(V1 == 'v', -1, 0))
)]

# answer part 1
y1 <- copy(y)[, `:=`( X = cumsum(mX), Y = cumsum(mY) )]
unique(y1[, .(X, Y)])[, .N]

# answer part 2
y2 <- copy(y)[, SR := rep_len(c('S', 'R'), nrow(y))][, `:=`( X = cumsum(mX), Y = cumsum(mY) ), SR]
unique(y2[, .(X, Y)])[, .N]
