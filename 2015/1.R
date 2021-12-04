#####################################################################
# Advent of Code 2015 - Day 1 - https://adventofcode.com/2015/day/1 #
#####################################################################

library(data.table)
y <- data.table(readLines('./2015/1.txt'))
y <- y[, strsplit(V1, split = '')][, `:=`( M = 1:.N, V = ifelse(V1 == '(', 1, -1) )]

# answer part 1
y[, sum(V)]

# answer part 2
y[, S := cumsum(y[1:.N, V])][S < 0][1, M]
