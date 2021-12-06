#####################################################################
# Advent of Code 2021 - Day 1 - https://adventofcode.com/2021/day/1 #
#####################################################################

library(data.table)
y <- fread('./2021/1.csv')

# answer part 1
y[, sum(V1 > shift(V1), na.rm = TRUE)]

# answer part 2
y[, sum(frollsum(V1, 3) > frollsum(shift(V1), 3), na.rm = TRUE)]
