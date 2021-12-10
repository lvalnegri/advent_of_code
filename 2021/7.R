#####################################################################
# Advent of Code 2021 - Day 7 - https://adventofcode.com/2021/day/7 #
#####################################################################

library(data.table)
y <- data.table(value = as.integer(unlist(strsplit(readLines('./2021/7.txt', 1), split = ','))))

# answer part 1
y1 <- lapply(0:max(y$value), \(x) sum(y[, abs(value - x)]))
y1[which.min(y1)]

# answer part 2
y2 <- lapply(0:max(y$value), \(x) sum(y[, abs(value - x)] * (y[, abs(value - x)] + 1) / 2))
y2[which.min(y2)]
