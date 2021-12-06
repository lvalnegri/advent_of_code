#####################################################################
# Advent of Code 2021 - Day 2 - https://adventofcode.com/2021/day/2 #
#####################################################################

library(data.table)
y <- fread('./2021/2.csv')

# answer part 1
y[, X1 := 0][V1 == 'forward', X1 := V2]
y[, Y1 := ifelse(V1 == 'forward', 0, ifelse(V1 == 'down', V2, -V2))]
sum(y$X1) * sum(y$Y1)

# answer part 2
y[, Z := ifelse(V1 == 'forward', 0, ifelse(V1 == 'down', V2, -V2))][, Z := cumsum(y[1:.N, Z])]
y[, `:=`(X2 = 0, Y2 = 0)][V1 == 'forward', `:=`(X2 = V2, Y2 = V2 * Z)]
sum(y$X2) * sum(y$Y2)

