#####################################################################
# Advent of Code 2015 - Day 2 - https://adventofcode.com/2015/day/2 #
#####################################################################

library(data.table)
y <- fread('./2015/2.csv', header = FALSE)
y[, c('l', 'w', 'h') := tstrsplit(V1, split = 'x', type.convert = TRUE)
  ][, `:=`( area = 2*l*w + 2*w*h + 2*h*l, slack = pmin(l*w, w*h, h*l) )
    ][, total := area + slack]

# answer part 1
sum(y$total)

# answer part 2
mid_min <- Vectorize(function(l, w, h){
  pm <- pmin(l, w, h)
  ifelse(length(which(c(l, w, h) == pm)) > 1, pm, min( setdiff( c(l, w, h), pmin(l, w, h) ) ) )
})
y[, min1 := pmin(l, w, h)][, min2 := mid_min(l, w, h)][, ribbon := 2*(min1 + min2) + l*w*h]
sum(y$ribbon)

