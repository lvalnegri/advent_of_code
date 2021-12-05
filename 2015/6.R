#####################################################################
# Advent of Code 2015 - Day 6 - https://adventofcode.com/2015/day/6 #
#####################################################################

library(data.table)
y <- fread("sed 's/toggle /toggle NA /g' ./2015/6.txt", sep = ' ', header = FALSE, )
y[!is.na(V2), V1 := paste(V1, V2)]
y[, c('xs', 'ys') := tstrsplit(V3, split = ',', type.convert = TRUE)]
y[, c('xe', 'ye') := tstrsplit(V5, split = ',', type.convert = TRUE)]
y[, paste0('V', 2:5) := NULL]

# answer part 1
ym <- matrix(rep(-1, 1e6), nrow = 1000)
for(idx in 1:nrow(y)){
    yx <- y[idx]
    if(yx$V1 == 'toggle'){
        ym[yx$xs:yx$xe, yx$ys:yx$ye] <- ym[yx$xs:yx$xe, yx$ys:yx$ye] * -1 
    } else {
        ym[yx$xs:yx$xe, yx$ys:yx$ye] <- ifelse(yx$V1 == 'turn on', 1, -1) 
    }
}
sum(ym==1)

# answer part 2
ym <- matrix(rep(0, 1e6), nrow = 1000)
for(idx in 1:nrow(y)){
    yx <- y[idx]
    ym[yx$xs:yx$xe, yx$ys:yx$ye] <- ym[yx$xs:yx$xe, yx$ys:yx$ye] +
        ifelse(yx$V1 == 'turn on', 1, ifelse(yx$V1 == 'turn off', -1, 2)) 
    ym[ym < 0] <- 0
}
sum(ym)
