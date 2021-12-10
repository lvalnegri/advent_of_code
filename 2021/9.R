#####################################################################
# Advent of Code 2021 - Day 9 - https://adventofcode.com/2021/day/9 #
#####################################################################

library(data.table)
y <- fread('./2021/9.txt', header = FALSE)
y <- y[, tstrsplit(V1, split = '')][, lapply(.SD, as.integer)]

# answer part 1
calc_adj <- function(px, py){
  yf <- 'min('
  if(px > 1) yf <- paste(yf, "y[px - 1, get(paste0('V', py))]")
  if(px < nrow(y))  yf <- paste(yf, ifelse(px > 1, ',', ''), "y[px + 1, get(paste0('V', py))]")
  if(py > 1) yf <- paste(yf, ',', "y[px, get(paste0('V', py - 1))]")
  if(py < ncol(y))  yf <- paste(yf, ',', "y[px, get(paste0('V', py + 1))]")
  ym <- eval(parse(text = paste(yf, ')')))
  ifelse(ym > y[px, get(paste0('V', py))], y[px, get(paste0('V', py))], NA) 
}
yx <- rbindlist( lapply(1:nrow(y), \(m) lapply(1:ncol(y), \(n) calc_adj(m, n)) ) )
sum(as.matrix(yx)[!is.na(yx)] + 1)

# answer part 2
