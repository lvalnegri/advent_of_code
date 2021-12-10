#####################################################################
# Advent of Code 2021 - Day 8 - https://adventofcode.com/2021/day/8 #
#####################################################################

library(data.table)
y <- fread('./2021/8.txt', header = FALSE, sep = ' ', drop = 11)

# answer part 1
y1 <- y[, 11:14][, lapply(.SD, nchar)]
melt(y1, measure.vars = names(y1))[value %in% c(2, 3, 4, 7), .N]

# answer part 2
get_s <- function(x) y2[n == x, unlist(Y)]
dgt <- data.table(
  as_int = 0:9,
  as_chr = c(
        'abcefg', 'cf', 'acdeg', 'acdfg', 'bcdf', 
        'abdfg', 'abdefg', 'acf', 'abcdefg', 'abcdfg')
  )
ys <- 0
for(idx in 1:nrow(y)){
    y2 <- as.character(y[idx, 1:10])
    y2 <- lapply(1:10, \(x) strsplit(y2[x], split = '')[[1]])
    y2 <- data.table( n = sapply(1:10, \(x) length(y2[[x]])), Y = y2)
    yx <- character(7)
    yx[1] <- setdiff(get_s(3), get_s(2))
    yx[5] <- setdiff(names(which(table(get_s(5)) < 3)), get_s(4))
    yx[7] <- setdiff(get_s(7), c(get_s(3), get_s(4), yx[5]))
    yx[2] <- setdiff(names(which(table(get_s(5)) < 3)), c(get_s(2), yx[5]))
    yx[4] <- setdiff(get_s(4), c(get_s(2), yx[2]))
    yx[3] <- setdiff(names(which(table(get_s(6)) < 3)), c(yx[4], yx[5]))
    yx[6] <- setdiff(get_s(7), yx)
    yx <- data.table(letters[1:7], yx)
    ys <- ys + as.integer(paste0(
                  lapply(
                      paste0('V1', 2:5),
                      \(z)
                        dgt[as_chr == paste0(
                            sort(
                                sapply(
                                    1:nchar(y[idx, get(z)]), 
                                    \(x) 
                                        yx[yx == substr(y[idx, get(z)], x, x), V1]
                                )
                            ), 
                            collapse = ''
                        ), as_int]  
                  ),
                  collapse = ''
    ))
}
ys
