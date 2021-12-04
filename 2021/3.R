#####################################################################
# Advent of Code 2021 - Day 3 - https://adventofcode.com/2021/day/3 #
#####################################################################

library(data.table)
y <- fread('./2021/3.csv', colClasses = 'character')
y <- y[, tstrsplit(V1, split = '')]

bin2int <- function(b){ strtoi( paste0( as.integer( b ), collapse = ''), base = 2 ) }
rates <- function(n)  bin2int( y[, lapply(.SD, \(x) sum(x == n))] > nrow(y) / 2 )
rates(0) * rates(1)

out <- list()
for(n in 0:1){
    yx <- copy(y)
    for(x in 1:ncol(y)){
        xv <- paste0('V', x)
        yx <- yx[get(xv) == eval(parse(text = paste( 
                    'as.integer( yx[, sum(get(xv) == 1)]', ifelse(n == 0, '<', '>=') , 'yx[, sum(get(xv) == 0)] )' 
        ) )) ]
        if(nrow(yx) == 1) break
    }
    out[n + 1] <- bin2int(yx)
}    
out[[1]] * out[[2]]
