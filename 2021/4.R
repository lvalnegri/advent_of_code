#####################################################################
# Advent of Code 2021 - Day 4 - https://adventofcode.com/2021/day/4 #
#####################################################################

library(data.table)
yn <- data.table(value = as.integer(unlist(strsplit(readLines('./2021/4.txt', 1), split = ','))))
yn[, draw := 1:.N]

wc <- as.integer(gsub(' .*', '', system('wc -l ./2021/4.txt', intern = TRUE))) %/% 6
st <- 2
y <- rbindlist(lapply(
    1:wc,
    \(idx){
        z <- fread('./2021/4.txt', skip = st + 6 * (idx - 1), nrows = 5)
        data.table(
            table = idx,
            rbindlist(list(
                data.table( dir = 'c', rbindlist(lapply(1:5, \(x) data.table(pos = x, value = z[[x]]) )) ),
                data.table( 'r', rbindlist(lapply(1:5, \(x) data.table(x, t(z[x])) )) )
            ))
        )
    }
))
y <- yn[y, on = 'value'][order(draw)][, win := 1:.N, .(table, dir, pos)]

# answer part 1
wx <- head(y[win == 5], 1)
wx$value * y[table == wx$table & dir == 'c' & draw > wx$draw, sum(value)]

# answer part 2
lx <- head(y[table == tail(unique(y[win == 5, table]), 1)][win == 5], 1)
lx$value * y[table == lx$table & dir == 'c' & draw > lx$draw, sum(value)]
