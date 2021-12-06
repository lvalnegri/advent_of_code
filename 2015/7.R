#####################################################################
# Advent of Code 2015 - Day 7 - https://adventofcode.com/2015/day/7 #
#####################################################################

library(data.table)
yy <- fread("sed 's/NOT /NA NOT /g' ./2015/7.txt", sep = ' ', header = FALSE, fill = TRUE)
yy[V5 == '', `:=`(V5 = V3, V3 = V1, V1 = NA, V2 = NA)]
yy$V4 <- NULL
setnames(yy, c('x1', 'op', 'x2', 'y'))
yy$pass <- FALSE

yy[yy == 'if'] <- 'iif' # need relabelling as "if" is a keyword in R
yy[yy == 'in'] <- 'inn' # need relabelling as "in" is an operator in R
yy[yy == 'lh'] <- 'lhh' # need relabelling as "lh" is a function in R
yy[yy == 'ls'] <- 'lss' # need relabelling as "ls" is a command in R

# answer part 1
done <- FALSE
pss <- 1
todo <- nrow(yy)
while(!done){
    message('\nPass Number ', pss, ' started. Operations to achieve: ', todo)
    for(idx in 1:nrow(yy)){
        yx <- yy[idx]
        if(yx$pass) next
        if(is.na(yx$op)){
             yt <- paste0(yx$y, ' <- ', yx$x2)
        } else {
            if(is.na(yx$x1)){
                yt <- paste0(yx$y, ' <- 65535 - ', yx$x2) # paste0(yx$y, ' <- bitwNot(', yx$x2, ')')
            } else {
                ytx <- switch(yx$op, 'AND' = 'bitwAnd', 'OR' = 'bitwOr', 'RSHIFT' = 'bitwShiftR', 'LSHIFT' = 'bitwShiftL')
                yt <- paste0(yx$y, ' <- ', ytx, '(', yx$x1, ', ', yx$x2, ')')
            }
        }
        tryCatch({ 
                eval(parse(text = yt)) 
                message(' - Operation "', yt, '" succeeded!')
                yy[idx, pass := TRUE]
                todo <- todo - 1
                print()
            }, 
            error = function(e) { NULL }
        )
    }
    message('Pass Number ', pss, ' finished. Operations left: ', todo)
    pss <- pss + 1
    if(todo == 0) done <- TRUE
}
a

# answer part 2
