#####################################################################
# Advent of Code 2015 - Day 4 - https://adventofcode.com/2015/day/4 #
#####################################################################

y <- openssl::md5(paste0('ckczppom', 1:5000000))

# answer part 1
which(grepl('^00000', y))

# answer part 2
which(grepl('^000000', y))
