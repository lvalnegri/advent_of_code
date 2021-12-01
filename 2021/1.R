library(data.table)
y <- fread('./2021/1.csv')
y[, sum(V1 > shift(V1), na.rm = TRUE)]
y[, sum(frollsum(V1, 3) > frollsum(shift(V1), 3), na.rm = TRUE)]
