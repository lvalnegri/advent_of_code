library(data.table)
y <- fread('./2021/2.csv')

y[, X1 := 0][V1 == 'forward', X1 := V2]
y[, Y1 := ifelse(V1 == 'forward', 0, ifelse(V1 == 'down', V2, -V2))]
sum(y$X) * sum(y$Y)

y[, Z := ifelse(V1 == 'forward', 0, ifelse(V1 == 'down', V2, -V2))][, Z := cumsum(y[1:.N, Z])]
y[, `:=`(X2 = 0, Y2 = 0)][V1 == 'forward', `:=`(X2 = V2, Y2 = V2 * Z)]
sum(y$X2) * sum(y$Y2)

