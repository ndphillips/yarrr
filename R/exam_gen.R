
# auction_gen

set.seed(100)

N <- 100

examscores <- data.frame(
  a = round(rnorm(N, mean = 50, sd = 10), 0),
  b = round(rnorm(N, mean = 30, sd = 5), 0),
  c = round(rnorm(N, mean = 80, sd = 10), 0),
  d = round(rnorm(N, mean = 40, sd = 5), 0)
)


save(examscores, file = "data/examscores.RData")



