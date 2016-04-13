# diamonds

set.seed(101)

N <- 150

weight <- round(rnorm(N, mean = 10, sd = 2), 2)

clarity <- round(rnorm(N, mean = 1, sd = .2), 2)

color <- round(rnorm(N, mean = 5, sd = 1), 0)

value <- round(2 * weight + 20 * clarity + rnorm(N, mean = N, sd = 20), 1)

diamonds <- data.frame(weight, clarity, color, value)

#summary(lm(value ~., data = diamonds))


save(diamonds, file = "data/diamonds.RData")

