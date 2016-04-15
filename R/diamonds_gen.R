# diamonds

set.seed(101)

N <- 150

weight <- round(rnorm(N, mean = 10, sd = 2), 2)

clarity <- round(rnorm(N, mean = 1, sd = .2), 2)

color <- round(rnorm(N, mean = 5, sd = 1), 0)

value <- round(2 * weight + 20 * clarity + rnorm(N, mean = N, sd = 5), 1)

diamonds <- data.frame(weight, clarity, color, value)

# summary(lm(value ~., data = diamonds))
# plot(value, lm(value ~., data = diamonds)$fitted.values)
# abline(a = 0, b = 1)

save(diamonds, file = "data/diamonds.RData")

