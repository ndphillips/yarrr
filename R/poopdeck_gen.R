# poopdeck

set.seed(200)

poopdeck <- data.frame(cleaner = rep(c("a", "b", "c"), times = 200),
                        type = rep(c("parrot", "shark"), each = 3, times = 100),
                       day = rep(1:100, each = 6),
                       stringsAsFactors = F
                        )



cleaner.num <- rep(0, 600)

cleaner.num[poopdeck$cleaner == "a"] <- 20
cleaner.num[poopdeck$cleaner == "b"] <- 15
cleaner.num[poopdeck$cleaner == "c"] <- 20

type.num <- rep(0, 600)

type.num[poopdeck$type == "parrot"] <- 5
type.num[poopdeck$type == "shark"] <- 30

int.num <- rep(0, 600)
int.num[poopdeck$cleaner == "a" & poopdeck$type == "parrot"] <- -10
int.num[poopdeck$cleaner == "a" & poopdeck$type == "shark"] <- 10

int.num[poopdeck$cleaner == "c" & poopdeck$type == "parrot"] <- 0
int.num[poopdeck$cleaner == "c" & poopdeck$type == "shark"] <- -15


time.mean <- cleaner.num + type.num + int.num
time <- round(rnorm(600, mean = time.mean + 30, sd = 20), 0)

poopdeck$time <- time

poopdeck <- poopdeck[c("day", "cleaner", "type", "time")]


save(poopdeck, file = "data/poopdeck.RData")
