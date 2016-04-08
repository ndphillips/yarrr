# poopdeck

set.seed(200)
cleaner <- rep(c("a", "b", "c"), each = 100)
type <- rep(c("pirate", "shark"), times = 150)


cleaner.num <- rep(0, 300)

cleaner.num[cleaner == "a"] <- 20
cleaner.num[cleaner == "b"] <- 15
cleaner.num[cleaner == "c"] <- 20

type.num <- rep(0, 300)

type.num[type == "pirate"] <- 5
type.num[type == "shark"] <- 30

int.num <- rep(0, 300)
int.num[cleaner == "a" & type == "pirate"] <- -10
int.num[cleaner == "a" & type == "shark"] <- 10

int.num[cleaner == "c" & type == "pirate"] <- 0
int.num[cleaner == "c" & type == "shark"] <- -15


time.mean <- cleaner.num + type.num + int.num
time <- round(rnorm(300, mean = time.mean + 30, sd = 20), 1)

poopdeck <- data.frame(cleaner = cleaner,
                       type = type,
                       time = time,
                       stringsAsFactors = F)

poopdeck <- poopdeck[sample(1:nrow(poopdeck)),]
rownames(poopdeck) <- 1:nrow(poopdeck)

save(poopdeck, file = "data/poopdeck.RData")
