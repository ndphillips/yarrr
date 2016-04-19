
# auction_gen

set.seed(100)

N <- 1000

auction <- data.frame(cannons = round(rnorm(N, mean = 20, sd = 4), 0),
                      rooms = round(rnorm(N, mean = 16, sd = 4), 0),
                      age = round(rnorm(N, mean = 100, sd = 30), 0),
                      condition = round(rbeta(N, 4, 4), 1) * 10,
                      color = sample(c("black", "brown", "red", "salmon", "plum"), size = N, prob = c(.3, .3, .2, .15, .05), replace = T),
                      style = sample(c("classic", "modern"), size = N, prob = c(.5, .5), replace = T),
                      stringsAsFactors = F
)

summary(auction)


auction$jbb <- round(with(auction,
                              100 * cannons +
                              50 * rooms +
                              100 * condition +
                              rnorm(N, mean = 0, sd = 200)
), 0)


auction$jbb[auction$style == "modern"] <- auction$jbb[auction$style == "modern"] - 1 * auction$age[auction$style == "modern"]
auction$jbb[auction$style == "classic"] <- auction$jbb[auction$style == "classic"] + 3 * auction$age[auction$style == "classic"]



hist(auction$jbb)

auction$price <- auction$jbb + rnorm(N, mean = 0, sd = 200)

auction$price[auction$color == "black"] <- auction$price[auction$color == "black"] + 100
auction$price[auction$color == "brown"] <- auction$price[auction$color == "brown"] + 0
auction$price[auction$color == "red"] <- auction$price[auction$color == "red"] - 500
auction$price[auction$color == "salmon"] <- auction$price[auction$color == "salmon"] + 200
auction$price[auction$color == "plum"] <- auction$price[auction$color == "plum"] + 200


auction$price <- round(auction$price, 0)

head(auction)

summary(lm(jbb ~ cannons + rooms + age + condition + color, data = auction))
summary(lm(price ~ cannons + rooms + age + condition + color, data = auction))

summary(lm(jbb ~ age * style, data = auction))

save(auction, file = "~/Dropbox/Git/YaRrr_Book/yarrr/data/auction.RData")

#  write.table(auction, "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/data/auction.txt", sep = "\t")



