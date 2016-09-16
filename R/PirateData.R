# Create pirate dataset for YaRrr

rerun <- F

if(rerun) {

{

set.seed(100)

n <- 1000

pirates <- data.frame("id" = 1:n,
                      "sex" = sample(c("male", "female", "other"), size = n, replace = T, prob = c(.48, .48, .04)),
                      "headband" = sample(c("yes", "no"), size = n, replace = T, prob = c(.9, .1)),
                      "age" = round(rnorm(n, 30, 7), 0), stringsAsFactors = F
)

n.males <- sum(pirates$sex == "male")
n.females <- sum(pirates$sex == "female")
n.other <- sum(pirates$sex == "other")


# Create age as a function of sex

pirates$age[pirates$sex == "male"] <- round(rnorm(n.males, mean = 25, sd = 5))
pirates$age[pirates$sex == "female"] <- round(rnorm(n.females, mean = 30, sd = 5))
pirates$age[pirates$sex == "other"] <- round(rnorm(n.other, mean = 27.5, sd = 5))

# Create college as a function of age

college.p <- 1 / (1 + exp(-pirates$age + 30))

pirates$college <- unlist(lapply(1:n, function(x) {sample(c("JSSFP",
                                                            "CCCC"), size = 1,
                                                          prob = c(college.p[x], 1 - college.p[x]))}))


# Create tattoos as a function of headband use

pirates$tattoos[pirates$headband == "yes"] <- round(rnorm(sum(pirates$headband == "yes"), mean = 10, sd = 3), 0)
pirates$tattoos[pirates$headband == "no"] <- round(rnorm(sum(pirates$headband == "no"), mean = 5, sd = 3), 0)
pirates$tattoos[pirates$tattoos < 0] <- 0

# Create tchests found as a function of age and tattoos
pirates$tchests <- round(rexp(n, 5 / (pirates$age * 4 + pirates$tattoos / 5)), 0)

# Create parrots as a function of age
pirates$parrots <- round(rexp(n, 1 / pirates$age * 10), 0)

# Create favorate pirate as a function of sex

pirates$favorite.pirate[pirates$sex == "male"] <- sample(x = c("Jack Sparrow", "Blackbeard", "Lewis Scot", "Hook", "Edward Low", "Anicetus"),
                                                         size = sum(pirates$sex == "male"),
                                                         replace = T,
                                                         prob = c(.75, .05, .05, .05, .05, .05)
)

pirates$favorite.pirate[pirates$sex != "male"] <- sample(x = c("Jack Sparrow", "Blackbeard", "Lewis Scot", "Hook", "Edward Low", "Anicetus"),
                                                         size = sum(pirates$sex != "male"),
                                                         replace = T,
                                                         prob = rep(1/6, times = 6)
)


# Create sword type as a function of headband

pirates$sword.type[pirates$headband == "yes"] <- sample(c("cutlass", "sabre", "scimitar", "banana"),
                                                        size = sum(pirates$headband == "yes"), replace = T,
                                                        prob = c(.9, .04, .04, .01))

pirates$sword.type[pirates$headband == "no"] <- sample(c("cutlass", "sabre", "scimitar", "banana"),
                                                       size = sum(pirates$headband == "no"), replace = T,
                                                       prob = c(.1, .3, .3, .3))

# Create eye-patch as a function of age and parrots

pirates$eyepatch <- unlist(lapply(1:nrow(pirates), function(x) {

  age.i <- pirates$age[x]
  parrots.i <- pirates$parrots[x]

  patch.prob <- 1 / (1 + exp(-age.i / 50 - parrots.i / 20))

  patch.i <- sample(c(1, 0), size = 1, replace = T, prob = c(patch.prob, 1 - patch.prob))


  return(patch.i)

}))


# Create sword speed as a function of eyepatch use and sword.type

pirates$sword.time <- unlist(lapply(1:nrow(pirates), function(x) {

  sword.i <- pirates$sword.type[x]
  eyepatch.i <- pirates$eyepatch[x]

  sword.num.convert <- data.frame("sword" = c("cutlass", "sabre", "scimitar", "banana"),
                                  "num" = c(15, 2, 1, .0001)
  )

  eyepatch.num.convert <- data.frame("eyepatch" = c(1, 0),
                                     "num" = c(1, 5)
  )

  sword.num <- sword.num.convert$num[sword.num.convert$sword == sword.i]
  eyepatch.num <- eyepatch.num.convert$num[eyepatch.num.convert$eyepatch == eyepatch.i]

  speed.i <- rexp(1, rate = (sword.num + eyepatch.num / 5) / 10)


  return(round(speed.i, 2))

}))



# Create beard-length as a function of sex and tattoos

pirates$beard.length <- unlist(lapply(1:nrow(pirates), function(x) {

  sex.i <- pirates$sex[x]
  tattoos.i <- pirates$tattoos[x]

  if(sex.i == "male") {sex.num <- 10}
  if(sex.i == "female") {sex.num <- 0}
  if(sex.i == "other") {sex.num <- 5}

  beard.length <- rnorm(1, mean = sex.num + tattoos.i, sd = 5)

if(sex.i == "female") {beard.length <- rnorm(1, mean = 0, sd = 1)}
  if(beard.length < 0) {beard.length <- 0}

  return(round(beard.length, 0))

}))

# Create favorite pixar movie as a function of eyepatch

pirates$fav.pixar <- unlist(lapply(1:nrow(pirates), function(x) {

  movie.vec <- c("Toy Story",
                 "A Bug's Life",
                 "Toy Story 2",
                 "Monsters, Inc.",
                 "Finding Nemo",
                 "The Incredibles",
                 "Cars",
                 "Ratatouille",
                 "WALL-E",
                 "Up",
                 "Toy Story 3",
                 "Cars 2",
                 "Brave",
                 "Monsters University",
                 "Inside Out")

  patch.i <- pirates$eyepatch[x]
  prob.vec.1 <- c(10, 10, 10, 20, 200, 30, 10, 1, 25, 40, 5, 5, 10, 35, 5)
  prob.vec.2 <- c(10, 10, 10, 20, 10, 30, 10, 1, 25, 40, 5, 5, 10, 35, 200)

  if(patch.i == 0) {prob.vec.i <- prob.vec.1}
  if(patch.i > 0) {prob.vec.i <- prob.vec.2}

fav.pix <- sample(movie.vec, size = 1, prob = prob.vec.i / sum(prob.vec.i))


  return(fav.pix)

}))

# Height as a function of sex

pirates$height[pirates$sex == "female"] <- rnorm(n = sum(pirates$sex == "female"), mean = 163, sd = 10)
pirates$height[pirates$sex == "male"] <- rnorm(n = sum(pirates$sex == "male"), mean = 177, sd = 10)
pirates$height[pirates$sex == "other"] <- rnorm(n = sum(pirates$sex == "other"), mean = 170, sd = 10)
pirates$height <- round(pirates$height, 2)

# weight as a function of sex and height

pirates$weight <- round(24 * (pirates$height / 100) ^ 2 + rnorm(nrow(pirates), mean = 0, sd = 4), 1)

# grogg as a function of nothing
pirates$grogg <- round(rnorm(n, mean = 10, sd = 3), 0)
pirates$grogg[pirates$grogg < 0] <- 0


# reorder columns

pirates <- pirates[c("id", "sex", "age", "height", "weight", setdiff(names(pirates), c("id", "sex", "age", "height", "weight")))]

# sort randomly

pirates <- pirates[sample(n, size = n),]
pirates$id <- 1:n
rownames(pirates) <- 1:n

# ------------------
# Write table to file
write.table(pirates, file = "yarrr/data/pirates.txt", sep = "\t")
save(pirates, file = "yarrr/data/pirates.RData")
# ------------------

# ---------
# Create pirate.errors
# ----------


pirateserrors <- pirates

## Add some bad data
pirateserrors$sex[sample(1:nrow(pirateserrors), size = 3)] <- sample(c("yes please!", "sure I'll have some", "depends on who is offering"), size = 3, replace = F)
pirateserrors$age[sample(1:nrow(pirateserrors), size = 20)] <- sample(c(999, 0, -99, 500, 12345), size = 20, replace = T)
pirateserrors$headband[sample(1:nrow(pirateserrors), size = 10)] <- sample(c("sometimes", "what is a headband?"), size = 10, replace = T)
pirateserrors$college[sample(1:nrow(pirateserrors), size = 10)] <- sample(c(NA), size = 10, replace = T)
pirateserrors$tattoos[sample(1:nrow(pirateserrors), size = 10)] <- sample(c(1000000, -10, NA), size = 10, replace = T)
pirateserrors$favorite.pirate[sample(1:nrow(pirateserrors), size = 10)] <- sample(c("your mom"), size = 10, replace = T)
pirateserrors$eyepatch[sample(1:nrow(pirateserrors), size = 20)] <- sample(c(2:10), size = 20, replace = T)
pirateserrors$tchests[sample(1:nrow(pirateserrors), size = 20)] <- sample(1000:10000, size = 20, replace = T)
pirateserrors$parrots[sample(1:nrow(pirateserrors), size = 20)] <- sample(500:1000, size = 20, replace = T)


# Add NAs in random locations

row.vec <- sample(1:nrow(pirateserrors), size = 50, replace = T)
col.vec <- sample(1:ncol(pirateserrors), size = 50, replace = T)

for(i in 1:length(row.vec)) {

pirateserrors[row.vec[i], col.vec[i]] <- NA

}


# Write table to file
write.table(pirateserrors, file = "yarrr/data/Users/Nathaniel/Dropbox/Git/YaRrr_Book/data/pirateserrors.txt", sep = "\t")
save(pirateserrors, file = "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/yarrr/data/pirateserrors.RData")

}


# Ship dataset

shipauction <- data.frame(cannons = sample(c(seq(2, 20, 2)), size = 1000, replace = T),
                    rooms = sample(seq(10, 60, 4), size = 1000, replace = T),
                    age = round(rnorm(1000, mean = 50, sd = 10), 1),
                    style = sample(c("modern", "classic"), size = 1000, replace = T),
                    condition = sample(10:1, size = 1000, replace = T, prob = c(1:5, 5:1)),
                    weight = rnorm(1000, 5000, 500),
                    color = sample(c("black", "brown", "red"), size = 1000, prob = c(.5, .3, .2), replace = T),
                    stringsAsFactors = F
)

shipauction$style <- sapply(1:nrow(shipauction), function(x) {
  sample(c("modern", "classic"), 1,
         prob = c(1 - 1 / (1 + exp(-((shipauction$age[x] - 50) / 10))),
                  1 / (1 + exp(-((shipauction$age[x] - 50) / 10)))))})

shipauction$price[shipauction$style == "modern"] <- with(shipauction[shipauction$style == "modern",],
                                       10000 +
                                       100 * cannons +
                                       500 * rooms +
                                       (-500) * age +
                                       200 * condition
)


shipauction$price[shipauction$style == "classic"] <- with(shipauction[shipauction$style == "classic",],
                                                          0 +
                                                           100 * cannons +
                                                           500 * rooms +
                                                           (300) * age +
                                                           200 * condition
)

shipauction$price[shipauction$color == "black"] <- shipauction$price[shipauction$color == "black"] + 10000
shipauction$price[shipauction$color == "black"] <- shipauction$price[shipauction$color == "brown"] + 0
shipauction$price[shipauction$color == "black"] <- shipauction$price[shipauction$color == "red"] -5000

shipauction$price <- round(shipauction$price + rnorm(1000, mean = 0, sd = 4000), 0)

summary(lm(price~., data = shipauction))

write.table(shipauction, "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/data/shipauction.txt", sep = "\t")

}
