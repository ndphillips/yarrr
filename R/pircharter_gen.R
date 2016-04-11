
# charter
# dataset
set.seed(100)

  outcome.sd <- 3
  total.n <- 300

  dv.name <- "time"

  iv1.name <- "company"
  iv1.names <- c("JoRo", "BmcB", "MiPa")
  iv1.vals <- c(10, 3, 12)

  iv2.name <- "destination"
  iv2.names <- c("Portland", "Berlin", "Basel")
  iv2.vals <- c(20, 40, 12)

  prob.mtx <- rbind(c(.4, .3, .3),
                    c(.1, .8, .1),
                    c(.1, .2, .7))

  iv1 <- rep(iv1.names, each = total.n / 3)

  iv2 <- c(
    sample(iv2.names, size = total.n / 3, prob = prob.mtx[1,], replace = T),
    sample(iv2.names, size = total.n / 3, prob = prob.mtx[2,], replace = T),
    sample(iv2.names, size = total.n / 3, prob = prob.mtx[3,], replace = T)
  )



  dv <- rep(0, length(iv2))

  iv1.lu <- data.frame(iv1 = iv1.names,
                       iv1.num = iv1.vals,
                       stringsAsFactors = F
  )

  iv2.lu <- data.frame(iv2 = iv2.names,
                       iv2.num = iv2.vals,
                       stringsAsFactors = F
  )


  for(i in 1:length(dv)) {

    iv1.i <- iv1[i]
    iv2.i <- iv2[i]

    iv1.i.num <- subset(iv1.lu, iv1 == iv1.i)$iv1.num
    iv2.i.num <- subset(iv2.lu, iv2 == iv2.i)$iv2.num

    i.mean <-  iv1.i.num + iv2.i.num

    dv.i <- round(rnorm(1, mean = i.mean, sd = outcome.sd), 2)

    dv[i] <- dv.i

  }

  df <- data.frame(iv1, iv2, dv, stringsAsFactors = F)
  df <- df[sample(1:nrow(df)),]

  names(df) <- c(iv1.name, iv2.name, dv.name)

  rownames(df) <- 1:nrow(df)

  pircharter <- df


  save(pircharter, file = "data/pircharter.RData")


