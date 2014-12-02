rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R/beer')
getwd()
dir()

options(width = 100)
library(data.table)

beers <- read.csv("beer_reviews.csv")

beers <- data.table(beers)

review_df <- beers[, list(review_overall, review_aroma, review_appearance, review_palate, 
                          review_taste), by = c("review_profilename", "beer_name", "beer_style")]
setkeyv(review_df, "review_profilename")

beer_similarity <- function(beer1, beer2) {
  reviewers <- review_df[beer_name %chin% c(beer1, beer2), list(num_uniq = length(unique(.SD$beer_name))), 
                         by = "review_profilename"][num_uniq == 2]
  if (nrow(reviewers) <= 1) 
    return(NA)
  reviews <- review_df[reviewers][beer_name %chin% c(beer1, beer2)]
  setkeyv(reviews, c("review_profilename", "beer_name"))
  beer1_reviews <- reviews[, list(review_overall, review_aroma, review_appearance, review_palate, review_taste)]
  beer_dist <- as.matrix(dist(scale(beer1_reviews), diag = TRUE, upper = TRUE))
  two_dist <- vector("numeric", length = (nrow(beer_dist) - 1)/2)
  idx <- 1
  for (i in seq(1, nrow(beer_dist) - 1, by = 2)) {
    two_dist[idx] <- beer_dist[i, i + 1]
    idx <- idx + 1
  }
  names(two_dist) <- unique(reviews$review_profilename)
  sum(dnorm(two_dist, mean = mean(two_dist), sd = sd(two_dist)))/sum(two_dist * dnorm(two_dist, mean = mean(two_dist), 
                                                                                      sd = sd(two_dist)))
}
# http://nbviewer.ipython.org/20a18d52c539b87de2af 결과와 비교
beer1 <- "Coors Light"
beer2 <- c("Natural Light", "Michelob Ultra", "Bud Light", "Blue Moon Belgian White", "Fat Tire Amber Ale", 
           "Dale's Pale Ale", "Guinness Draught", "60 Minute IPA", "Sierra Nevada Pale Ale")
?sapply
require(stats); require(graphics)

sims <- sapply(beer2, function(x) beer_similarity(beer1, x))

sims[order(sims, decreasing = T)]

beer1 <- "Sierra Nevada Pale Ale"
beer2 <- c("Sierra Nevada Stout", "Samuel Adams Boston Lager", "Indica India Pale Ale", "Suntory The Premium Malt's", 
           "Budweiser", "Guinness Draught", "Pilsner Urquell", "Smithwick's", "Stella Artois", "Tsingtao", "Bud Light")

sims <- sapply(beer2, function(x) beer_similarity(beer1, x))

sims[order(sims, decreasing = T)]

# top 50 맥주만 리뷰
no_of_rev <- beers[, list(num_of_rev = .N), by = c("beer_name", "beer_style")]

no_of_rev <- no_of_rev[order(num_of_rev, decreasing = T)]

sims <- sapply(no_of_rev$beer_name[1:50], function(x) beer_similarity("Samuel Adams Boston Lager", x))

no_of_rev[1:50, `:=`(sims, sims)]

no_of_rev[order(sims, decreasing = T)][1:50]