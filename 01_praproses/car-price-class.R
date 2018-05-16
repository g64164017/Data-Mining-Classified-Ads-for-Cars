# The data was scraped from several websites in Czech Republic and Germany 
# over a period of more than a year. Originally I wanted to build a model 
# for estimating whether a car is a good buy or a bad buy based on the posting. 
# https://www.kaggle.com/mirosval/personal-cars-classifieds


# INISIALISASI
# rm(list = ls())
# full_df <- read.csv("all_anonymized_2015_11_2017_03.csv", header = T, stringsAsFactors = T)
rm(list = ls()[ls()!="full_df"])

ds <- full_df

# OBSERVASI
summary(ds)

# PREPROCESSING
## PRICE_EUR

# remove outlier
boxplot(ds$price_eur)
hist(ds$price_eur)
(qnt <- quantile(ds$price_eur, probs=c(.25, .75)))
(H <- 1.5 * IQR(ds$price_eur))
ds <- ds[!ds$price_eur > qnt[2] + H,]   # remove outlier instance
# ds$price_eur[ds$price_eur > qnt[2] + H] <- qnt[2] + H
boxplot(ds$price_eur)
hist(ds$price_eur)


# discretize price (generate class)
library(infotheo)
cl.n = 3
ds$price_eq <- as.factor(discretize(ds$price_eur, "equalfreq", cl.n)$X)
ds$price_ew <- as.factor(discretize(ds$price_eur, "equalwidth", cl.n)$X)
for(i in levels(ds$price_eq)){
  levels(ds$price_eq)[i] <- paste(min(ds$price_eur[ds$price_eq == i]), max(ds$price_eur[ds$price_eq == i]))
}
hist(as.numeric(ds$price_eq))
hist(as.numeric(ds$price_ew))

boxplot(ds$price_eur[ds$price_eq == 1])
boxplot(ds$price_eur[ds$price_eq == 2])
boxplot(ds$price_eur[ds$price_eq == 3])
boxplot(ds$price_eur[ds$price_eq == 4])


## MAKER
# levels(ds$maker)[levels(ds$maker)==""] <-NA
# summary(ds$maker)

## MODEL
# levels(ds$model)[levels(ds$model)==""] <-NA
# summary(ds$maker)

## MELEAGE
# length(ds$mileage[ds$pri])
# summary(ds$mileage)

# DATA REDUCTION
## find duplicate
library(dplyr)
ds <- distinct(ds)

library(randomForest)
(ds.forest <- randomForest(price_eq ~ ., 
                          data = ds[,-(ncol(ds)-1)], 
                          na.action = na.omit,
                          importance = TRUE,
                          proximity = TRUE
                          ))

## RESAMPLING
idx <- sample(1:dim(ds)[1], 0.6 * dim(ds)[1])
ds.sample <- ds[idx,]

# CLUSTER DETECTION

# (ds.kmeans <- kmeans(ds, 10))

# library(klaR)
# corclust(ds)

# ds.hc <-hclust(dist(ds.sample[,-ncol(ds.sample)]), method = "ave")
## Error: cannot allocate vector of size 15670.7 Gb

# library(clustMixType)
# ds.kpres <- kproto(ds.sample, cl.n)
# clprofiles(ds.kpres, ds.sample)
