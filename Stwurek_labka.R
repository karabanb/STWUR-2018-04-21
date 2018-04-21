
library(tidyverse)
library(mlr)
library(mlrMBO)


set.seed(15390)

dat <- read.csv(file = "mieszkania_mlr.csv", encoding = "UTF-8")

predict_affordable <- makeClassifTask(id = "affordableApartments",
                                      data = dat, target = "tanie")

all_learners <- listLearners()
listLearners("classif", properties = c("prob"))[1L:3, 1L:3]

learnerRF <- makeLearner("classif.randomForest", predict.type = "prob")
learnerNN <- makeLearner("classif.nnet", predict.type = "prob")

cv_scheme <- makeResampleDesc("CV", iters = 5, stratify = TRUE)
