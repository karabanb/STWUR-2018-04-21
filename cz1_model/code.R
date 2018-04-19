library(mlr)
library(dplyr)

dat <- read.csv(file = "mieszkania_mlr.csv", encoding = "UTF-8")

predict_affordable <- makeClassifTask(id = "affordableApartments", 
                                      data = dat, target = "tanie")

all_learners <- listLearners()
filter(all_learners, type == "classif")[["class"]]
learnerRF <- makeLearner("classif.randomForest")
learnerNN <- makeLearner("classif.nnet")

bench_regr <- benchmark(learners = list(learnerRF,
                                        learnerNN),
                        tasks = list(predict_affordable))

