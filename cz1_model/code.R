library(mlr)
library(dplyr)

dat <- read.csv(file = "mieszkania_mlr.csv", encoding = "UTF-8")

predict_affordable <- makeClassifTask(id = "affordableApartments", 
                                      data = dat, target = "tanie")

all_learners <- listLearners()
listLearners("classif", properties = c("prob"))[1L:3, 1L:3]

learnerRF <- makeLearner("classif.randomForest", predict.type = "prob")
learnerNN <- makeLearner("classif.nnet", predict.type = "prob")

cv_scheme <- makeResampleDesc("CV", iters = 5, stratify = TRUE)

resample(learnerNN, predict_affordable, cv_scheme, measures = list(auc))

resample(learnerRF, predict_affordable, cv_scheme, measures = list(auc))

bench_affordable <- benchmark(learners = list(learnerRF, learnerNN),
                              tasks = predict_affordable,
                              resamplings = cv_scheme, 
                              measures = list(auc))


model_rf <- train(learnerRF, predict_affordable, subset = 1L:5000)
preds <- predict(model_rf, predict_affordable, subset = 5001L:5853)

calculateROCMeasures(preds)


getParamSet("classif.nnet")
getParamSet("classif.randomForest")

makeLearner("classif.nnet", predict.type = "prob", size = 5)
