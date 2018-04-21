
library(tidyverse)
library(mlr)
library(mlrMBO)


set.seed(15390)

dat <- read.csv(file = "mieszkania_mlr.csv", encoding = "UTF-8")

predict_affordable <- makeClassifTask(id = "affordableApartments",
                                      data = dat, target = "tanie")

all_learners <- listLearners()
listLearners("classif", properties = c("prob"))[1L:3, 1L:3]

learnerRF <- makeLearner("classif.randomForest",id = "las losowy", predict.type = "prob")
learnerNN <- makeLearner("classif.nnet", id = "siec neuronowa", predict.type = "prob")

cv_scheme <- makeResampleDesc("CV", iters = 5, stratify = TRUE)

resample(learnerNN, predict_affordable, cv_scheme, measures = list(auc))
resample(learnerRF, predict_affordable, cv_scheme, measures = list(auc))

bench_affordable <- benchmark(learners = list(learnerRF, learnerNN),
                              tasks = predict_affordable,
                              resamplings = cv_scheme,
                              measures = list(auc))

# 1. Przeprowadz walidacje krzyzowa learnerRF (3-foldy)

cv_scheme <- makeResampleDesc("CV", iters = 3, stratify = TRUE)

bench_affordable_cv3 <- benchmark(learners = list(learnerRF, learnerNN),
                              tasks = predict_affordable,
                              resamplings = cv_scheme,
                              measures = list(auc))

# 2. Sprawdz inne metody walidacji modelu np. makeResapleDesc("Holdout", split = 1/2, stratfy = TRUE)

res_scheme <- makeResampleDesc("Holdout", split = 1/2, stratify = TRUE)

bench_affordable_ho <- benchmark(learners = list(learnerRF, learnerNN),
                              tasks = predict_affordable,
                              resamplings = res_scheme,
                              measures = list(auc))


# 3. Przeridz cene mieszkania (regr.randomForest)

predict_price <- makeRegrTask(id = "housePrices",
                              data = dat %>%
                                mutate(., tanie = as.numeric(tanie)) %>%
                                filter(., cena_m2 < 9000),
                              target = "cena_m2")


learnerLM <- makeLearner(id = "Regresja liniowa",
                         "regr.lm",
                         fix.factors.prediction = TRUE )

trn_scheme <- makeResampleDesc("CV", iters = 5, stratify = FALSE)

resample(learnerLM, predict_price, trn_scheme, measures = list(rmse, rsq, mape))


