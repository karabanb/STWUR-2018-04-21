
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
                              data = dat %>% mutate(., tanie = as.numeric(tanie)),
                              target = "cena_m2")


learnerLM <- makeLearner(id = "Regresja liniowa",
                         "regr.randomForest",
                         fix.factors.prediction = TRUE )



trn_scheme <- makeResampleDesc("CV", iters = 5, stratify = FALSE)

regr_res <- resample(learnerLM, predict_price, trn_scheme, measures = list(rmse, rsq, mape))


## cz2. - strojenia

getParamSet("classif.nnet")
getParamSet("classif.randomForest")

learnerNN2 <- makeLearner("classif.nnet", predict.type = "prob", size = 5, decay = .2)

parametrs_set <- makeParamSet(
  makeIntegerParam("size", lower = 1, upper = 15),
  makeNumericParam("decay", -5, 5, trafo = function(x) 2^x)
)

library(mlrMBO)

mbo_ctrl <- makeTuneControlMBO(mbo.control = setMBOControlTermination(makeMBOControl(), iters = 2))
optimal_nnet <- tuneParams(makeLearner("classif.nnet", predict.type = "prob"), predict_affordable,
                           cv_scheme, par.set = parametrs_set, measures = list(auc), control = mbo_ctrl)


benchmark(learners = list(makeLearner("classif.nnet", id = "nonoptimal", predict.type = "prob"),
                          makeLearner("classif.nnet", id ="optimal", predict.type = "prob", par.vals = optimal_nnet),
                          makeLearner("classif.randomForest", predict.type = "prob")),
                     tasks = predict_affordable,
                     resamplings = cv_scheme,
                     measures = list(auc))

regr_task <- makeRegrTask(id = "affordableAprtaments", data = dat[ , -8], target = "cena_m2")

getParamSet("classif.randomForest")

parameters_set_RF <- makeParamSet(
  makeIntegerParam("ntree", lower = 100, upper = 1000),
  makeIntegerParam("mtry", lower = 1, upper = 5)
)

mbo_ctr <- makeTuneControlMBO(mbo.control = setMBOControlTermination(makeMBOControl(), iters = 2))
optimal_rf <- tuneParams(makeLearner("regr.randomForest"), regr_task, makeResampleDesc("CV", iters = 3),
                         par.set = parameters_set_RF, control = mbo_ctr,
                         show.info = TRUE)
