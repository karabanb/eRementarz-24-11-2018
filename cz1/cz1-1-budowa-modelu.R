library(mlr)
library(dplyr)

set.seed(15390)

mieszkania <- read.csv(file = "https://raw.githubusercontent.com/STWUR/STWUR-2017-06-07/master/data/mieszkania_dane.csv", 
                       encoding = "UTF-8") %>% 
  na.omit

predict_price <- makeRegrTask(id = "price", 
                              data = mieszkania, target = "cena_m2")

learnerRF <- makeLearner("regr.ranger")

cv_scheme <- makeResampleDesc("CV", iters = 5)

resample(learnerRF, predict_price, cv_scheme)

parameters_set <- makeParamSet(
  makeIntegerParam("num.trees", lower = 400, upper = 1000),
  makeIntegerParam("min.node.size", lower = 1, upper = 5)
)

optimal_rf <- tuneParams(learnerRF, predict_price, cv_scheme, 
                         par.set = parameters_set, control = makeTuneControlGrid(resolution = 3L))

library(ggplot2)

generateHyperParsEffectData(optimal_rf)[["data"]] %>% 
  ggplot(aes(x = num.trees, y = min.node.size, 
             fill = sqrt(mse.test.mean), label = round(sqrt(mse.test.mean), 2))) +
  geom_tile() +
  geom_label(fill = "white") +
  theme_bw()


best_par <- data.frame(optimal_rf[["opt.path"]]) %>% 
  filter(mse.test.mean == min(mse.test.mean))

worst_par <- data.frame(optimal_rf[["opt.path"]]) %>% 
  filter(mse.test.mean == max(mse.test.mean))

model_best <- ranger(formula = cena_m2 ~ ., data = mieszkania, num.trees = best_par[["num.trees"]],
                     min.node.size = best_par[["min.node.size"]])

model_worst <- ranger(formula = cena_m2 ~ ., data = mieszkania, num.trees = worst_par[["num.trees"]],
                      min.node.size = worst_par[["min.node.size"]])

library(auditor)

audit_best <- audit(model_best, data = mieszkania, y = mieszkania[["cena_m2"]], label = "best", 
                    predict.function = function(m, data) predict(m, data)[["predictions"]])

audit_worst <- audit(model_worst, data = mieszkania, y = mieszkania[["cena_m2"]], label = "worst", 
                     predict.function = function(m, data) predict(m, data)[["predictions"]])

plotModelCorrelation(audit_best, audit_worst)
plotResidualDensity(audit_best, audit_worst)
plotScaleLocation(audit_best, audit_worst)
plotTwoSidedECDF(audit_best, audit_worst)


save(model_best, model_worst, file = "./cz2/models.RData")
