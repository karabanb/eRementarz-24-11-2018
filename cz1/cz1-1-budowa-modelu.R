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


opt_dat <- data.frame(optimal_rf[["opt.path"]]) %>% 
  filter(mse.test.mean == min(mse.test.mean))

model_rf <- train(makeLearner("regr.ranger", 
                              num.trees = opt_dat[["num.trees"]],
                              min.node.size = opt_dat[["min.node.size"]]), 
                  predict_price)
