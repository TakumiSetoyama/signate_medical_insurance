csdat = read.csv("./data/train.csv")

csdat$sex = ifelse(csdat$sex=="female", 1, 0)
csdat$smoker = ifelse(csdat$smoker == "yes", 1, 0)
csdat$region_se = ifelse(csdat$region == "southeast", 1, 0)
csdat$region_sw = ifelse(csdat$region == "southwest", 1, 0)
csdat$region_ne = ifelse(csdat$region == "northeast", 1, 0)

library(nnet)
model = multinom(charges~age+sex+bmi+children+smoker, data=csdat)

summary(model)
library(dplyr)
csdat = mutate(csdat, pred = predict(model))

sum(csdat$charges == csdat$pred)/nrow(csdat)

library(MLmetrics)
F1_Score(csdat$charges, csdat$pred, positive = NULL)
                