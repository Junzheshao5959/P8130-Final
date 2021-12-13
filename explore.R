library(tidyverse)
library(data.table)
cid_dt = read_csv('cdi.csv')
cid_dt = cid_dt %>% mutate(region = factor(region))
cid_dt %>% select(-c(id,cty,state,crimes,region)) %>%
    pivot_longer(cols = everything(), names_to = "variables", values_to = "value") %>%
    ggplot(aes(factor(variables), value)) +geom_boxplot() + facet_wrap(~variables, scale="free")
x = "area"
cid_dt = as.data.table(cid_dt)
for (x in colnames(cid_dt %>% select(-c(id,cty,state,crimes,region)))){
  print(x)
  cid_dt = cid_dt[!(cid_dt[[x]] %in% boxplot.stats(cid_dt[[x]])$out)]
}
cid_dt = as_tibble(cid_dt)
cid_dt %>% select(-c(id,cty,state,crimes,region)) %>%
  pivot_longer(cols = everything(), names_to = "variables", values_to = "value") %>%
  ggplot(aes(factor(variables), value)) +geom_boxplot() + facet_wrap(~variables, scale="free")
set.seed(8130)
validation_dt = cid_dt %>% sample_frac(0.1)
tt_dt = cid_dt %>% filter(!(id %in% validation_dt$id))

tt_dt %>% select(-c(id,cty,state,crimes,region)) %>% as.data.frame %>% plot(pch=20 , cex=1.5 , col="#69b3a2")

set.seed(8130)
train_dt = tt_dt %>% sample_frac(0.7)
test_dt = tt_dt %>% filter(!(id %in% train_dt$id))

t_select = train_dt %>% select(-c(id,cty,state,pop,totalinc))
intercept_only <-  lm(crimes ~ 1, data = t_select)

#define model with all predictors
all <- lm(crimes ~ ., data = t_select)

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)
forward$anova
forward$coefficients
test_dt$pred_crime = predict(all, test_dt)
test_dt$residual = test_dt$pred_crime -test_dt$crimes
test_dt %>% plot(residual ~ crimes, data =.)
mean(test_dt$residual^2)
car::vif(all)
