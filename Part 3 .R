library(psych) 
library(car) 	
library(lmtest)
library(sandwich) 
library(boot)
library(lmboot) 
library(tidyverse) 
library(gridExtra)
library(lm.beta)
library(cAIC4) # for cAIC\t
library(r2glmm) # for r2beta\t
library(MuMIn) # for r.squaredGLMM
library(influence.ME) # for influence (this will also load the lme4 package)
library(lattice) # for qqmath
library(lme4) # for mixed models
library(lmerTest) # for significance test on lmer() mixed models

#custum function ala Zoltan for standerdized beta coef
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}


#Load old model 
theory_based_model <- readRDS("theory_based_model.rds")

#load data 
data_sample_3 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_3.csv")
data_sample_4 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_4.csv")

#sex as factor 
data_sample_3 <- data_sample_3 %>% mutate(sex = factor(sex))
class(data_sample_3$sex)
data_sample_4 <- data_sample_4 %>% mutate(sex = factor(sex))
class(data_sample_4$sex)

#hospital as factor
data_sample_3 <- data_sample_3 %>% mutate(hospital = factor(hospital))
class(data_sample_3$hospital)
summary(data_sample_3$hospital)
data_sample_4 <- data_sample_4 %>% mutate(hospital = factor(hospital))
class(data_sample_4$hospital)
summary(data_sample_4$hospital)

#check data
summary(data_sample_3)
describe(data_sample_3)
summary(data_sample_4)
describe(data_sample_4)

#data sample 3 recode n errors
#change woman to female row 25 
data_sample_3$sex <- recode_factor(data_sample_3$sex, woman = "female"
                              )
summary(data_sample_3$sex)
#householdincome. change or delete- check plot minus income. - keep since it is possible to ha negative income if having big debts 
data_sample_3 %>% 
  ggplot(aes(household_income)) + geom_histogram()
data_sample_3 %>% slice(2)

theory_based_model




#regression model with hosptial as random intercept
hospital_random = lmer(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = data_sample_3)
hospital_random # check 

#calculate residuals  
data_sample_3 = data_sample_3 %>%
  mutate(resid = residuals(hospital_random))

random_effects = as.data.frame(ranef(hospital_random)[[1]])
names(random_effects) = c("intercept")

#checking model 
#influental outliers
influence_observation = influence(hospital_random, obs = T)$alt.fixed # this can take a minute or so
influence_group = influence(hospital_random, group = "hospital")$alt.fixed

#plot of outliers # LOOKING OK, no outliers 
data_plot_inflience = as_tibble(influence_group) %>%
  gather(colnames(influence_group), value = coefficient, key = predictor)
data_plot_inflience %>%
  ggplot() + aes(x = 1, y = coefficient, group = predictor) +
  geom_violin() + geom_jitter(width = 0.2) + facet_wrap(~predictor,
                                                        scales = "free")

#normality check 
qqmath(hospital_random, id = 0.05)

data_sample_3 %>%
  ggplot() + aes(sample = resid) + stat_qq() + stat_qq_line() +
  facet_wrap(~hospital, scales = "free")
#normality of random effects 
qqmath(ranef(hospital_random))

random_effects %>%
  ggplot() + aes(sample = intercept) + stat_qq() + stat_qq_line()

describe(random_effects$intercept)$skew #ok

describe(random_effects$intercept)$kurtosis #ok

#linearity 
plot(hospital_random, arg = "pearson")

#homoscadacity check 
homosced_mod = lm(resid^2 ~ hospital, data = data_sample_3)
summary(homosced_mod) #non-sig aka looking good 
hospital_random
#check for multicolinarity - non correlation
pairs.panels(data_sample_3[, c("sex", "age",
                                 "STAI_trait", "pain_cat", "mindfulness", "cortisol_serum")], col = "red", lm = T)


#model
hospital_random
confint(hospital_random)
stdCoef.merMod(hospital_random)
summary(hospital_random)
#comparing with theory based model
summary(theory_based_model)
confint(theory_based_model)
coef_table(theory_based_model)
stdCoef.merMod(theory_based_model)
#
cAIC(hospital_random) #better for random intercept- double check
AIC(hospital_random)


# marginal R squared with confidence intervals
r2beta(hospital_random, method = "nsj", data = data_sample_3)

# marginal and conditional R squared values
r.squaredGLMM(hospital_random)
0.4632079-0.3852492
#predict pain in data4 with hospital_random 
pain_predicted_mixed <-  predict(hospital_random, newdata = data_sample_4, allow.new.levels = TRUE)
data_sample_4 <- cbind(data_sample_4, pain_predicted_mixed)


#variance explained  #google modmean, using the data_sample_3 its a thing.
#TSS #calculated using the data sample 4. 
TSS <- sum((data_sample_4$pain - mean(data_sample_4$pain))^2)
TSS
#RSS
predicted_pain_mixed_differences <- with(data_sample_4, pain - pain_predicted_mixed)
RSS <- sum(predicted_pain_mixed_differences ^ 2)
R2 = 1 - (RSS/TSS)
R2

#compare r2 with the marginal r2 
#R2 for model on data 4
R2
# marginal R squared with confidence intervals
r2beta(hospital_random, method = "nsj", data = data_sample_3)

# marginal and conditional R squared values
r.squaredGLMM(hospital_random) #the conditional is higher because it takes into 
#accont the random intercept ie hospital. 
#the R2 will be closer to the contidtionalr2, 
#basically saying that r2, marignal r2 will be better estimate on new data
#it does not include the hosptial error


#Build a new linear mixed effects model on dataset 3 predicting pain. 
#However, instead of including all predictors, 
#you should only include the most influential predictor from the previous model.
#Allow for both random intercept and random slope.
#Now visualize the fitted regression lines for each hospital separately.

#new model with the most influental predictor aka blood cortisol°!!!random intercept and random slope!!!1
cortisol_hospital_random = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = data_sample_3)
?isSingular
isSingular(cortisol_hospital_random)
#plot each line for hospital separatley
data_sample_3 = data_sample_3 %>%
  mutate(pred_slope = predict(cortisol_hospital_random))
#plot
data_sample_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)






