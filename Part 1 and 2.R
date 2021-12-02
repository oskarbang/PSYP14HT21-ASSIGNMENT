#load packages 
library(psych) 
library(car) 	
library(lmtest)
library(sandwich) 
library(boot)
library(lmboot) 
library(tidyverse) 
library(gridExtra)
library(lm.beta)

#costum function ala ZOLTAN THE MAN, THE MYTH, THE LEGEND 
coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

#Load data 
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")

#view data
View(data_sample_1)

#Summary of data 
summary(data_sample_1)
data_sample_1 %>% describe() 

#plots and exploration of data
#sex summary and plot
data_sample_1 %>% 
  select(sex) %>% table()

#sex as factor
data_sample_1 <- data_sample_1 %>% mutate(sex = factor(sex))

class(data_sample_1$sex)
#pain histogram
data_sample_1 %>% 
    ggplot(aes(pain)) + geom_histogram(bin = 20)

#find error code pain and recode 
summary(data_sample_1$pain)
data_sample_1 %>% filter(pain == 55)
#change to correct painscore = 5
data_sample_1$pain[88]=5
 #check change
summary(data_sample_1$pain)

#STAI hitstogram
data_sample_1 %>%  
  ggplot(aes(STAI_trait)) + geom_histogram()
#find and change error in code 
data_sample_1 %>% 
  filter(STAI_trait == 4.2)
#change score to correct STAI score 42.0
data_sample_1$STAI_trait[34]=42.0
#check change
summary(data_sample_1$STAI_trait)

#age histogram
data_sample_1 %>% 
  ggplot(aes(age)) + geom_histogram()

#pain catastrophing histogram
data_sample_1 %>%
  ggplot(aes(pain_cat)) + geom_histogram()

data_sample_1 %>%
    select(pain_cat) %>% summary()

#cortisol serum (blood measure)
data_sample_1 %>% 
  ggplot(aes(cortisol_serum)) + geom_histogram()

data_sample_1 %>% 
    select(cortisol_serum) %>% summary ()

#cortisol saliva 
data_sample_1 %>% 
    ggplot(aes(cortisol_saliva)) + geom_histogram()
data_sample_1 %>% 
    select(cortisol_saliva) %>% summary ()
#mindfulness
data_sample_1 %>% 
    ggplot(aes(mindfulness)) + geom_histogram()
data_sample_1 %>% 
    select(mindfulness) %>% summary ()


#model1. multiple regression, predicting pain from sex and age 
model1 <- lm(pain ~ sex + age, data = data_sample_1)
#model2 
model2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1)

#check models 
summary(model1)
summary(model2)
#cooks distance 
model1 %>% 	
  plot(which = 4)	
model1 %>% 
  plot(which = 5)
#Check the outliers
data_sample_1 %>% 	
  slice(c(8, 23, 47))	

#For model 2 
model2 %>% 	
  plot(which = 4)	
model1 %>% 
  plot(which = 5)
#check for outliers
data_sample_1 %>% 
  slice(c(47, 74, 86))
#Leave outliers for now !!! # doesn't seem to matter 

#assumption checks for regression
#normality 
#for model 1 
model1 %>% 	
  plot(which = 2)	

describe(residuals(model1))

residuals_model1 = 
  enframe(residuals(model1)) 
residuals_model1 %>%
    ggplot() + aes(x = value) + geom_histogram()
#for model 2
model2 %>% 
  plot(which = 2)

describe(residuals(model2))
residuals_model2 = 
  enframe(residuals(model2)) 
residuals_model1 %>%
  ggplot() + aes(x = value) + geom_histogram()

#linearity 
#model 1
model1 %>%
  residualPlots() #sig - altough only small, still ok to use if curve only is small, navarro
#the tuckey test only test for improvments, the curv is looking good
#model 2 
model2 %>%
  residualPlots() #non-sig

#homogeneity of variance 
#plot of standerdized residuals 
model1 %>%plot(which = 3)
#NCV test 
model1 %>%ncvTest()
#plot model2
model2 %>%plot(which = 3)
#NCV test 
model2 %>%ncvTest()# NCV test
# no violations to the assumptions of homogenity of variance

#multicollinearity - uncorrelated predictors 
model1 %>%vif() #ok vif
model2 %>% vif() #theoretically sound to remove saliva from model, since same meassure 
data_sample_1 %>% select(cortisol_serum, cortisol_saliva) %>% cor()

#remove saliva from model2 regression 
model2new <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)

#check assumptions previous assumptions
#cooks distance 
model2new %>% 
  plot(which = 4)
#normality 
model2new %>% 
  plot(which = 2)
#Linearity 
model2new %>% 
  residualPlots() #okay
#homogenity of variance 
model2new %>%
  ncvTest()
#multicolinarity
model2new %>% vif()


#model comparisons
summary(model1)
summary(model2new)

#anova comparing models
anova(model1, model2new)

#AIC
AIC(model1)
AIC(model2new)

#confidence intervalls 
confint(model1)
confint(model2new)



#Part 2 trying all puting all variables in the regression 
#fist looking at the new variables 
#for IQ 
data_sample_1 %>% 
  select(IQ) %>% summary()

data_sample_1 %>% 
  ggplot(aes(IQ)) + geom_histogram(bin = 20)

data_sample_1 %>% 
  ggplot(aes(pain, IQ)) + geom_point()

#for weight 
data_sample_1 %>% 
  select(weight) %>% describe() #person weighing 39 kilos, possible if small and eating disorder

data_sample_1 %>% 
  ggplot(aes(weight)) + geom_histogram(bin = 20)

data_sample_1 %>% 
  ggplot(aes(pain, weight)) + geom_point()

#for houshold income 
data_sample_1 %>% 
  select(household_income) %>% summary()

data_sample_1 %>% 
  ggplot(aes(household_income)) + geom_histogram(bin = 20)

data_sample_1 %>% 
  ggplot(aes(pain, household_income)) + geom_point()

#backwards regression 
#first the regression for all variables 
model_all <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income , data = data_sample_1)
summary(model_all)
#assumptions 
#cooks distance 
model_all %>% 
  plot(which = 4)
data_sample_1 %>% 	
  slice(c(47, 85, 86))	
C#normality 
model_all %>% 
  plot(which = 2)
data_sample_1 %>% 	
  slice(c(86, 85, 104))

describe(residuals(model_all))

residuals_model_all = 
  enframe(residuals(model_all)) 
residuals_model_all %>%
  ggplot() + aes(x = value) + geom_histogram()
#Linearity
model_all %>% 
  residualPlots() 
#homogenity of variance 
model_all %>%
  ncvTest()
#multicolinarity
model_all %>% vif()


#backwards regression 
model_all_back = step(model_all, direction = "backward")

summary(model_all_back)

 backward_model<- lm(formula = pain ~ age + pain_cat + mindfulness + cortisol_serum, 
   data = data_sample_1)
 summary(backward_model)
 
 #theorybased model
 theory_based_model <- model2new
 
 #Check model backwards 
 #cooks distance 
 backward_model %>% 
   plot(which = 4)
 data_sample_1 %>% 	
   slice(c(47, 104, 117))
 
 #Linearity
 backward_model %>% 
   residualPlots() 
 #homogenity of variance 
 backward_model %>%
   ncvTest()
 #multicolinarity
 backward_model %>% vif()
 #normality 
 backward_model %>% 
   plot(which = 2)
 
#model comparisons
summary(theory_based_model)
summary(backward_model)
summary(model_all)
#anova comparing models # don't listen, it is nested!!!. not optimal since not nested!!!!!! should not use for report
anova(theory_based_model, backward_model)

#AIC least point better fit 
AIC(theory_based_model)
AIC(backward_model) #3 points better indicating a better model
AIC(model_all)
#confidence intervalls 
confint(theory_based_model)
confint(backward_model)
#table
coef_table(theory_based_model)
coef_table(model_all)
coef_table(backward_model)

#try on new data
data_sample_2 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")

data_sample_2 <- data_sample_2 %>% mutate(sex = factor(sex))
class(data_sample_2$sex)

summary(data_sample_2) # data looking ok 
describe(data_sample_2)

# for table reporing!!!!!! coef_table()
#predict new data 
#theory based
predicted_pain_theory <- predict(theory_based_model, newdata = data_sample_2)
data_sample_2 <- cbind(data_sample_2, predicted_pain_theory)
#backward based 
predicted_pain_back <-  predict(backward_model, newdata = data_sample_2)
data_sample_2<- cbind(data_sample_2, predicted_pain_back)

#pain next to each other 
colnames(data_sample_2)
data_sample_2_order <- data_sample_2[, c(1, 2, 13, 14, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)]


data_sample_2_order %>% select(pain, predicted_pain_back) %>% cor()
data_sample_2_order %>% select(pain, predicted_pain_theory) %>% cor()



# Find the case-by-case differences
predicted_pain_theory_differences <- with(data_sample_2, pain - predicted_pain_theory)
predicted_pain_back_differences <- with(data_sample_2, pain - predicted_pain_back)

# Calculate mean square errors
#Calculate the mean square error for each model.
#The smaller the mean square error, 
#the closer the model outputs are to the actual response variable.
mean(predicted_pain_theory_differences ^ 2)
mean(predicted_pain_back_differences ^ 2)
sum(predicted_pain_theory_differences ^ 2)
sum(predicted_pain_back_differences ^ 2)
#Save first model
saveRDS(theory_based_model, "theory_based_model.rds")
theory_based_model <- readRDS("theory_based_model.rds")



