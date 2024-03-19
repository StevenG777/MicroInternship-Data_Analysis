mtcars

# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	Engine (0 = V-shaped, 1 = straight)
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors
# lm(as.formula(paste("price ~ ",c,sep = "")),data = diamonds)

#Split the data into training set and the testing set
dmds <- resample_partition(mtcars,c(train = 0.8, test = 0.2)) 

#training Set
train_reg <- as_tibble(dmds$train)

#Plot the Engine v.s. the weight
ggplot(mtcars,aes(vs,wt,group=vs)) + 
  geom_boxplot() 

ggplot(mtcars,aes(vs,mpg,group=vs)) + 
  geom_boxplot() 
ggplot(mtcars,aes(vs,fill=as.factor(cyl),group=cyl)) + 
  geom_bar(position="dodge") 


ggplot(mtcars,aes(vs,qsec,group=vs)) + 
  geom_boxplot() 


ggplot(mtcars,aes(x=carb,group=vs,fill=vs)) + 
  geom_bar(position="dodge") 

fit_am <- glm(vs ~ am, data = mtcars, family = "binomial")
summary(fit_am)
#nope

fit_wt <- glm(vs ~ wt, data = mtcars, family = "binomial")
summary(fit_wt)

fit_mpg <- glm(vs ~ mpg, data = mtcars, family = "binomial")
summary(fit_mpg)

fit_cyl <- glm(vs ~ cyl, data = mtcars, family = "binomial")
summary(fit_cyl)

fit_am <- glm(vs ~ am, data = mtcars, family = "binomial")
summary(fit_am)

fit_disp <- glm(vs ~disp, data = mtcars, family = "binomial")
summary(fit_disp)

fit_all <- glm(vs ~disp+mpg+cyl+wt+hp, data = mtcars, family = "binomial")
summary(fit_all)

fit1 <- glm(vs ~ wt + disp+carb, data = mtcars, family = "binomial")
summary(fit1)


##With feature selection
library(leaps)
library(MASS)
base.mod <- lm(vs ~ 1 , data= mtcars)  # base intercept only model
all.mod <- lm(vs ~ ., data= mtcars) # full model with all predictors
# Stepwise regression model
step.model <- stepAIC(all.mod, direction = "both", 
                      trace = FALSE)
sum_logit <- summary(step.model)
colnames(sum_logit$coefficients) <- c("est","std","T","Pr")
#Find the list of significant variable
row.names(as.data.frame(sum_logit$coefficients)%>%
  filter(Pr<=0.05))



df1 <- mtcars%>%
  add_predictions(fit_all, "lpred_all")%>%
  mutate(prob_all=exp(lpred_all)/(1+exp(lpred_all)))


df1 <- df1%>%
  add_predictions(fit1, "lpred_1")%>%
  mutate(prob_1=exp(lpred_1)/(1+exp(lpred_1)))

df1 <- df1%>%
  add_predictions(fit2, "lpred_2")%>%
  mutate(prob_2=exp(lpred_2)/(1+exp(lpred_2)))
auc(df1$vs,df1$prob_2)
#Don't put qsec
