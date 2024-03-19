
library(ggplot2)
library(cowplot)
library(pROC)
## NOTE: The data used in this demo comes from the UCI machine learning
## repository.
## http://archive.ics.uci.edu/ml/index.php
## Specifically, this is the heart disease data set.
## http://archive.ics.uci.edu/ml/datasets/Heart+Disease

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data <- read.csv(url, header=FALSE)

colnames(data) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain
  # 1 = typical angina,
  # 2 = atypical angina,
  # 3 = non-anginal pain,
  # 4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
  # 1 = normal
  # 2 = having ST-T wave abnormality
  # 3 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment
  # 1 = upsloping
  # 2 = flat
  # 3 = downsloping
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
  # 3 = normal (no cold spots)
  # 6 = fixed defect (cold spots during rest and exercise)
  # 7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)

head(data) # now we have data and column names


## First, convert "?"s to NAs...
data[data == "?"] <- NA

## Now add factors for variables that are factors and clean up the factors
## that had missing data...
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)
data$sex <- as.factor(data$sex)

data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca) # since this column had "?"s in it
# R thinks that the levels for the factor are strings, but
# we know they are integers, so first convert the strings to integers...
data$ca <- as.factor(data$ca)  # ...then convert the integers to factor levels

data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)


## Now determine how many rows have "NA" (aka "Missing data"). If it's just
## a few, we can remove them from the dataset, otherwise we should consider
## imputing the values with a Random Forest or some other imputation method.
nrow(data[is.na(data$ca) | is.na(data$thal),])
data[is.na(data$ca) | is.na(data$thal),]
## so 6 of the 303 rows of data have missing values. This isn't a large
## percentage (2%), so we can just remove them from the dataset
## NOTE: This is different from when we did machine learning with
## Random Forests. When we did that, we imputed values.
nrow(data)
data <- data[!(is.na(data$ca) | is.na(data$thal)),]
nrow(data)

## This next line replaces 0 and 1 with "Healthy" and "Unhealthy"
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")

#Create a new column which is binary
data$hd_binary <- ifelse(data$hd == "Healthy", 0,1)
data$hd <- as.factor(data$hd) # Now convert to a factor
#Split the data into training set and the testing set

dmds <- resample_partition(data,c(train = 0.8, test = 0.2)) 

#training Set
train_reg <- as_tibble(dmds$train)
train_reg1 <-train_reg 


library(leaps)
library(MASS)
all.mod <- lm(hd_binary ~ .-hd, data= train_reg) # full model with all predictors

# Stepwise regression model
step.model <- stepAIC(all.mod, direction = "both", 
                      trace = FALSE)


#Plot the chest pain v.s. status
ggplot(train_reg,aes(x=hd,fill=cp)) + 
  geom_bar(position = "dodge")

#Plot the sex v.s. status
ggplot(train_reg,aes(x=hd,fill=sex)) + 
  geom_bar(position = "dodge")
#We will love to put the sex in the model

#Plot the resting blood pressure v.s. status
ggplot(train_reg,aes(x=hd,y=trestbps)) + 
  geom_boxplot()

#Plot the age v.s. statu
ggplot(train_reg,aes(x=hd,y=age)) + 
  geom_boxplot()

#Stepwise selection
library(leaps)
library(MASS)
base.mod <- lm(hd_binary ~ -1 , data= train_reg)  # base intercept only model
all.mod <- lm(hd_binary ~ .-hd-1, data= train_reg) # full model with all predictors
# Stepwise regression model
step.model <- stepAIC(all.mod, direction = "both", 
                      trace = FALSE)


train_reg_trans <- model_matrix(train_reg, hd ~ .-1)
train_reg_trans$hd <- train_reg$hd


sum_logit <- summary(step.model)
sum_logit
#Rename the column for an easier retrieval
colnames(sum_logit$coefficients) <- c("est","std","T","Pr")
b <- row.names(as.data.frame(sum_logit$coefficients)%>%
            filter(Pr<=0.05))


#B is a list of significant variable ins 
(c <- paste(b, collapse = "+"))
fit_logit <- glm(data=train_reg_trans,as.formula(paste("hd ~ ",c,sep = "")),family = "binomial")


fit_logit <- glm(data=train_reg_trans,hd~sexF+XXX+XXX+GGG,family = "binomial")
summary(fit_logit)
auc(train_reg_trans$hd,fit_logit$fitted.values)
# train_reg <- train_reg%>%
#   add_predictions(fit1, "lpred")%>%
#   mutate(prob=exp(lpred)/(1+exp(lpred)),hd_binary=ifelse(hd=="Healthy",0,1))

train_reg1 <- train_reg
head(train_reg1)
vars <- colnames(train_reg1)
#Variable transformation 
vars=vars[vars!="hd" & vars!="hd_binary" ]
#Interact every variable with others
for (i in vars)  {
  vars <- vars[vars!=i]
  for (j in vars) {
    if (i != j) {
      eval(parse(text=paste0("train_reg1 <- train_reg1%>%mutate(",i,"_",j,"=","as.numeric(",i,")*as.numeric(",j,"))")))
    }
  }
}
for (i in vars)  {
  vars <- vars[vars!=i]
  for (j in vars) {
      eval(parse(text=paste0("train_reg1 <- train_reg1%>%mutate(",i,"_",j,"=","as.numeric(",i,")*as.numeric(",j,"))")))
  
  }
}


colnames(train_reg1)
base.mod <- lm(hd_binary ~ -1 , data= train_reg1)  # base intercept only model
all.mod <- lm(hd_binary ~ .-hd-1, data= train_reg1) # full model with all predictors
# Stepwise regression model
step.model <- stepAIC(all.mod, direction = "both", 
                      trace = FALSE)

sum_logit <- summary(step.model)
sum_logit
#Rename the column for an easier retrieval
colnames(sum_logit$coefficients) <- c("est","std","T","Pr")
b <- row.names(as.data.frame(sum_logit$coefficients)%>%
                 filter(Pr<=0.05))

(c <- paste(b, collapse = "+"))
fit_logit <- glm(data=train_reg_trans,as.formula(paste("hd ~ ",c,sep = "")),family = "binomial")
summary(fit_logit)
auc(train_reg_trans$hd,fit_logit$fitted.values)
# fitLN <- lm(data=train_reg1,hd_binary~.-hd)
# summary(fitLN)
# 
# fitLN <- lm(data=train_reg,hd_binary~.-hd)
# summary(fitLN)


