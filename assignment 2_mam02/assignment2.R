library("foreign")

D=read.spss("GIRAFH.sav", to.data.frame=TRUE)
D$alcoholuse = as.factor(as.character(D$alcoholuse))
table(D$alcoholuse)
library("mice")
imputed_d=mice(D)
data <- complete(imputed_d)

summary(D)

#install.packages("table1")
library("table1")

########TABLES#######
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

######### Single regressie 
#install.packages("tidyverse")
#install.packages("caret")
#install.packages("leaps")
library(tidyverse)
library(caret)
library(leaps)

sex <- summary(glm(event ~ sex, family = "binomial", data = D, na.action = na.omit))
height <- summary(glm(event ~ height, family = "binomial", data = D, na.action = na.omit))
weight <- summary(glm(event ~ weight, family = "binomial", data = D, na.action = na.omit))
bmi <- summary(glm(event ~ bmi, family = "binomial", data = D, na.action = na.omit))
alcoholuse <- summary(glm(event ~ alcoholuse, family = "binomial", data = D, na.action = na.omit))
smoking <- summary(glm(event ~ smoking, family = "binomial", data = D, na.action = na.omit))
systbp <- summary(glm(event ~ systbp, family = "binomial", data = D, na.action = na.omit))
diasbp <- summary(glm(event ~ diasbp, family = "binomial", data = D, na.action = na.omit))
hyper <- summary(glm(event ~ hypertension, family = "binomial", data = D, na.action = na.omit))
glucose <- summary(glm(event ~ Glucose, family = "binomial", data = D, na.action = na.omit))
hba1c <- summary(glm(event ~ Hba1c, family = "binomial", data = D, na.action = na.omit))
diabetes <- summary(glm(event ~ diabetes, family = "binomial", data = D, na.action = na.omit))
familiar <-summary(glm(event ~ familiarHC, family = "binomial", data = D, na.action = na.omit))
tc <- summary(glm(event ~ Tc, family = "binomial", data = D, na.action = na.omit))
hdl <- summary(glm(event ~ HDL, family = "binomial", data = D, na.action = na.omit))
tg <- summary(glm(event ~ Tg, family = "binomial", data = D, na.action = na.omit))
lpa <- summary(glm(event ~ Lpa, family = "binomial", data = D, na.action = na.omit))
homocysteine <- summary(glm(event ~ homocysteine, family = "binomial", data = D, na.action = na.omit))
creatinine <-summary(glm(event ~ creatinine, family = "binomial", data = D, na.action = na.omit))
age <- summary(glm(event ~ age, family = "binomial", data = D, na.action = na.omit))

###### Multi variable
table1(~sex + height + weight + bmi + alcoholuse + smoking + systbp + diasbp + hypertension 
       + Glucose + Hba1c + diabetes + familiarHC + Tc + HDL + Tg + Lpa + homocysteine + 
         creatinine + age + event|event,data = data, overall = F, extra.col = list('P-value' = pvalue))

model.full <- lrm(event~.,data = data, x=TRUE,y=TRUE)
model.full <- glm(event~.,data = data,family="binomial")
summary(model.full)
with(summary(model.full), 1 - deviance/null.deviance)
############################IGNORE##################################################
library(MASS)
stepAIC(model.full,direction = 'backward')
############################IGNORE##################################################

#######################CORRECTE MODEL###############################################
model.test= lrm(event~sex+height+weight+bmi+smoking+systbp+diasbp+hypertension+Glucose+Hba1c+diabetes+familiarHC+Tc+HDL+Tg+Lpa+homocysteine+creatinine+age,data = data, x=TRUE, y=TRUE)
fastbw(model.full)
model.correct = lrm(event~sex+weight+bmi+smoking+hypertension+Glucose+Hba1c+diabetes+familiarHC+Tc+HDL+Lpa+homocysteine,data = data, x=TRUE, y=TRUE)
summary(model.correct)
library(rms)
fastbw(model.correct)
plot(calibrate(model.correct,B=1000))
validate(model.correct, method = "boot", B=1000)
#######################CORRECTE MODEL###############################################
#############################Regression#############################
##### Create subset
subset_d <- subset(data, select = c("event","sex", "alcoholuse", "smoking","hypertension","Glucose","diabetes","familiarHC","HDL","Lpa")) 


set.seed(1234)
train.smp.size <- round(2/3 * nrow(subset_d))
test.smp.size <- round(1/3 * nrow(subset_d))

##### Random column
train.ind <- sample(seq_len(nrow(subset_d)), size = train.smp.size)

##### Seperate to train
data.train <- subset_d[train.ind, ]

##### To test
data.test <- subset_d[-train.ind, ]



model.train <- lrm(data.train$event~data.train$sex + data.train$alcoholuse + data.train$smoking + data.train$hypertension + data.train$Glucose + data.train$diabetes + data.train$familiarHC + data.train$HDL + data.train$Lpa, family="binomial")
summary(model.train)







