setwd("~/Documents/master/1.2/R")
D=read.spss("GIRAFH.sav", to.data.frame=TRUE)


########### imputeren
install.packages("mice")
install.packages("table1")

library(mice)
imputed_data = mice(D)
data <- complete(imputed_data)
with(....)
pool(...)

######### table 1
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

table1(~sex + height + weight + bmi + alcoholuse + smoking + systbp + diasbp + hypertension 
       + Glucose + Hba1c + diabetes + familiarHC + Tc + HDL + Tg + Lpa + homocysteine + 
         creatinine + age + event|event,data = data,overall = F, extra.col=list("P-value"=pvalue))

######### regressie 
install.packages("tidyverse")
install.packages("caret")
install.packages("leaps")
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



summary(glm(event ~ sex + height + weight + bmi + alcoholuse + smoking + systbp + diasbp + hypertension 
    + Glucose + Hba1c + diabetes + familiarHC + Tc + HDL + Tg + Lpa + homocysteine + 
      creatinine + age, family = "binomial", data = data))

install.packages("rms")
library(rms)
model.correct = lrm(event~sex + alcoholuse + smoking + hypertension + Glucose + diabetes + familiarHC + HDL + Lpa, data = data)

