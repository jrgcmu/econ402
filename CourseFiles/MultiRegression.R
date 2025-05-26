## ----r setup, include=FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

## ----r------------------------------------------------------------------------
b <- rep(0,1000)
b1 <- rep(0,1000)
for (i in 1:1000) {
  x1 <- rnorm(100)
  x2 <- 1 + 2*x1 + rnorm(100) #x1 is corr'd with x2
  y <- 2 + 2*x1 + 3*x2 + rnorm(100)
  b[i] <- lm(y~x1)$coef[2]
  b1[i] <- lm(y~x1 + x2)$coef[2]
}
mean(b) # By the OVB formula, E(b) = 2 + 3*2 = 8
mean(b1)

## ----r------------------------------------------------------------------------
library(tidyverse)
gril <- read_csv("griliches.csv")
gril$wage <- exp(gril$lw)
model1 <- lm(wage ~ s, data=gril)
summary(model1)

## ----r------------------------------------------------------------------------
model2 <- lm(iq ~ s, data=gril)
summary(model2)

## ----r------------------------------------------------------------------------
model3 <- lm(wage ~ s + iq, data=gril)
summary(model3)

## ----r------------------------------------------------------------------------
model1 <- lm(wage ~ s + iq, data=gril)
summary(model1)

## ----r------------------------------------------------------------------------
yresid <- lm(wage ~ iq, data=gril)$resid
xresid <- lm(s ~ iq, data=gril)$resid
reg.anatomy <- lm(yresid ~ xresid)
summary(reg.anatomy)

## ----r, echo=F----------------------------------------------------------------
x <- seq(0, 5, by=.1)
y <- df(x, 2, 100)
plot(x,y, type="l")
x2 <- seq(3.087, 5, length.out=50)
y <- df(x2, 2, 100)
polygon(c(x2, 3.087, 3.087), c(y, 0, df(3.087, 2, 100)), col = adjustcolor("blue", alpha.f=.75), border = NA)
mtext(expression("F"[.05]^"(2,100)"~"=3.087"), 1, line=3, at=3.087)

## ----r------------------------------------------------------------------------
model.u <- lm(wage ~ s + expr + tenure, data=gril)
model.r <- lm(wage ~ s, data=gril)
ssr.u <- sum(model.u$residuals^2)
ssr.r <- sum(model.r$residuals^2)
f.stat <- ((ssr.r - ssr.u)/2)/(ssr.u/754)
f.stat

## ----r------------------------------------------------------------------------
library(car)
linearHypothesis(model.u, c("expr = 0", "tenure = 0"))

## ----r------------------------------------------------------------------------
summary(model.u)

## ----r------------------------------------------------------------------------
gril$z <- gril$expr + gril$tenure
model.r2 <- lm(wage ~ s + z, data=gril)
ssr.r2 <- sum(model.r2$resid^2)
F.2 <-((ssr.r2 - ssr.u)/1) / (ssr.u /754) 
F.2
qf(.05, 1, 754, lower.tail=FALSE)

## ----r------------------------------------------------------------------------
linearHypothesis(model.u, c("expr = tenure"))

## ----r------------------------------------------------------------------------
library(haven)
mur <- read_dta("MURDER.dta")

## ----r------------------------------------------------------------------------
summary(mur)

## ----r------------------------------------------------------------------------
ggplot(data=mur, aes(x=exec, y=mrdrte)) + geom_point() 

## ----r------------------------------------------------------------------------
ggplot(data=mur, aes(x=exec, y=mrdrte)) + geom_point() + geom_smooth(method='lm')

## ----r------------------------------------------------------------------------
mur.mod1 <- lm(mrdrte ~ exec, data=mur)
summary(mur.mod1)

## ----r------------------------------------------------------------------------
mur.mod2 <- lm(mrdrte ~ exec + unem, data=mur)
summary(mur.mod2)

## ----r------------------------------------------------------------------------
mur.mod3 <- lm(mrdrte ~ exec + unem + as.factor(year), data=mur)
summary(mur.mod3)

## ----r------------------------------------------------------------------------
mur.mod4 <- lm(mrdrte ~ exec + unem + as.factor(year) + as.factor(state), data=mur)
summary(mur.mod4)

## ----r------------------------------------------------------------------------
att <- read_dta("ATTEND.dta")
summary(att)

## ----r------------------------------------------------------------------------
ggplot(data=att, aes(x=attend, y=termGPA)) + geom_point() + geom_smooth(method='lm')

## ----r------------------------------------------------------------------------
att.mod1 <- lm(termGPA ~ attend, data=att)
summary(att.mod1)

## ----r------------------------------------------------------------------------
att.mod2 <- lm(termGPA ~ attend + ACT + priGPA, data=att)
summary(att.mod2)

## ----r------------------------------------------------------------------------
library(car)
linearHypothesis(att.mod2, c("attend = ACT"))

## ----r------------------------------------------------------------------------
library(texreg)
screenreg(list(att.mod1, att.mod2))

## ----r------------------------------------------------------------------------
library(psych)
describe(att, fast=TRUE, skew=FALSE)

## ----r------------------------------------------------------------------------
describe(att ~ frosh, fast=T, skew=F)

