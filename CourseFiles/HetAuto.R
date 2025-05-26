## ----r setup, include=FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

## ----r------------------------------------------------------------------------
set.seed(12345)
t <- rep(0,1000)
t2 <- rep(0,1000)
for (i in 1:1000) {
  x <- rnorm(100)
  e <- rnorm(100)
  y <- 1 + 2*x + e # homoskedastic
  y2 <- 1 + 2*x + 2*x*e # heteroskedastic
  b <- lm(y ~ x)$coef[2]
  se <- sqrt(vcov(lm(y ~ x))[2,2])
  b2 <- lm(y2 ~ x)$coef[2]
  se2 <- sqrt(vcov(lm(y2 ~ x))[2,2])
  t[i] <- (b-2)/se
  t2[i] <- (b2-2)/se2
}
mean(abs(t)>1.96)
mean(abs(t2)>1.96)

## ----r------------------------------------------------------------------------
library(tidyverse)
gril <- read_csv("griliches.csv")
gril$w <- exp(gril$lw)
model <- lm(w ~ s, data=gril)
summary(model)

## ----r------------------------------------------------------------------------
library(sandwich)
library(lmtest)
coeftest(model, vcov = vcovHC(model, type="HC1"))

## ----r------------------------------------------------------------------------
set.seed(12345)
t <- rep(0,1000)
t2 <- rep(0,1000)
t3 <- t2
for (i in 1:1000) {
  x <- rnorm(100)
  e <- rnorm(100)
  y <- 1 + 2*x + e # homoskedastic
  y2 <- 1 + 2*x + 2*x*e # heteroskedastic
  b <- lm(y ~ x)$coef[2]
  se <- sqrt(vcov(lm(y ~ x))[2,2])
  b2 <- lm(y2 ~ x)$coef[2]
  se2 <- sqrt(vcov(lm(y2 ~ x))[2,2])
  t[i] <- (b-2)/se
  t2[i] <- (b2-2)/se2
  se3 <- sqrt(vcovHC(lm(y2 ~ x), type="HC1")[2,2])
  t3[i] <- (b2-2)/se3
}

## ----r------------------------------------------------------------------------
mean(abs(t)>1.96)
mean(abs(t2)>1.96)
mean(abs(t3)>1.96)

## ----r------------------------------------------------------------------------
gril$e2 <- model$residuals^2
r2 <- summary(lm(e2 ~ s + I(s^2), data=gril))$r.squared
(white.test.stat <- 758*r2)
qchisq(p=.05, df=2, lower.tail=FALSE) # critical value
pchisq(white.test.stat, df=2, lower.tail=FALSE)

## ----r------------------------------------------------------------------------
library(whitestrap)
white_test(model)

## ----r echo=FALSE-------------------------------------------------------------
set.seed(12345)
x0 <- rnorm(200)
x <- .95*dplyr::lag(x0) + x0
x[1] <- x0[1]
plot(1:200, x, type="l")

## ----r echo=FALSE-------------------------------------------------------------
x0 <- rnorm(200)
x <- -.95*dplyr::lag(x0) + x0
x[1] <- x0[1]
plot(1:200, x, type="l")

## ----r------------------------------------------------------------------------
library(lmtest)
library(sandwich)
e0 <- rnorm(1000)
e <- .25*dplyr::lag(e0) + e0
e[1] <- e0[1]
x0 <- rnorm(1000)
x <- .25*dplyr::lag(x0) + x0
x[1] <- x0[1]
y <- 1 + 2*x + e

## ----r------------------------------------------------------------------------
summary(lm(y ~ x))

## ----r------------------------------------------------------------------------
coeftest(lm(y ~ x), vcov=vcovHAC)

## ----r------------------------------------------------------------------------
set.seed(12345)
t <- rep(0,1000)
t2 <- t
for (j in 1:1000) {
  e0 <- rnorm(1000)
  e <- .25*dplyr::lag(e0) + e0
  e[1] <- e0[1]
  x0 <- rnorm(1000)
  x <- .25*dplyr::lag(x0) + x0
  x[1] <- x0[1]
  y <- 1 + 2*x + e
  b <- lm(y ~ x)$coef[2]
  se <- sqrt(vcov(lm(y ~ x))[2,2])
  t[j] <- (b - 2)/se
  se2 <- sqrt(vcovHAC(lm(y ~ x))[2,2])
  t2[j] <- (b - 2)/se2
}
mean(abs(t)>1.96)
mean(abs(t2)>1.96)

## ----r------------------------------------------------------------------------
library(haven)
mur <- read_dta("MURDER.dta")
mur.mod1 <- lm(mrdrte ~ exec + unem, data=mur)
summary(mur.mod1) # default SEs

## ----r------------------------------------------------------------------------
coeftest(mur.mod1, vcov=vcovCL(mur.mod1, cluster = ~state))

