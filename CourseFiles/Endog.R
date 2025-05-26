## ----r setup, include=FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

## ----r------------------------------------------------------------------------
library(tidyverse)
library(ivreg)
b <- rep(0,1000)
b.iv <- b
for (i in 1:1000) {
  z <- rnorm(1000)
  u <- rnorm(1000)
  x <- .5*z + .25*u + rnorm(1000) # x deps on z and u
  e <- u + rnorm(1000) # e deps on u
  y <- 1 + 2*x + e
  b[i] <- lm(y ~ x)$coef[2]
  b.iv[i] <- ivreg(y ~ x | z)$coefficients[2]
}
bs <- data.frame(b=c(b, b.iv))
bs$est <- factor(c(rep("ols", 1000), rep("iv",1000)), level=c("ols", "iv"))

## ----r------------------------------------------------------------------------
ggplot(bs, aes(b)) + geom_histogram() + facet_wrap("factor(est)") + geom_vline(xintercept=2)

## ----r------------------------------------------------------------------------
z <- rnorm(1000)
u <- rnorm(1000)
x <- .5*z + .25*u + rnorm(1000) # x deps on z and u
e <- u + rnorm(1000) # e deps on u
y <- 1 + 2*x + e
x.hat <- lm(x ~ z)$fitted.values
summary(lm(y ~ x.hat))

## ----r------------------------------------------------------------------------
summary(ivreg(y ~ x | z))

## ----r------------------------------------------------------------------------
chx <- read_csv("broiler.csv")
summary(chx)

## ----r------------------------------------------------------------------------
model.1 <- lm(Q ~ PCHICK + Y, data=chx)
summary(model.1)

## ----r------------------------------------------------------------------------
model.2 <- ivreg(Q ~ PCHICK + Y  |  PF + Y, data=chx)
summary(model.2)

## ----r------------------------------------------------------------------------
card <- read_csv("card.csv")
summary(card)

## ----r------------------------------------------------------------------------
model.1 <- lm(wage ~ educ, data=card)
summary(model.1)

## ----r------------------------------------------------------------------------
model.2 <- lm(wage ~ educ + exper  + smsa + fatheduc + motheduc , data=card)
summary(model.2)

## ----r------------------------------------------------------------------------
summary(lm(educ ~ nearc2 + nearc4 + exper + smsa + fatheduc + motheduc, data=card))

## ----r------------------------------------------------------------------------
model.3 <- ivreg(wage ~ educ + exper  + smsa + fatheduc + motheduc + smsa 
                 | nearc2 + nearc4 + exper + smsa + fatheduc + motheduc,
                 data=card)
summary(model.3)

