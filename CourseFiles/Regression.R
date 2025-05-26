## ----r setup, include=FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

## ----r------------------------------------------------------------------------
x <- c(0, 1)
y <- c(0, 1)
plot(x,y, type="b")
y1 <- c(1,0)
lines(x,y1, type="b", lty=2)

## ----r------------------------------------------------------------------------
library(tidyverse)
gril <- read_csv("griliches.csv")
gril$wage <- exp(gril$lw)
model <- lm(wage ~ s, data=gril)
summary(model)

## ----r------------------------------------------------------------------------
wbar <- mean(gril$wage)
wdev <- gril$wage - wbar
sbar <- mean(gril$s)
sdev <- gril$s - sbar
b <- sum(wdev*sdev)/sum(sdev^2)
b
a <- wbar - b*sbar
a

## ----r------------------------------------------------------------------------
plot(gril$s, model$fitted.values, type="b")

## ----r------------------------------------------------------------------------
summary(gril$s)

## ----r------------------------------------------------------------------------
summary(model)

