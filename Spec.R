## ----r setup, include=FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

## ----r------------------------------------------------------------------------
library(tidyverse)
gril <- read_csv("griliches.csv")
summary(gril$rns)

## ----r------------------------------------------------------------------------
gril$s_rns <- gril$rns * gril$s
gril$w <- exp(gril$lw)
ixn.model <- lm(w ~ rns + s + s_rns, data=gril)
summary(ixn.model)

## ----r------------------------------------------------------------------------
north.model <- lm(w ~ s, data=gril[gril$rns==0,])
south.model <- lm(w ~ s, data=gril[gril$rns==1,])
south.model$coef - north.model$coef

## ----r------------------------------------------------------------------------
pred.n <- cbind(1, 9:18) %*% north.model$coef
pred.s <- cbind(1, 9:18) %*% south.model$coef
plot(gril$s, gril$w, pch=20)
lines(9:18, pred.n, type="l", lty=1, lwd=2, col="blue")
lines(9:18, pred.s, type="l", lty=2, lwd=2, col="red")
legend("topleft", legend=c("North", "South"), lty=1:2, box.lty=0)

## ----r------------------------------------------------------------------------
library(car)
linearHypothesis(ixn.model, c("rns=0", "s_rns=0"))

## ----r------------------------------------------------------------------------
library(haven)
mur <- read_dta("MURDER.dta")
mur.mod4 <- lm(mrdrte ~ exec + unem + as.factor(year) + as.factor(state), data=mur)
summary(mur.mod4)

## ----r------------------------------------------------------------------------
library(haven)
mroz <- read_dta("MROZ.dta")
summary(mroz)

## ----r------------------------------------------------------------------------
lf.model <- lm(inlf ~ kidslt6 + kidsge6 + educ + exper + huseduc + huswage, data=mroz)
summary(lf.model)

## ----r------------------------------------------------------------------------
library(haven)
ff <- read_dta("fastfood.dta")
ff <- ff |> 
      select(state, empft, empft2, emppt, emppt2) |> 
      rename(empft_1=empft, emppt_1=emppt, empft_2=empft2, emppt_2 = emppt2) |>
      pivot_longer(starts_with("emp"), names_to=c(".value", "period"), values_to=c("emp"),
                   names_sep="_")

## ----r------------------------------------------------------------------------
ck <- lm(empft ~ factor(state) + factor(period) + factor(state)*factor(period), data=ff)
summary(ck)

## ----r------------------------------------------------------------------------
x <- 1:10
y <- x^{.3}
plot(x, y, type="b", xlab="x", ylab="f(x)")

## ----r------------------------------------------------------------------------
x <- 1:50
y <- log(x) + rnorm(50)/5
quad <- lm(y ~ x + I(x^2))
y.hat <- quad$fitted.values
plot(x, y)
lines(x, y.hat, type="l")

## ----r------------------------------------------------------------------------
quad <- lm(y ~ x)
y.hat.lin <- quad$fitted.values
plot(x, y)
lines(x, y.hat.lin, type="l")

## ----r------------------------------------------------------------------------
gril$ls <- log(gril$s)
b1 <- lm(lw ~ ls, data=gril)$coef[2]
b2 <- lm(lw ~ s, data=gril)$coef[2]
b3 <- lm(w ~ ls, data=gril)$coef[2]
c(b1, b2, b3)

