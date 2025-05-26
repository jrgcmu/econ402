## ----r setup, include=FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

## ----r------------------------------------------------------------------------
x <- seq(-4, 4, by = 0.1)
y <- dnorm(x)
plot(x, y, type = "l", xlab = "x", ylab = "Density")

## ----r, echo=F----------------------------------------------------------------
x <- seq(-4, 4, by = 0.1)
y <- dnorm(x)
plot(x, y, type = "l", xlab = "x", ylab = "Density")
x <- seq(-1, 1, length.out=50)
y <- dnorm(x)
polygon(c(-1, x, 1), c(0, y , 0), col = adjustcolor("blue", alpha.f=.75), border = NA, density = c(10, 20), angle = c(-45, 45))
mtext("-1", 1, line=1, at=-1)
mtext("1", 1, line=1, at=1)

## ----r, echo=FALSE------------------------------------------------------------
x <- seq(-4, 4, by = 0.1)
y <- dnorm(x)
plot(x, y, type = "l", xlab = "x", ylab = "Density")
x1 <- seq(-1.96, -4, length.out = 50)
x2 <- seq(1.96, 4, length.out = 50)
y <- dnorm(x1)
polygon(c(x1, -1.96, -1.96), c(y, 0, dnorm(-1.96)), col=adjustcolor("blue", alpha.f=.75), border = NA)
y <- dnorm(x2)
polygon(c(x2, 1.96, 1.96), c(y, 0, dnorm(1.96)), col=adjustcolor("blue", alpha.f=.75), border = NA)
mtext(expression("-t"[.025]), 1, line=2, at=-1.96)
mtext(expression("t"[.025]), 1, line=2, at=1.96)

## ----r------------------------------------------------------------------------
library(tidyverse)
gril <- read.csv("griliches.csv")
gril$wage <- exp(gril$lw)
our.model <- lm(wage ~ s, gril)
summary(our.model)

## ----r------------------------------------------------------------------------
t <- rep(0,1000)
for (i in 1:1000) {
  x <- rnorm(100)
  e <- rnorm(100)
  y <- 1 + 2*x + e
  b <- lm(y ~ x)$coef[2]
  se <- sqrt(vcov(lm(y ~ x))[2,2])
  t[i] <- (b-2)/se
}
mean(abs(t)>1.96)

## ----r, echo=FALSE------------------------------------------------------------
x <- seq(-4, 4, by = 0.1)
y <- dnorm(x)
plot(x, y, type = "l", xlab = "x", ylab = "Density")
x2 <- seq(1.96, 4, length.out = 50)
y <- dnorm(x2)
polygon(c(x2, 1.645, 1.645), c(y, 0, dnorm(1.645)), col = adjustcolor("blue", alpha.f=.75), border = NA)
mtext(expression("t"[.05]), 1, line=2, at=1.645)

## ----r------------------------------------------------------------------------
in.ci <- rep(0,1000)
for (i in 1:1000) {
  x <- rnorm(100)
  e <- rnorm(100)
  y <- 1 + 2*x + e
  b <- lm(y ~ x)$coef[2]
  se <- sqrt(vcov(lm(y ~ x))[2,2])
  low <- b - 1.96*se
  high <- b + 1.96*se
  in.ci[i] <- (low <= 2 && 2 <= high)
}
mean(in.ci)

## ----r------------------------------------------------------------------------
confint(our.model, level=.95)

## ----r, echo=FALSE------------------------------------------------------------
x <- seq(-4, 4, by = 0.1)
y <- dnorm(x)
plot(x, y, type = "l", xlab = "x", ylab = "Density")
x1 <- seq(-1.96, -4, length.out = 50)
x2 <- seq(1.96, 4, length.out = 50)
y <- dnorm(x1)
polygon(c(x1, -1.5, -1.5), c(y, 0, dnorm(-1.5)), col=adjustcolor("blue", alpha.f=.75), border = NA)
y <- dnorm(x2)
polygon(c(x2, 1.5, 1.5), c(y, 0, dnorm(1.5)), col=adjustcolor("blue", alpha.f=.75), border = NA)
mtext(expression("-t"), 1, line=2, at=-1.5)
mtext(expression("t"), 1, line=2, at=1.5)

