## ----r setup, include=FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

## ----r------------------------------------------------------------------------
library(tidyverse)
b <- rep(0,100)
for (i in 1:1000) {
  x <- rnorm(100)
  e <- rnorm(100)
  y <- 1 + 2*x + e
  b[i] <- lm(y ~ x)$coef[2]
}
ggplot() + geom_histogram(aes(b)) + geom_vline(xintercept=2)

## ----r------------------------------------------------------------------------
runreg <- function(n) {
  x <- rnorm(n)
  y <- 1 + 2*x + rnorm(n)
  b <- lm(y ~ x)$coef[2]
}
b1 <- rep(0,1000)
b2 <- b1
b3 <- b1
for (i in 1:1000) {
  b1[i] <- runreg(100)
  b2[i] <- runreg(500)
  b3[i] <- runreg(1000)
}
bs <- c(b1, b2, b3)
n <- c(rep(100, 1000), rep(500, 1000), rep(1000, 1000))
df <- data.frame(bs, n)
ggplot(df, aes(bs)) + 
  geom_histogram() + geom_vline(xintercept=2) + facet_wrap(n ~ .)

## ----r------------------------------------------------------------------------
galt <- read_csv("galton.csv")
summary(galt)

## ----r------------------------------------------------------------------------
galton_model <- lm(child ~ parent, data=galt)
summary(galton_model)

