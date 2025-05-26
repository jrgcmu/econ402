## ----r setup, include=FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

## ----r------------------------------------------------------------------------
library(tidyverse)
gril <- read_csv("griliches.csv")
var(gril$age)
sd(gril$age)
var(gril$expr)
sd(gril$expr)

## ----r------------------------------------------------------------------------
x <- c(1, 3, 5)
y <- c(10, 15, 20)
z <- c(10, 5, -3)
cov(x,y)
cov(x,z)

## ----r------------------------------------------------------------------------
gril$w <- exp(gril$lw) # get the wage (not the log wage)
cov(gril$w, gril$s) # get the cov between wages and education

## ----r------------------------------------------------------------------------
x <- c(1, 5, 7)
y <- c(7, 8, 10)
z <- c(14, 16, 20)
cov(x,y)
cov(x,z)

## ----r------------------------------------------------------------------------
x <- c(1, 5, 7)
y <- c(7, 8, 10)
z <- c(14, 16, 20)
cor(x,y)
cor(x,z)

## ----r------------------------------------------------------------------------
cor(gril$w, gril$s)

## ----r------------------------------------------------------------------------
library(ggplot2)
x <- rnorm(1000,0,1)
y <- rnorm(1000,0,5)
ggplot() + geom_density(aes(x)) + geom_density(aes(y), lty=2)

