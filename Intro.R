## ----r setup, include=FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

## ----r------------------------------------------------------------------------
library(tidyverse)
card <- read_csv("card.csv")

## ----r------------------------------------------------------------------------
summary(card)

## ----r------------------------------------------------------------------------
ggplot(data=card, aes(x=educ, y=wage)) + geom_point()

## ----r------------------------------------------------------------------------
educ.mod.1 <- lm(wage ~ educ, data=card)
summary(educ.mod.1)

## ----r------------------------------------------------------------------------
educ.mod.2 <- lm(wage ~ educ + exper, data=card)
summary(educ.mod.2)

## ----r------------------------------------------------------------------------
educ.mod.3 <- lm(wage ~ educ + exper + IQ + KWW, data=card)
summary(educ.mod.3)

## ----r------------------------------------------------------------------------
educ.mod.4 <- lm(wage ~ educ + exper + IQ + KWW + motheduc + fatheduc + libcrd14, data=card)
summary(educ.mod.4)

