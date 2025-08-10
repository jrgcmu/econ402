## ----r setup, include=FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

## ----r------------------------------------------------------------------------
2 + 2

## ----r------------------------------------------------------------------------
a <- 5
a 
a*2
a^2

## ----r------------------------------------------------------------------------
b <- c(1, 5, 10)
c <- c("Hello", ",", "how", "are", "you", "?")
b
b/5
c

## ----r, eval=FALSE------------------------------------------------------------
# setwd("~/Desktop")
# save.image("MyRFile.RData")

## ----r, eval=FALSE------------------------------------------------------------
# setwd("~/Desktop")
# load("MyRFile.RData")

## ----r, eval=FALSE------------------------------------------------------------
# install.packages("packagename")

## ----r, eval=FALSE------------------------------------------------------------
# library(packagename)

## ----r------------------------------------------------------------------------
library(tidyverse)
murder <- read_csv("murder.csv")

## ----r------------------------------------------------------------------------
library(readxl)
murder <- read_excel("murder.xlsx", sheet="Sheet1")

## ----r------------------------------------------------------------------------
head(murder)

## ----r------------------------------------------------------------------------
str(murder)

## ----r------------------------------------------------------------------------
ggplot(murder, aes(x = mrdrte)) + geom_histogram()

## ----r------------------------------------------------------------------------
ggplot(murder, aes(x = exec, y = mrdrte)) + geom_point()

## ----r------------------------------------------------------------------------
ggplot(murder, aes(x = exec, y = mrdrte)) + 
  geom_point() + geom_smooth(method="lm")

## ----r------------------------------------------------------------------------
ggplot(murder, aes(x = exec, y = mrdrte)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~ year, scales="free")

## ----r------------------------------------------------------------------------
summary(murder)

## ----r------------------------------------------------------------------------
psych::describe(murder)

## ----r------------------------------------------------------------------------
murder |> summarize(mean_mrdrte = mean(mrdrte), 
                    sd_mrdrte = sd(mrdrte),
                    mean_exec = mean(exec),
                    sd_exec = sd(exec),
                    n = n()) 

## ----r------------------------------------------------------------------------
murder |> filter(year==87) |>
  summarize(mean_mrdrte = mean(mrdrte), 
            sd_mrdrte = sd(mrdrte),
            mean_exec = mean(exec),
            sd_exec = sd(exec),
            n = n()) 

## ----r------------------------------------------------------------------------
murder |> group_by(year) |>
 summarize(mean_mrdrte = mean(mrdrte), 
           sd_mrdrte = sd(mrdrte),
           mean_exec = mean(exec),
           sd_exec = sd(exec),
           n = n())  

## ----r------------------------------------------------------------------------
murder$unem_sq <- murder$unem^2

## ----r------------------------------------------------------------------------
murder <- murder |> mutate(unem_sq = unem^2,
                           exec_sq = exec^2,
                           year90 = (year==90),
                           year93 = (year==93))

## ----r------------------------------------------------------------------------
murder <- murder |> mutate(year = case_when(year == 87 ~ 1987, 
                                            year == 90 ~ 1990,
                                            year == 93 ~ 1993))
murder |> head()

## ----r------------------------------------------------------------------------
fake <- murder |> select(state, year) |> mutate(newvar = rnorm(n()))

## ----r------------------------------------------------------------------------
murder <- murder |> left_join(fake, join_by(state, year))
murder |> select(state, year, mrdrte, newvar) |> head()

## ----r------------------------------------------------------------------------
model <- lm(mrdrte ~ exec, murder)

## ----r------------------------------------------------------------------------
summary(model)

