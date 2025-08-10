setwd("/Users/johngardner/Library/CloudStorage/Box-Box/OleMissTeaching/402-Econometrics/NewSlides")


# basics ------------------------------------------------------------------

2 + 2
a <- 5
a 
a*2
a^2


# vectors -----------------------------------------------------------------

b <- c(1, 5, 10)
c <- c("Hello", ",", "how", "are", "you", "?")
b
b/5
c

# reading data ------------------------------------------------------------

library(tidyverse)
murder <- read_csv("murder.csv")

library(readxl)
murder <- read_excel("murder.xlsx", sheet="Sheet1")

head(murder)

str(murder)

View(murder)


# graphs ------------------------------------------------------------------

ggplot(murder, aes(x = mrdrte)) + geom_histogram()

ggplot(murder, aes(x = exec, y = mrdrte)) + geom_point()

ggplot(murder, aes(x = exec, y = mrdrte)) + 
  geom_point() + geom_smooth(method="lm")

ggplot(murder, aes(x = exec, y = mrdrte)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~ year, scales="free")


# summary statistics ------------------------------------------------------

summary(murder)

psych::describe(murder)

murder |> summarize(mean_mrdrte = mean(mrdrte), 
                    sd_mrdrte = sd(mrdrte),
                    mean_exec = mean(exec),
                    sd_exec = sd(exec),
                    n = n()) 

murder |> filter(year==87) |>
  summarize(mean_mrdrte = mean(mrdrte), 
            sd_mrdrte = sd(mrdrte),
            mean_exec = mean(exec),
            sd_exec = sd(exec),
            n = n()) 

murder |> group_by(year) |>
 summarize(mean_mrdrte = mean(mrdrte), 
           sd_mrdrte = sd(mrdrte),
           mean_exec = mean(exec),
           sd_exec = sd(exec),
           n = n())  


# transforming data -------------------------------------------------------

murder$unem_sq <- murder$unem^2

murder <- murder |> mutate(unem_sq = unem^2,
                           exec_sq = exec^2,
                           year90 = (year==90),
                           year93 = (year==93))

murder <- murder |> mutate(year = case_when(year == 87 ~ 1987, 
                                            year == 90 ~ 1990,
                                            year == 93 ~ 1993))
murder |> head()


# merging data ------------------------------------------------------------

fake <- murder |> select(state, year) |> mutate(newvar = rnorm(n()))

murder <- murder |> left_join(fake, join_by(state, year))

murder |> select(state, year, mrdrte, newvar) |> head()


# more --------------------------------------------------------------------

model <- lm(mrdrte ~ exec, murder)

summary(model)

