## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE, warnings = FALSE)


## ------------------------------------------------------------------------
library(tidyverse)
ScoreData <- read_csv("ScoreData.csv")
head(ScoreData)


## ----  echo = FALSE------------------------------------------------------
library(ggrepel)
ggplot(ScoreData, aes(x=Person, y=Score)) +
  geom_point(size=3) +
  ylim(5, 20) +
  labs(x = "Person Index", y = "Score") +
  theme_grey(base_size = 20, base_family = "") +
  geom_text_repel(aes(label=Person), size = 6)



## ------------------------------------------------------------------------
modelString<-"
model {
# data is binomial with rate given by person's group assignment
for (i in 1:N){
theta[i] <- equals(z[i], 1) * p1 + equals(z[i], 0) * p0
y[i] ~ dbin(theta[i], m)
}
# each person belongs to one of two latent classes
for (i in 1:N){
z[i] ~ dbern(q)
}
# first class is random guessing
p1 ~ dbeta(1, 1) T(0.4, 0.6)
# second class has unknown higher correct rate
p0 ~ dbeta(1,1) T(p1, 1)
q ~ dbeta(1, 1)
}
"


## ------------------------------------------------------------------------
y <- ScoreData$Score
N <- length(y)
the_data <- list("y" = y, "N" = N, "m" = 20)


## ------------------------------------------------------------------------
initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}


## ------------------------------------------------------------------------
library(runjags)
posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("z", "p1", "p0", "q"),
                      adapt = 1000,
                      burnin = 20000,
                      sample = 5000,
                      thin = 5,
                      inits = initsfunction,
                      silent.jags = TRUE
                     )


## ---- echo = FALSE-------------------------------------------------------
library(coda)
post <- as.mcmc(posterior)
post_means  <- data.frame(Index = 1:30,
                          Mean = apply(post, 2, mean)[1:30])
ggplot(post_means, aes(Index, Mean)) +
  geom_point(size = 3) +
  theme_grey(base_size = 20, base_family = "")


## ---- echo = FALSE-------------------------------------------------------
post <- as.data.frame(post)
post %>% dplyr::select(p1, p0, q) %>%
  gather(Type, Value) -> post2
library(ggridges)
ggplot(post2, aes(x = Value, y = Type)) +
  geom_density_ridges() +
  theme_grey(base_size = 20, base_family = "")

