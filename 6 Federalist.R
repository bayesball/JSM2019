## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE, warnings = FALSE)


## ------------------------------------------------------------------------
library(tidyverse)
d <- read_csv("Hamilton_can.csv")
head(d)


## ---- echo = FALSE-------------------------------------------------------
ggplot(d, aes(Rate)) + 
  geom_histogram(bins = 10,
                 color = "white",
                 fill = "red")


## ------------------------------------------------------------------------
modelString = "
model{
for (i in 1:N) {
y[i] ~ dpois(n[i] * lambda / 1000)
}
lambda ~ dgamma(0.001, 0.001)
}
"


## ------------------------------------------------------------------------
y <- d$N
n <- d$Total
the_data <- list("y" = y, "n" = n, N = length(y))


## ------------------------------------------------------------------------
library(runjags)
jags <- run.jags(modelString,
                 data = the_data,
                 monitor = c("lambda"),
                 n.chains = 1,
                 burnin = 2000,
                 sample = 5000,
                 silent.jags = TRUE)


## ------------------------------------------------------------------------
library(coda)
post <- as.mcmc(jags)
one_rep <- function(j){
  lambda <- post[j]
  sd(rpois(length(y), n * lambda / 1000))
}
sapply(1:5000, one_rep) -> SD


## ---- echo = FALSE-------------------------------------------------------
library(CalledStrike)
ggplot(data.frame(sd = SD), aes(sd)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) + increasefont() +
  geom_vline(xintercept = sd(y), size = 3 ) +
  annotate('text', x = 4, y = 950, 
           label = "Observed", size = 7) +
  ggtitle(paste("Tail Probability =",
                mean(SD >= sd(y))))


## ------------------------------------------------------------------------
modelString = "
model{
for(i in 1:N){
p[i] <- beta / (beta + n[i] / 1000)
y[i] ~ dnegbin(p[i], alpha)
}	
mu <- alpha / beta
alpha ~ dgamma(.001, .001)
beta ~ dgamma(.001, .001)
}
"


## ------------------------------------------------------------------------
the_data <- list("y" = y, "n" = n, N = length(y))
jags <- run.jags(modelString,
                 data = the_data,
                 monitor = c("alpha", "beta", "mu"),
                 n.chains = 1,
                 burnin = 2000,
                 sample = 5000,
                 silent.jags = TRUE)


## ---- echo = FALSE-------------------------------------------------------
post <- as.data.frame(as.mcmc(jags))
one_rep <- function(j){
  p <- post$beta[j] / (post$beta[j] + n / 1000)
  sd(rnbinom(length(y), size = post$alpha[j], prob = p))
}
sapply(1:5000, one_rep) -> SD
ggplot(data.frame(sd = SD), aes(sd)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) + increasefont() +
  geom_vline(xintercept = sd(y), size = 3 ) +
  annotate('text', x = 7, y = 1250, 
           label = "Observed", size = 7) +
  ggtitle(paste("Tail Probability =",
                mean(SD >= sd(y))))


## ---- echo = FALSE-------------------------------------------------------
library(coda)
plot(jags, vars = "mu")


## ------------------------------------------------------------------------
modelString = "
model{
for(i in 1:N1){
p1[i] <- beta1 / (beta1 + n1[i] / 1000)
y1[i] ~ dnegbin(p1[i], alpha1)
}	
for(i in 1:N2){
p2[i] <- beta2 / (beta2 + n2[i] / 1000)
y2[i] ~ dnegbin(p2[i], alpha2)
}	
alpha1 ~ dgamma(.001, .001)
beta1 ~ dgamma(.001, .001) 
alpha2 ~ dgamma(.001, .001)
beta2 ~ dgamma(.001, .001)
ratio <- (alpha2 / beta2) / (alpha1 / beta1)
}
"


## ------------------------------------------------------------------------
d1 <- read_csv("Hamilton_can.csv")
d2 <- read_csv("Madison_can.csv")

the_data <- list("y1" = d1$N, "n1" = d1$Total, 
                 "N1" = length(d1$N),
                 "y2" = d2$N, "n2" = d2$Total,
                 "N2" = length(d2$N))


## ---- echo = FALSE-------------------------------------------------------
jags <- run.jags(modelString,
                 data = the_data,
                 monitor = c("ratio"),
                 n.chains = 1,
                 burnin = 2000,
                 sample = 5000,
                 silent.jags = TRUE)
plot(jags)


## ---- echo = FALSE-------------------------------------------------------
d <- read_csv("fed_word_study.csv")
bayes_one_word <- function(theword){
  d1 <- filter(d, Authorship == "Hamilton",
               word == theword)
  d2 <- filter(d, Authorship == "Madison",
               word == theword)
  the_data <- list("y1" = d1$N, "n1" = d1$Total, 
                   "N1" = length(d1$N),
                   "y2" = d2$N, "n2" = d2$Total,
                   "N2" = length(d2$N))
  jags <- run.jags(modelString,
                   data = the_data,
                   monitor = c("ratio"),
                   n.chains = 1,
                   burnin = 2000,
                   sample = 5000,
                 silent.jags = TRUE)
  quantile(as.mcmc(jags$mcmc), c(.025, .5, .975))
}


## ------------------------------------------------------------------------
word_list <- c("by", "from", "to", "an", "any", "may",
               "his", "upon", "also", "can",
               "of", "on", "there", "this")
word_list <- sort(word_list)
S <- sapply(word_list, bayes_one_word)
df <- data.frame(Word = word_list,
                 LO = S[1, ],
                 M = S[2, ],
                 HI = S[3, ])


## ---- echo = FALSE-------------------------------------------------------
ggplot(df, aes(x = Word, y = M)) +
  geom_errorbar(aes(ymin = LO, ymax = HI), width = 0.3, size = 1) +
  geom_point() + coord_flip() +
  increasefont() +
  ylab("Ratio favoring Madison") +
  geom_hline(yintercept = 1)

