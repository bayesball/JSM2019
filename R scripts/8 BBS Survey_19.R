## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE,
                      message = FALSE)


## ------------------------------------------------------------------------
library(tidyverse)
(d <- read_csv("BBS_survey.csv"))


## ------------------------------------------------------------------------
model_string <- "model{
  for (s in 1:28) {
      varhat[s] ~ dgamma(p[s], lam[s])
      p[s] <- n[s] / 2
      lam[s] <- p[s] * tau.betahat[s]
      tau.betahat[s] ~ dgamma(0.001, 0.001)
      sd.betahat[s] <- 1 / sqrt(tau.betahat[s])
      betahat[s] ~ dnorm(beta[s], tau.betahat[s])
      beta[s] ~ dnorm(0, 0.0001)
  }
}"


## ------------------------------------------------------------------------
betahat <- d$Trend
varhat <- d$SE ^ 2
n <- d$N_Site
the_data <- list(betahat = betahat,
                 varhat = varhat,
                 n = n)


## ------------------------------------------------------------------------
library(runjags)
posterior <- run.jags(model_string,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("beta"),
                      adapt = 1000,
                      burnin = 10000,
                      sample = 5000)


## ------------------------------------------------------------------------
library(coda)
library(bayesplot)
posterior %>% as.mcmc() %>% as.data.frame() -> post
dplyr::select(post, `beta[1]`:`beta[28]`) %>%
  mcmc_intervals() 


## ------------------------------------------------------------------------
model_string <- "model{
  for (s in 1:28) {
      varhat[s] ~ dgamma(p[s], lam[s])
      p[s] <- n[s] / 2
      lam[s] <- p[s] * tau.betahat[s]
      tau.betahat[s] ~ dgamma(0.001, 0.001)
      sd.betahat[s] <- 1 / sqrt(tau.betahat[s])
      betahat[s] ~ dnorm(beta, tau.betahat[s])
  }
  beta ~ dnorm(0, 0.0001)
}"


## ------------------------------------------------------------------------
library(runjags)
posterior <- run.jags(model_string,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("beta"),
                      adapt = 1000,
                      burnin = 10000,
                      sample = 5000)


## ------------------------------------------------------------------------
summary(posterior) 


## ------------------------------------------------------------------------
model_string <- "model{
  for (s in 1:N) {
      varhat[s] ~ dgamma(p[s], lam[s])
      p[s] <- n[s] / 2
      lam[s] <- p[s] * tau.betahat[s]
      tau.betahat[s] ~ dgamma(0.001, 0.001)
      sd.betahat[s] <- 1 / sqrt(tau.betahat[s])
      betahat[s] ~ dnorm(beta[s], tau.betahat[s])
      beta[s] ~ dnorm(mu, tau.beta)
  }
  mu ~ dnorm(0.0, 1.0E-6)
  tau.beta ~ dgamma(0.001, 0.001)
  sd.beta <- 1 / sqrt(tau.beta)
  another.beta ~ dnorm(mu, tau.beta)
}"


## ------------------------------------------------------------------------
d <- read_csv("BBS_survey.csv")
betahat <- d$Trend
varhat <- d$SE ^ 2
n <- d$N_Site
the_data <- list(betahat = betahat,
                 varhat = varhat,
                 n = n,
                 N = length(n))


## ------------------------------------------------------------------------
library(runjags)
posterior <- run.jags(model_string,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("beta", "mu", "sd.beta"),
                      adapt = 1000,
                      burnin = 20000,
                      sample = 5000)


## ------------------------------------------------------------------------
library(coda)
posterior %>% as.mcmc() %>% as.data.frame() -> post


## ------------------------------------------------------------------------
dplyr::select(post, `beta[1]`:`beta[28]`) %>%
  mcmc_intervals()


## ---- echo = FALSE-------------------------------------------------------
library(TeachBayes)


## ---- fig.height = 5.5---------------------------------------------------
Beta <- post[, 1:28]
sum.negative <- function(y) {sum(y < 0)}
D <- apply(Beta, 1, sum.negative)
bar_plot(D)


## ---- fig.height = 5.5---------------------------------------------------
Ranks <- t(apply(Beta, 1, rank))
bar_plot(Ranks[, 27]) + xlim(0, 13)

