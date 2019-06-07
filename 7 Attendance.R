## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE, warnings = FALSE)


## ------------------------------------------------------------------------
library(readr)
d <- read_csv("tribe2016.csv")


## ------------------------------------------------------------------------
library(betareg)
fit <- betareg(fraction ~ Weekend + Period, data=d,
               link="logit")


## ------------------------------------------------------------------------
summary(fit)


## ------------------------------------------------------------------------
library(rstanarm)
fit2 <- stan_betareg(fraction ~ Weekend + Period, data=d,
                     link="logit")


## ------------------------------------------------------------------------
prior_summary(fit2)


## ------------------------------------------------------------------------
library(bayesplot)
mcmc_trace(as.matrix(fit2))


## ------------------------------------------------------------------------
mcmc_acf(as.matrix(fit2))


## ------------------------------------------------------------------------
mcmc_dens(as.matrix(fit2))


## ------------------------------------------------------------------------
posterior_interval(fit2)


## ------------------------------------------------------------------------
posterior_sims <- as.matrix(fit2)
head(posterior_sims)


## ------------------------------------------------------------------------
library(arm)
d1 <- data.frame(Label="Weekday Period 1", 
      Fraction=invlogit(posterior_sims[, "(Intercept)"]))
d2 <- data.frame(Label="Weekday Period 2", 
      Fraction=invlogit(posterior_sims[, "(Intercept)"] +
                      posterior_sims[, "PeriodSecond"]))
d3 <- data.frame(Label="Weekday Period 3", 
      Fraction=invlogit(posterior_sims[, "(Intercept)"] +
                      posterior_sims[, "PeriodThird"]))


## ---- fig.height=4-------------------------------------------------------
library(ggplot2)
ggplot(rbind(d1, d2, d3), aes(Fraction)) +
  geom_density() + facet_wrap(~ Label, ncol=1)


## ---- fig.height=3-------------------------------------------------------
pp_check(fit2)


## ------------------------------------------------------------------------
ynew <- posterior_predict(fit2)


## ---- fig.height=3-------------------------------------------------------
ppc_stat(d$fraction, ynew, stat="median")


## ---- fig.height=3-------------------------------------------------------
ppc_stat(d$fraction, ynew, stat="sd")


## ---- fig.height=3-------------------------------------------------------
ppc_stat(d$fraction, ynew, stat="min")

