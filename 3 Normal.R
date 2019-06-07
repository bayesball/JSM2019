## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE, warnings = FALSE)


## ------------------------------------------------------------------------
library(rethinking)
data(Howell1)
d2 <- Howell1[Howell1$age >= 18, ]


## ---- fig.width=4, fig.height=2.5----------------------------------------
sample_mu <- rnorm(1000, 170, 20)
sample_sigma <- runif(1000, 0, 50)
prior_h <- rnorm(1000, sample_mu, sample_sigma)
dens(prior_h)


## ------------------------------------------------------------------------
require(LearnBayes)
lpost <- function(parameters, y){
  mu <- parameters[1]
  sigma <- parameters[2]
  log_likelihood <- sum(dnorm(y, mu, sigma, log=TRUE))
  log_prior <- dnorm(mu, 178, 20, log=TRUE) +
               dunif(sigma, 0, 50, log=TRUE )
  log_likelihood + log_prior
}


## ------------------------------------------------------------------------
mycontour(lpost, c(152, 157, 6.5, 9), d2$height)


## ------------------------------------------------------------------------
flist <- alist(
  height ~ dnorm( mu, sigma ) ,
  mu ~ dnorm( 178, 20 ) ,
  sigma ~ dunif( 0, 50)
)


## ------------------------------------------------------------------------
m4.1 <- map( flist, data = d2)


## ------------------------------------------------------------------------
precis( m4.1 )


## ------------------------------------------------------------------------
m4.2 <- map(flist <- alist(
  height ~ dnorm( mu, sigma ) ,
  mu ~ dnorm( 178, 0.1 ) ,
  sigma ~ dunif( 0, 50)
  ), data=d2
)
precis(m4.2)


## ------------------------------------------------------------------------
vcov( m4.1 )


## ------------------------------------------------------------------------
post <- extract.samples( m4.1, 1000)
head(post)


## ------------------------------------------------------------------------
library(ggplot2)
ggplot(post, aes(mu, sigma)) + geom_point()


## ------------------------------------------------------------------------
library(dplyr)
summarize(post, Prob=mean(mu > 155))


## ------------------------------------------------------------------------
summarize(post, LO = quantile(sigma, .1),
                HI = quantile(sigma, .9))


## ------------------------------------------------------------------------
post <- mutate(post, CV = mu / sigma)


## ------------------------------------------------------------------------
summarize(post, LO = quantile(CV, .1),
                HI = quantile(CV, .9))


## ------------------------------------------------------------------------
one_sample <- function(j){
  pars <- post[j, ]
  ys <- rnorm(10, pars$mu, pars$sigma)
  max(ys)
}


## ------------------------------------------------------------------------
library(tidyverse)
MS <- map_dbl(1:1000, one_sample)


## ---- fig.width=4, fig.height=2.5----------------------------------------
dens(MS)


## ------------------------------------------------------------------------
quantile(MS, c(.10, .90))

