## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE, warnings = FALSE)


## ------------------------------------------------------------------------
library(LearnBayes)
head(birthweight)


## ------------------------------------------------------------------------
birthweight$s_age <- scale(birthweight$age)
birthweight$s_weight <- scale(birthweight$weight)
logpost <- function(theta, data){
  a <- theta[1]
  b <- theta[2]
  sigma <- exp(theta[3])
  sum(dnorm(data$s_weight, 
            mean = a + b * data$s_age, 
                         sd = sigma, log=TRUE))
}


## ------------------------------------------------------------------------
laplace(logpost, c(0, 0, 0), birthweight)$mode


## ------------------------------------------------------------------------
library(rethinking)

flist <- alist(
  s_weight ~ dnorm( mu, sigma ) ,
  mu <- a + b * s_age ,
  a ~ dnorm( 0, 10) ,
  b ~ dnorm(0, 10) ,
  sigma ~ dunif( 0, 20)
)


## ------------------------------------------------------------------------
m3 <- rethinking::map(flist, data=birthweight)
precis(m3)


## ------------------------------------------------------------------------
post <- extract.samples(m3)
head(post)


## ---- fig.width=4, fig.height=2.5----------------------------------------
library(ggplot2)
ggplot(birthweight, aes(s_age, s_weight)) +
  geom_point() +
  geom_abline(data=post[1:10, ], 
              aes(intercept=a, slope=b))


## ---- fig.width=4, fig.height=2.5----------------------------------------
s_age <- 1
mean_response <- post[, "a"] + s_age * post[, "b"]
ggplot(data.frame(mean_response), aes(mean_response)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(color="red")


## ------------------------------------------------------------------------
data_new <- data.frame(s_age = c(-1, 0, 1))
pred <- sim(m3, data_new)


## ------------------------------------------------------------------------
apply(pred, 2, quantile, c(.1, .9))

