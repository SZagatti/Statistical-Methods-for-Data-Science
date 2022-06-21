library("bayesplot")
library("rstanarm")
library("ggplot2")
library("rstan")

#input values

#true mean
theta_sample <- 2
#likelihood variance
sigma2 <- 2
#sample size
n <- 10

#generate some data
set.seed(123)
y <- rnorm(n,theta_sample, sqrt(sigma2))

#launch biparametric Stan model

data3<- list(N=n, y=y, a=-10, b=10)
fit3 <- stan(file="biparametric.stan", data = data3, chains = 4, iter=2000,
             refresh=-1)

#extract stan output for biparametric model

sim3 <- extract(fit3)
posterior_biv <- as.matrix(fit3)

theta_est <- mean(sim3$theta)
sigma_est <- mean(sim3$sigma)
c(theta_est, sigma_est)
traceplot(fit3, pars=c("theta", "sigma"))

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

mcmc_areas(posterior_biv, 
           pars = c("theta","sigma"), 
           prob = 0.8) + plot_title