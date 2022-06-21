#normal case
set.seed(345)
mu <- 5
sigma <- 2
R <- 100
n <- 10

plot(0,0,xlim=c(0,10),ylim=c(0,11), type="n", xlab=expression(mu), ylab="",
     main = paste("100 IC for the mean (known variance)"), cex.main=1.2)
abline(v=mu)
alpha <- 0.05
inf <- vector(mode="numeric", length=R)
sup <- vector(mode="numeric", length=R)
l <- vector(mode="numeric",   length=R)
d <- 0
for (i in 1:R)
{
  y <- rnorm(n, mu, sigma)
  inf[i] <- mean(y)-qt(1-alpha/2, n-1)*var(y)/sqrt(n)
  sup[i] <- mean(y)+qt(1-alpha/2, n-1)*var(y)/sqrt(n)
  d <- d + 0.1 
  l[i] <- (mu > inf[i] & mu < sup[i]) 
  lines(seq(inf[i],sup[i],length=100), rep(d, 100), col=(l[i]+1))
}