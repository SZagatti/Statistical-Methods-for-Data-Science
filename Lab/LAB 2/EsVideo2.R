set.seed(101)
n <- 50
K <- 4
M <- 6
# generate the values
for (i in 1:6) {
y[i] <- sample( 1:K, n, replace=TRUE, prob =c( 7/16, 5/16, 3/16, 1/16))
observed[i] <- table(y[i])
expected[i] <- c( n*(7/16), n*(5/16), n*(3/16), n*(1/16))
x2[i] <- sum( (observed[i]-expected[i])^(2)/expected[i])
#manually compute the p-value
pchisq(x2[i], df =(K-1)*(M-1), lower.tail =FALSE )
}