data{
  int N;
  int y[N];
  real<lower=0> a;
  real<lower=0> b;
}
parameters{
  real<lower=0, upper=1> theta;
}
model{
  target+=bernoulli_lpmf(y|theta);
  target+=beta_lpdf(theta|a,b);
}
