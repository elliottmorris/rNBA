/*
We generated random data that we know has a normal(100,18) distribution
Now we model it as arising from normal(mu,sigma)
We expect to see mu=100 and sigma=18 of course
But also for the uncertainty around mu to have standard deviation 3 (see the notes)
*/

data {
  int N; // N is an integer: the number of observations in the data
  real y[N]; // x are real numbers: the data we generated at random
  real mu_prior_mu;
  real mu_prior_sigma;
  real sigma_prior_mu;
  real sigma_prior_sigma;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  mu ~ normal(mu_prior_mu,mu_prior_sigma);
  sigma ~ normal(sigma_prior_mu,sigma_prior_sigma);
  y ~ normal(mu, sigma);
}

