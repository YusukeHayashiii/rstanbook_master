data {
  int I;
  int<lower=0, upper=1> A[I];
  real<lower=0, upper=1> Score[I];
  int<lower=0, upper=1> cloud[I];
  int<lower=0, upper=1> rain[I];
  int<lower=0, upper=1> Y[I];
}

parameters {
  real b[5];
}

transformed parameters {
  real q[I];
  for (i in 1:I)
    q[i] = inv_logit(b[1] + b[2]*A[i] + b[3]*Score[i] + b[4]*cloud[i] + b[5]*rain[i]);
}

model {
  for (i in 1:I)
    Y[i] ~ bernoulli(q[i]);
}
