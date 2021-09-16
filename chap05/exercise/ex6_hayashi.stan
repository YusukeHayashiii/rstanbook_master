data {
    int N;
    real<lower=0> X[N];
    int<lower=0, upper=1> F[N];
    int<lower=0> Y[N];
}

parameters {
   real b[3];
}

transformed parameters {
   real lambda[N];

   for (i in 1:N) {
       lambda[i] = b[1] + b[2]*X[i] + b[3]*F[i];
   }
}

model {
    for (i in 1:N) {
        Y[i] ~ poisson_log(lambda[i]);
    }
}

generated quantities {
    int Y_pred[N];
    for (i in 1:N) {
        Y_pred[i] = poisson_log_rng(lambda[i]);
    }
}
