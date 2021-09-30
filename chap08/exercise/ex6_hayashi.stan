data {
    int N;
    int M;
    int Y[N];
}

parameters {
    real a0;
    real a[N];
    real<lower=0> s_a;

}

transformed parameters {
    real q[N];
    for (n in 1:N) {
        q[n] = inv_logit(a0 + a[n]);
    }
}

model {
    for (n in 1:N) {
        a[n] ~ normal(0, s_a);
        Y[n] ~ binomial(M, q[n]);
    }
}
