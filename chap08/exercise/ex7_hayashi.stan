data {
    int N;
    int K;
    int<lower=0> Y[N];
    int<lower=1, upper=K> pot[N];
    int<lower=0, upper=1> F[N];
}

parameters {
    real a[2];
    real b[K]; // 植木鉢差
    real c[N]; // 個体差
    real<lower=0> s_b;
    real<lower=0> s_c;
}

transformed parameters {
    real lambda[N];

    for (n in 1:N) {
        lambda[n] = a[1] + a[2]*F[n] + b[pot[n]] + c[n];
    }
}

model {
    for (n in 1:N) {
        Y[n] ~ poisson_log(lambda[n]);
        c[n] ~ normal(0, s_c);
    }
    for (k in 1:K) {
        b[k] ~ normal(0, s_b);
    }
}
