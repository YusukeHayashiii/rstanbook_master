library(rstan)
library(ggplot2)

set.seed(123)
N1 <- 30
N2 <- 20
Y1 <- rnorm(n=N1, mean=0, sd=5)
Y2 <- rnorm(n=N2, mean=1, sd=4)

# 1 図を描く
win.graph(14, 7)
par(mfrow=c(1,2))

plot(Y1)
plot(Y2)

# 2 モデル式を書く

# 検定統計量T
# T = (u2 - u1) / (s^2 * (1/30 + 1/20))^1/2
# プールされた分散s^2
# s1^2, s2^2はそれぞれy1, y2の標本分散を表す
# s^2 = {(30-1)*s1^2 + (20-1)*s2^2} / (30 +20 -2)

# T ~ t-dist(48)
# u1 ~ normal(0, 1/6)
# u2 ~ Normal(1, 1/5)

### 解答
# Y1[N] ~ Normal(u1, σ)
# Y2[N] ~ Normal(u2, σ)

# 3
getwd()
setwd("C:/Users/s44990/Desktop/R_files/RStanBook-master/chap04")

data <- list(N1=N1, N2=N2, Y1=Y1, Y2=Y2)
fit <- stan(file='exercise/ex3_hayashi.stan',
            data=data,
            seed=1234)
ms <- rstan::extract(fit)

# 4
# パラメータの確認
fit

# mu1 < mu2 となっているMCMCサンプルの個数を数える
counts = 0
for (i in 1:length(ms$mu1)) {
    if (ms$mu1[i] < ms$mu2[i]) {
        counts <-  counts + 1
    }
}
print(counts)

# 確率を出す
prob <- counts / length(ms$mu1)
print(prob)

# 解答例
mean(ms$mu1 < ms$mu2)

# 5
## model
# Y1[N] ~ Normal(u1, σ1)
# Y2[N] ~ Normal(u2, σ2)

# mcmc sampling
fit_2 <- stan(file='exercise/ex5_hayashi.stan',
            data=data,
            seed=1234)
ms_2 <- rstan::extract(fit)

prob_2 <- mean(ms_2$mu1 < ms_2$mu2)
print(prob_2)
print(fit_2)
mean(ms_2$mu1 < ms_2$mu2)
