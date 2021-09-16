getwd()
setwd("C:/Users/s44990/Desktop/R_files/RStanBook-master/chap05")

library(rstan)
library(ggplot2)

# (1)
load('output/result-model5-3.RData')
ms <- rstan::extract(fit)
fit
## 元データを読み込む
d <- read.csv(file='input/data-attendance-1.txt')
head(d)
## εを算出
n <- length(d)
e <- rep(0, length=50)
for (i in 1:n) {
    e[i] <- d$Y[i] - mean(ms$mu[,i])
}
print(e)

# 解答例
dim(d$Y)
dim(t(ms$mu))
dim(d$Y - t(ms$mu))

noise <- t(d$Y - t(ms$mu))
dim(noise)


# (3)
d <- read.csv('input/data-attendance-3.txt')
aggregate(Y ~ A, data=d, FUN=table)


# (4)
d <- read.csv(file='input/data-attendance-3.txt')
head(d)
# 曇りと雨のダミー列を作成
library(tidyverse)
d <- d %>%
    mutate(cloud = if_else(Weather == "B", 1, 0),
           rain = if_else(Weather == "C", 1, 0)
           )
sapply(d, class)

data <- list(
    I=nrow(d),
    A=d$A,
    Score=d$Score/200,
    cloud=d$cloud,
    rain=d$rain,
    Y=d$Y
)
# MCMCの実行
fit <- stan(
    file = "exercise/ex4_hayashi.stan",
    data = data,
    seed = 1234
)
# 結果の表示
print(fit, pars="b")
dim(fit)


# (5)
load('output/result-model5-6.RData')
ms <- rstan::extract(fit)
dim(ms$m_pred)

# 可視化の準備
d_qua <- t(apply(ms$m_pred, 2, quantile, prob=c(0.1, 0.5, 0.9)))
colnames(d_qua) <- c('p10', 'p50', 'p90')
d_qua <- data.frame(d, d_qua)
head(d_qua)
d_qua$A <- as.factor(d_qua$A)

# 可視化
p <- ggplot(data=d_qua, aes(x=M, y=p50, ymin=p10, ymax=p90, shape=A, fill=A))
#p <- p + theme_bw(base_size=18) + theme(legend.key.height=grid::unit(2.5,'line'))
p <- p + coord_fixed(ratio=1, xlim=c(10, 80), ylim=c(10, 80))
p <- p + geom_pointrange(size=0.8)
p <- p + geom_abline(aes(slope=1, intercept=0), color='black', alpha=3/5, linetype='31')
p <- p + scale_shape_manual(values=c(21, 24))
#p <- p + scale_fill_manual(values=c('white', 'grey70'))
p <- p + labs(x='Observed', y='Predicted')
#p <- p + scale_x_continuous(breaks=seq(from=0, to=70, by=10))
#p <- p + scale_y_continuous(breaks=seq(from=0, to=70, by=10))

p

# (6)
d <- read.csv(file='input/data3a.csv')
head(d)
dim(d)
unique(d$f)
unique(d$x)

# f列の変換
d_conv <- data.frame(X=c(0, 1))
rownames(d_conv) <- c('C', 'T')

data <- list(
    N=nrow(d),
    X=d$x,
    F=d_conv[d$f, ],
    Y=d$y
)

 # MCMCの実行
fit <- stan(
    file = "exercise/ex6_hayashi.stan",
    data = data,
    seed = 123
)

print(fit, pars="b")


# (7)
d <- read.csv(file='input/data4a.csv')
head(d)
length(d$N)
unique(d$N)
unique(d$y)

# f列の変換
d_conv <- data.frame(X=c(0, 1))
rownames(d_conv) <- c('C', 'T')

data <- list(
    I=nrow(d),
    X=d$x,
    F=d_conv[d$f, ],
    N=d$N,
    Y=d$y
)

 # MCMCの実行
fit <- stan(
    file = "exercise/ex7_hayashi.stan",
    data = data,
    seed = 123
)

print(fit)

ms <- rstan::extract(fit)

# 可視化の準備
d_qua <- t(apply(ms$y_pred, 2, quantile, prob=c(0.1, 0.5, 0.9)))
colnames(d_qua) <- c('p10', 'p50', 'p90')
d_qua <- data.frame(d, d_qua)
head(d_qua)


# 可視化
p <- ggplot(data=d_qua, aes(x=y, y=p50, ymin=p10, ymax=p90, shape=f, fill=f))
#p <- p + theme_bw(base_size=18) + theme(legend.key.height=grid::unit(2.5,'line'))
p <- p + coord_fixed(ratio=1, xlim=c(0, 8), ylim=c(0, 8))
p <- p + geom_pointrange(size=0.8)
p <- p + geom_abline(aes(slope=1, intercept=0), color='black', alpha=3/5, linetype='31')
p <- p + scale_shape_manual(values=c(21, 24))
#p <- p + scale_fill_manual(values=c('white', 'grey70'))
p <- p + labs(x='Observed', y='Predicted')
#p <- p + scale_x_continuous(breaks=seq(from=0, to=70, by=10))
#p <- p + scale_y_continuous(breaks=seq(from=0, to=70, by=10))

p

