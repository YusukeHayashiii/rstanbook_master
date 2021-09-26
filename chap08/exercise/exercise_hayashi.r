getwd()
setwd("C:/Users/mem81/Desktop/R_files/rstanbook_master/chap08")
library(rstan)

# (1)
load("output/result-model8-2.RData")
fit2
ms <- rstan::extract(fit2)
N_mcmc <- length(ms$lp__)
X_new <- 0:max(d$X)
N_X <- length(X_new)
dim(ms$a)
N_mcmc
N_X
X_new

## 予測分布に相当する乱数を生成
d2 <- NULL

for (k in 1:K) {
    y_base_mcmc2 <- as.data.frame(matrix(nrow=N_mcmc, ncol=N_X))
    for (i in 1:N_X) {
        y_base_mcmc2[,i] <- rnorm(N_mcmc, mean=ms$a[,k] + ms$b[,k] * X_new[i], sd=ms$s_Y)
    }
    d2 <- rbind(d2, data.frame(X=X_new, 
                               t(apply(y_base_mcmc2, 2 , quantile, probs=c(0.025, 0.5, 0.975))), 
                               KID=k))

}
dim(y_base_mcmc2)
length(ms$a[,1])
head(d2)
dim(d2)
colnames(d2) <- c('X', 'p2.5', 'p50', 'p97.5', 'KID')
d$KID <- as.factor(d$KID)
d2$KID <- as.factor(d2$KID)
d2
head(d)

## 可視化
p <- ggplot(d, aes(x=X, y=Y, shape=KID)) # KIDで形を分ける
p <- p + theme_bw(base_size=18) + theme(legend.key.width=grid::unit(2.5, 'line'))
p <- p + facet_wrap(~KID) # KIDの要素でグラフを二次元に区分けする
p <- p + geom_ribbon(data=d2, aes(x=X, y=p50, ymin=p2.5, ymax=p97.5), fill='black', alpha=1/5)
p <- p + geom_line(data=d2, aes(y=p50), size=1, alpha=0.8)
p <- p + geom_point(size=3)
p <- p + scale_shape_manual(values=c(16,2,4,9))
p <- p + labs(x='X', y='Y', shape='KID')
p


# (2)
load("output/result-model8-3.RData")
print(fit3, pars = c("a0","b0"))
ms2 <- rstan::extract(fit3)
N_mcmc <- length(ms2$lp__)
X_new <- 0:max(d$X)
N_X <- length(X_new)
head(d)


## 予測分布に相当する乱数を生成
d2 <- NULL

for (k in 1:K) {
    y_base_mcmc2 <- as.data.frame(matrix(nrow=N_mcmc, ncol=N_X))
    for (i in 1:N_X) {
        y_base_mcmc2[,i] <- rnorm(N_mcmc, mean=ms2$a[,k] + ms2$b[,k] * X_new[i], sd=ms2$s_Y)
    }
    d2 <- rbind(d2, data.frame(X=X_new, 
                               t(apply(y_base_mcmc2, 2 , quantile, probs=c(0.025, 0.5, 0.975))), 
                               KID=k))

}
colnames(d2) <- c('X', 'p2.5', 'p50', 'p97.5', 'KID')
d$KID <- as.factor(d$KID)
d2$KID <- as.factor(d2$KID)
head(d2)

## 可視化
p <- ggplot(d, aes(x=X, y=Y, shape=KID)) # KIDで形を分ける
p <- p + theme_bw(base_size=18) + theme(legend.key.width=grid::unit(2.5, 'line'))
p <- p + facet_wrap(~KID) # KIDの要素でグラフを二次元に区分けする
p <- p + geom_ribbon(data=d2, aes(x=X, y=p50, ymin=p2.5, ymax=p97.5), fill='black', alpha=1/5)
p <- p + geom_line(data=d2, aes(y=p50), size=1, alpha=0.8)
p <- p + geom_point(size=3)
p <- p + scale_shape_manual(values=c(16,2,4,9))
p <- p + labs(x='X', y='Y', shape='KID')
p


# (3)
# 間違えて右のグラフを再現してしまったので、左のグラフの再現は解答をなぞった

## 可視化用データセットの準備
load("output/result-model8-5.RData")
print(fit5, pars=c("a","b"))
ms3 <- rstan::extract(fit5)
N_mcmc <- length(ms3$lp__)

## aについて行う
d_est <- data.frame(1:N_mcmc, ms3$a)
head(d_est)
colnames(d_est) <- c('mcmc', paste0('a', 1:30))
## d_mode: a1-a30のそれぞれについて、最頻値のXとYを格納したデータフレーム
d_mode <- data.frame(t(apply(ms3$a, 2, function(x) {
  dens <- density(x)
  mode_i <- which.max(dens$y)
  mode_x <- dens$x[mode_i]
  mode_y <- dens$y[mode_i]
  c(mode_x, mode_y)
})))
colnames(d_mode) <- c('X', 'Y')

## d_melt: d_estをaで縦持ちにしている
d_melt <- reshape2::melt(d_est, id=c('mcmc'), variable.name='X')

head(d_mode)
dim(d_mode)
head(d_melt)
dim(d_melt)

## 可視化
p <- ggplot()
p <- p + theme_bw(base_size=18)
## KDEを描画。X列の要素ごとにグラフを描く
p <- p + geom_density(data=d_melt, aes(x=value, group=X), fill='black', color='black', alpha=0.15)
## 最頻値を示す点線を描く
p <- p + geom_segment(data=d_mode, aes(x=X, xend=X, y=Y, yend=0), color='black', linetype='dashed', alpha=0.6)
## 最頻値のX軸に目盛を刻む
p <- p + geom_rug(data=d_mode, aes(x=X), sides='b')
p <- p + labs(x='value', y='density')
p <- p + scale_x_continuous(breaks=seq(from=-4, to=4, by=2))
p


## bについて行う
d_est <- data.frame(1:N_mcmc, ms3$b)
head(d_est)
colnames(d_est) <- c('mcmc', paste0('b', 1:30))
## d_mode: a1-a30のそれぞれについて、最頻値のXとYを格納したデータフレーム
d_mode <- data.frame(t(apply(ms3$b, 2, function(x) {
  dens <- density(x)
  mode_i <- which.max(dens$y)
  mode_x <- dens$x[mode_i]
  mode_y <- dens$y[mode_i]
  c(mode_x, mode_y)
})))
colnames(d_mode) <- c('X', 'Y')

## d_melt: d_estをaで縦持ちにしている
d_melt <- reshape2::melt(d_est, id=c('mcmc'), variable.name='X')

head(d_mode)
dim(d_mode)
head(d_melt)
dim(d_melt)

## 可視化
q <- ggplot()
q <- q + theme_bw(base_size=18)
## KDEを描画。X列の要素ごとにグラフを描く
q <- q + geom_density(data=d_melt, aes(x=value, group=X), fill='black', color='black', alpha=0.15)
## 最頻値を示す点線を描く
q <- q + geom_segment(data=d_mode, aes(x=X, xend=X, y=Y, yend=0), color='black', linetype='dashed', alpha=0.6)
## 最頻値のX軸に目盛を刻む
q <- q + geom_rug(data=d_mode, aes(x=X), sides='b')
q <- q + labs(x='value', y='density')
q <- q + scale_x_continuous(breaks=seq(from=-4, to=4, by=2))
q
