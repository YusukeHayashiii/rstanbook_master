getwd()
setwd("C:/Users/mem81/Desktop/R_files/rstanbook_master/chap08")
setwd("C:/Users/s44990/Desktop/R_files/RStanBook-master/chap08")
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


# (4)
d <- read.csv(file='input/data-attendance-4-2.txt')
head(d)
d_person <- tapply(d$Y, d$PersonID, mean)
class(d_person)
bw <- (max(d_person)-min(d_person))/30
p <- ggplot(data=data.frame(X=d_person), aes(X))
p <- p + geom_histogram(color='grey20', binwidth=bw)
p <- p + geom_line(eval(bquote(aes(y=..count..*.(bw)))), stat='density')
p

d_course <- tapply(d$Y, d$CourseID, mean)
d_course
bw <- (max(d_course)-min(d_course))/30
p <- ggplot(data=data.frame(X=d_course), aes(X))
p <- p + geom_histogram(color='grey20', binwidth=bw)
p <- p + geom_line(eval(bquote(aes(y=..count..*.(bw)))), stat='density')
p


# (5)
d1 <- read.csv('input/data-attendance-4-1.txt')
d2 <- read.csv('input/data-attendance-4-2.txt')
N <- 50
C <- 10
I <- nrow(d2)
conv <- c(0, 0.2, 1)
names(conv) <- c('A', 'B', 'C')
data <- list(N=N, C=C, I=I, A=d1$A,
            Score=d1$Score/200,
            PID=d2$PersonID, 
            CID=d2$CourseID, 
            W=conv[d2$Weather], 
            Y=d2$Y)
fit <- stan(file='exercise/ex5.stan', 
            data=data,
            pars=c('b', 'b_P', 'b_C', 's_P', 's_C', 'q'), 
            seed=1234)

print(fit, pars=c('b', 'b_P', 'b_C'))

## バイオリンプロットで可視化
ms <- rstan::extract(fit)
N_mcmc <- length((ms$lp__))

param_names <- c('mcmc', paste0('b', 1:4))
d_est <- data.frame(1:N_mcmc, ms$b)
colnames(d_est) <- param_names
#d_qua <- data.frame.quantile.mcmc(x=param_names[-1], y_mcmc=d_est[,-1])

d_melt <- reshape2::melt(d_est, id=c('mcmc'), variable.name='X')
d_melt$X <- factor(d_melt$X, levels=rev(levels(d_melt$X)))

dim(d_est)

p <- ggplot()
p <- p + theme_bw(base_size=18)
p <- p + coord_flip()
p <- p + geom_violin(data=d_melt, aes(x=X, y=value), fill='white', color='grey80', size=2, alpha=0.3, scale='width')
p <- p + labs(x='parameter', y='value')
p <- p + scale_y_continuous(breaks=seq(from=-2, to=6, by=2))
p


# (6)

d <- read.csv("input/data7a.csv")
head(d)
dim(d)
print(unique(d$id))

## モデル式

# Y[n] ~ Binomial(8, q[n])
# q[n] ~ inv_logit(a[n])
# a[n] ~ Nromal(a_個体平均, s_a)

## MCMCの実行
N <- nrow(d)
M <- 8
Y <- d$y

data <- list(N=N, M=M, Y=Y)
# data
fit <- stan(file="exercise/ex6_hayashi.stan",
            data = data,
            seed=1212)

print(fit, pars=c("a0", "q"))


# (7)
d<- read.csv("input/d1.csv")
head(d)
dim(d)
unique(d$pot)
unique(d$f)
unique(d$y)

## モデル式
# n...1:N(個体数), k...1:K(植木鉢数)
# Y[n] ~ Poisson(lambda[n])
# lambda[n] ~ exp(a[pot[n]] + b[pot[n]]*F[n])
# a[k] ~ normal(a0(全体平均), s_a)
# b[k] = normal(b0(全体平均), s_b)


# f列とpotの変換
d_conv_f <- data.frame(X=c(0, 1))
rownames(d_conv_f) <- c('C', 'T')

d_conv_pot <- data.frame(X=c(1:10))
rownames(d_conv_pot) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

data <- list(
    N=nrow(d),
    K=length(unique(d$pot)),
    F=d_conv_f[d$f, ],
    pot = d_conv_pot[d$pot, ],
    Y=d$y
)

fit <- stan(file="exercise/ex7_hayashi.stan",
            data=data,
            seed=111)

print(fit, pars=c("b", "c"))