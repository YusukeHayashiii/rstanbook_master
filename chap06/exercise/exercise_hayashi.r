getwd()
setwd("C:/Users/mem81/Desktop/R_files/rstanbook_master/chap06")

library(ggplot2)

# 1

##　ベルヌーイ分布に従う乱数を50個生成
set.seed(11)
sample(0:1, 50, replace=TRUE, prob=c(0.1, 0.9))

## カテゴリカル分布に従う乱数を50個生成
## K=5, theta=(0.1, 0.2, 0.25, 0.35, 0.1)
set.seed(11)
a = sample(1:5, 50, replace = TRUE, prob=c(0.1, 0.2, 0.25, 0.35, 0.1))
hist(a)


# 2
set.seed(11)
## 一様分布
runif(10, min=-1, max = 1)

## 二項分布
rbinom(10, 50, 0.4)
plot(0:50, dbinom(0:50, 50, 0.4), type="h", lwd=2)

## ベータ分布
rbeta(10, shape1=10, shape2=5)

d <- data.frame(X=seq(0, 1.0, len = 11))
p <- ggplot(data=d, aes(x=X))
p <- p + stat_function(fun=dbeta, args=list(shape1=10,shape2=5))
p

## 多項分布
rmultinom(10, 50, prob = c(0.2,0.3,0.3,0.2))

## ディリクレ分布
# install.packages("gtools")
library(gtools)
help(gtools)
rdirichlet(10, alpha=c(3,4,0.1))

## 指数分布
rexp(10, 3)

d <- data.frame(X=seq(0, 10, len = 11))
p <- ggplot(data=d, aes(x=X))
p <- p + stat_function(fun=dexp, args=list(rate=3))
p

## ポアソン分布
rpois(10, 3)
plot(0:10, dpois(0:10, 3), type="h", lwd=10)

## ガンマ分布
rgamma(10, 5, 2)

d <- data.frame(X=seq(0,10, len=11))
p <- ggplot(data=d, aes(x=X))
p <- p + stat_function(fun=dgamma, args=list(shape=5, rate=5))
p

## 正規分布
rnorm(10, 0, 3)

d <- data.frame(X=seq(-5,5, len=11))
p <- ggplot(data=d, aes(x=X))
p <- p + stat_function(fun=dnorm, args=list(mean=0, sd=3))
p

## 対数正規分布
rlnorm(10, 0, 3)

d <- data.frame(X=seq(0,10, len=11))
p <- ggplot(data=d, aes(x=X))
p <- p + stat_function(fun=dlnorm, args=list(meanlog=0, sdlog=3))
p

## 二変量正規分布
# install.packages("mvtnorm")
library(mvtnorm)
mu <- c(1,3)
sigma <- rbind(
    c(2,0.6),
    c(0.6, 0.5)
)
rmvnorm(10, mean=mu, sigma=sigma)

## コーシー分布
rcauchy(10, 1, 5)

d <- data.frame(X=seq(-10,10, len=21))
p <- ggplot(data=d, aes(x=X))
p <- p + stat_function(fun=dcauchy, args=list(location=1, scale=5))
p

## t分布
rt(10, 5)

d <- data.frame(X=seq(-5,5, len=11))
p <- ggplot(data=d, aes(x=X))
p <- p + stat_function(fun=dt, args=list(df=5))
p

## ラプラス分布
 rlaplace <- function(n) {
   u <- log(runif(n))
   v <- ifelse(runif(n)>1/2, 1, -1)
   return(u*v)
 }
 rlaplace(10)

runif(100)


# (3)
set.seed(11)
y1 <- rnorm(2000, mean=50, sd = 20)
set.seed(11)
y2 <- rnorm(2000, mean=20, sd = 15)

y <- y1-y2

d <- data.frame(Y=y)
head(d)
ggplot(data = d, mapping = aes(x = Y)) + 
    geom_density(size = 1.5) + 
    labs(title = 'カーネル密度推定')


# 4
# install.packages(("gamlss"))
library(gamlss)
help(gamlss)
rNBI(10,1,10)
# NBIが何かはよくわからず

rweibull(10, 5, 0.5)
d <- data.frame(X=seq(0,3, len=11))
p <- ggplot(data=d, aes(x=X))
p <- p + stat_function(fun=dweibull, args=list(shape=5, scale=0.5))
p
