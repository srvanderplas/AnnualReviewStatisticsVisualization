# scatterplot vs line plot
library(nullabor)
library(mnormt)
library(random)
library(ggplot2)
library(tidyr)
library(dplyr)

gen_true_data <- function(n, r, dep=TRUE, a=0.6, s=1.5) {
  d <- data.frame(rmnorm(n, c(0, 0), matrix(c(1, r, r, 1), 2, 2))) ##True plot, 0.5 for sigma off-diagonals requires ~300 iterations for all lineups.
  #d$e1 <- 0
  #d$e2 <- 0
  if (dep) {
    #randomwalk <- matrix(0, n, 2)
    #randomwalk[,1] <- a*cumsum(d[,1]) ##A pair of (hopefully) correlated random walks
    #randomwalk[,2] <- a*cumsum(d[,2])
    #d <- data.frame(randomwalk)
    d$X1 <- as.vector(arima.sim(list(ar=a), n, innov=d$X1, sd=s))
    d$X2 <- as.vector(arima.sim(list(ar=a), n, innov=d$X2, sd=s))
  }
  m <- mean(c(d$X1, d$X2))
  s <- sd(c(d$X1, d$X2))
  d$X1 <- scale(d$X1, m, s)
  d$X2 <- scale(d$X2, m, s)
  d$t <- 1:n
  return(d)
}

gen_null_data <- function(n, m, dep=TRUE, a=1) {
  nd <- NULL #matrix(0, (m-1)*n, 2) ##2 columns of independent random walks, after n steps restart and begin a new walk.
  for(i in 1:(m-1)) { ##19 null plots required
    #    for(j in 1:2) { ##Each null plot is a pair
    d <- data.frame(rmnorm(n, c(0, 0), matrix(c(1, 0, 0, 1), 2, 2)))
    if (dep) {
      
      d$X1 <- as.vector(arima.sim(list(ar=a), n))
      d$X2 <- as.vector(arima.sim(list(ar=a), n))
      
      #      for(k in 1:n) { ##The nulls go for n steps
      #        nd[(i-1)*n+k, j] <- null[k] ##for n = 10, i = 1 saves to 1-10, i = 2 saves to 11-20 etc. for each column j
      #      }
      #  d <- cbind(null1, null2)
    }
    mn <- mean(c(d$X1, d$X2))
    s <- sd(c(d$X1, d$X2))
    d$X1 <- scale(d$X1, mn, s)
    d$X2 <- scale(d$X2, mn, s)
    nd <- rbind(nd, d)
    #    }
  } 
  #colnames(nd) <- c("X1", "X2")
  #nulls <- data.frame(nd)
  nd$.n <- rep(1:(m-1), each = n) 
  nd$t <- 1:n
  return(nd)
}

set.seed(1000)
real <- gen_true_data(30, -0.5, dep=TRUE, 0.7)
cor(real$X1, real$X2)

lm <- real %>% gather(variable, value, -t)
ggplot(data=lm, aes(x=t, y=value, colour=variable)) + 
  geom_line()
ggplot(data=real, aes(X1, X2)) + geom_point()

nulls <- gen_null_data(30, 4, dep=TRUE, a=0.7)
l <- lineup(true=real, samples=nulls, n=4)
summarise(group_by(l, .sample), cor(X1, X2))

ggplot(data=l, aes(X1, X2)) + 
  geom_point() + 
  facet_wrap(~.sample, scales="free", ncol=4) + 
  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank())

lm <- l %>% gather(variable, value, X1, X2)
ggplot(data=lm, aes(x=t, y=value, colour=variable)) + 
  geom_line() + 
  facet_wrap(~.sample, scales="free_y", ncol=4) + 
  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  legend.position="none")
