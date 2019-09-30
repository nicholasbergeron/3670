# function generates a normal vector with mean mean and variance A
rnormvec <- function(nsamples=1, A, mean=0) {
  m = dim(A)[2]  # m := number of columns of A
  z = matrix(rnorm(nsamples*m),
             nrow=m,
             ncol=nsamples)
  x = (A %*% z) + rep(mean, times=nsamples)
  return(t(x))
}

# number of samples
N <- 1000

# flattened versions of the matrices we want to generate
v1 = c(1, 3, 3, 7)
v2 = c(2, 0, 1, 9)

# turn v1, v2 into 2x2 square matrices A1, A2
A1 <- matrix(v1, nrow=2)
A2 <- matrix(v2, nrow=2)

# 2x1 mean vectors
mu1 <- c(100, 100)
mu2 <- c(0, 0)

# probabilities
p1 = 0.35
p2 = 1 - p1


# filter the N samples by probability
t <- sample(c(1,2), N, prob=c(p1, p2), replace=TRUE)
n1 <- sum(t==1); n2 <- N - n1

# generate two normal vectors
x1 <- rnormvec(n=n1, A1, mu1)
x2 <- rnormvec(n=n2, A2, mu2)

plot(rbind(x1, x2, mu1, mu2),
     asp=1,
     col=c(rep('orange', n1),
           rep('blue', n2),
           'black',
           'black'),
     xlab="x_1",
     ylab="x_0",
     main="It's easy to tell which distribution these samples came from.")

v1 <- c(10, 6, 7, 2)  # the variance is much greater in magnitude
v2 <- c(8, 4, 8, 10)
A1 <- matrix(v1, nrow=2)
A2 <- matrix(v2, nrow=2)
mu1 <- c(10, -5)
mu2 <- c(4, -2)
p1 = 0.47
p2 = 1 - p1
t <- sample(c(1,2), N, prob=c(p1, p2), replace=TRUE)
n1 <- sum(t==1); n2 <- N - n1
x1 <- rnormvec(n=n1, A1, mu1)
x2 <- rnormvec(n=n2, A2, mu2)

plot(rbind(x1, x2, mu1, mu2),
     asp=1,
     col=c(rep('orange', n1),
           rep('blue', n2),
           'black',
           'black'),
     xlab="x_1",
     ylab="x_0",
     main="It's hard to tell which distribution these samples came from.")
