# the digit we care about
mydigit <- 7

# load training data as a dataframe
train <- as.data.frame(read.table(file="zip.train", header=FALSE))
# load all the data for mydigit in to the matrix X
X <- as.matrix(subset(train, V1==mydigit))
# disregard labeling
X <- X[,-1]

# function to visualize a vector
zimage1 <- function(vec) {
  # create a 16x16 matrix of the 256-color values
  img <- matrix(vec[1:256], nrow=16, ncol=16)
  img <- t(apply(-img, 1, rev))
  # map each element to a grayscale value
  image(img, col=grey(seq(0,1,length=256)))
}

# view an arbitrary training example of mydigit
zimage1(X[69,])

# take the average of each column of X
# in effect this is the ave. grayscale value across all of the training data
# where the digit is mydigit
Xave = colMeans(X)
# visualize the average
zimage1(Xave)

# get average value of each intensity (column)
mu <- matrix(Xave, ncol=1)
# num. rows of X
n = dim(X)[1]

# compute the variance 
Sig <- 1/n*t(X) %*% X - mu%*%t(mu)

# get the singular value decomposition of the variance
s <- svd(Sig)

# plot the diagonal part of the svd
plot(s$d,
     main="d component of SVD")
