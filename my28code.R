# load data
train <- as.data.frame(read.table(file="zip.train", header=FALSE))
test <- as.data.frame(read.table(file="zip.test", header=FALSE))

# linear regression part, similar to other examples so comments minimal
tr.bc<-subset(train, V1 == 2 | V1 == 3)

X <- as.matrix(tr.bc)
t <- tr.bc[, 1]

t[t==2] <- 0
t[t==3] <- 1

X[,1]<-1

XTX <- t(X)%*%X
XTt <- t(X)%*%t

w <- solve(XTX,XTt)

that <- X%*%w

threshold <- 0.5
ghat <- that
ghat[that < threshold] <- 0
ghat[that >= threshold] <- 1

# check training error
cat("training % correct: ", length(which(t==ghat)) / length(t))

# TEST phase

te.bc<-subset(test,V1==2 | V1==3)

Xte <- as.matrix(te.bc)
Xte[,1] <- 1

tte <- te.bc[,1]

tte[tte==2] <- 0
tte[tte==3] <- 1

ttehat <- Xte%*%w

gtehat <- ttehat
gtehat[ttehat < threshold] <- 0
gtehat[ttehat >= threshold] <- 1

# check how many are correct.
cat("testing % correct: ", length(which(tte==gtehat)) / length(tte))

# knn regression part

library("FNN")

X.train <- as.matrix(train)
X.train[,1] <- 1
g.train <- train[,1]

X.test <- as.matrix(test)
X.test[,1] <- 1
g.test <- test[,1]

k_vals <- c(1, 3, 5, 7, 15)

for (k_val in k_vals) { 
  gtrain.knn <- knn(train=X.train, test=X.train, g.train, k=k_val)
  cat("Training error % for k =", k_val, ": ", sum(gtrain.knn==g.train) / length(g.train), "\n")
}

for (k_val in k_vals) {
  gtest.knn <- knn(X.train, X.test, g.train, k = k_val, prob=TRUE)
  cat("Test error % for k =", k_val, ": ", sum(gtest.knn==g.test) / length(g.test), "\n")
}
