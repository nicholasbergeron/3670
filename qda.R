#!/usr/bin/env Rscript

# load data
train <- as.data.frame(read.table(file='zip.train', header=FALSE))
test <- as.data.frame(read.table(file='zip.test', header=FALSE))
threes <- as.matrix(subset(train, V1==3))
fives <- as.matrix(subset(train, V1==5))

# disregard labels
threes <- threes[, -1]
fives <- fives[, -1] 

# get mu
mean_threes <- colMeans(threes)
mean_fives <- colMeans(fives)

# get sigma
N <- nrow(threes) + nrow(fives)
K <- 2  # number of classes (2)

# get covariance for each class
sig_threes <-  matrix(0, 256, 256)
for (i in (1:nrow(threes))) {
	sig_threes = sig_threes + (threes[i, ] - mean_threes) %*% t(threes[i, ] - mean_threes)
}

sig_fives <- matrix(0, 256, 256)
for (i in (1:nrow(fives))) {
	sig_fives = sig_fives + (fives[i, ] - mean_fives) %*% t(fives[i, ] - mean_fives)
}

# get pi's
pi_threes <- nrow(threes) / N
pi_fives <- nrow(fives) / N

# we just want to do these computations once, not thousands of times
siginv_threes <- solve(sig_threes)
siginv_fives <- solve(sig_fives)
s <- svd(sig_threes)
sig_det_threes <- log(sum(s$d))
s <- svd(sig_fives)
sig_det_fives <- log(sum(s$d))

# quadratic discriminant function
discriminant <- function(x, siginv, sig_det, mu, class_pr) {
	return( -0.5 * t(x) %*% siginv %*% x + t(x) %*% siginv %*% mu - 0.5 * t(mu) %*% siginv %*% mu - 0.5 * log(sig_det) + log(class_pr))
}

# compute the desired matrix
compute_matrix <- function(samples) {
	n_samples <- nrow(samples)
	results_matrix <- matrix(0, n_samples, 2)
	for (i in (1:n_samples)) {
		my_sample <- samples[i, ]

		d_t <- discriminant(my_sample, siginv_threes, sig_det_threes, mean_threes, pi_threes)
		d_f <- discriminant(my_sample, siginv_fives, sig_det_fives, mean_fives, pi_fives)
		results_matrix[i, ] = results_matrix[i, ] + c(d_t, d_f)
	}

	return(results_matrix)
}

results_matrix <- compute_matrix(threes)
# this is the matrix question 2 asked us to compute
# results_matrix

# question 3
test_qda <- function(data) {
	classes <- data[, 1]  # just the labels
	training_samples <- data[, -1] # disregard label

	guesses <- matrix(0, nrow(training_samples), 1)

	for (i in 1:nrow(training_samples)) {
		class <- classes[i]
		my_sample <- training_samples[i, ]
		d_t <- discriminant(my_sample, siginv_threes, sig_det_threes, mean_threes, pi_threes)
		d_f <- discriminant(my_sample, siginv_fives, sig_det_fives, mean_fives, pi_fives)

		if (d_t > d_f) {
			guess_class <- 3
		} else {
			guess_class <- 5
		}

		guesses[i] <- (guess_class == class)
	}

	# sum over true values 
	guesses
	error_rate <- sum(guesses) / nrow(guesses)
	return(error_rate)
}

# training error rate
r_train<- test_qda(as.matrix(subset(train, V1==3 | V1==5)))
r_train

# test error rate
r_test <- test_qda(as.matrix(subset(test, V1==3 | V1==5)))
r_test
