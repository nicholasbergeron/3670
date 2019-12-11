#!/usr/bin/env Rscript

# load data
train <- as.data.frame(read.table(file='zip.train', header=FALSE))
test <- as.data.frame(read.table(file='zip.test', header=FALSE))

# load data into matrices
threes <- as.matrix(subset(train, V1==3))
fives <- as.matrix(subset(train, V1==5))
tr_threes <- as.matrix(subset(test, V1==3))
tr_fives <- as.matrix(subset(test, V1==5))

# disregard labels
threes <- threes[, -1]
fives <- fives[, -1] 
tr_threes <- tr_threes[, -1]
tr_fives <- tr_fives[, -1] 

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

# unbiased covariance estimator
sig <- (sig_threes + sig_fives) / (N -K)
siginv <- solve(sig) # compute sigma inverse so we don't have to do it again later

# get pi's
pi_threes <- nrow(threes) / N
pi_fives <- nrow(fives) / N

# define discriminant functions
discriminant <- function(x, siginv, mu, class_pr) {
	return(t(x) %*% siginv %*% mu - 0.5 %*% t(mu) %*% siginv %*% mu + log(class_pr))
}

# compute the desired matrix
compute_matrix <- function(samples) {
	n_samples <- nrow(samples)
	results_matrix <- matrix(0, n_samples, 2)
	for (i in (1:n_samples)) {
		my_sample <- samples[i, ]

		d_t <- discriminant(my_sample, siginv, mean_threes, pi_threes)
		d_f <- discriminant(my_sample, siginv, mean_fives, pi_fives)
		results_matrix[i, ] = results_matrix[i, ] + c(d_t, d_f)
	}

	return(results_matrix)
}

results_matrix <- compute_matrix(threes)
# this is the matrix question 2 asked us to compute
# results_matrix

# begin question 3 stuff

test_lda <- function(data) {
	classes <- data[, 1]  # just the labels
	training_samples <- data[, -1] # disregard label

	guesses <- matrix(0, nrow(training_samples), 1)

	for (i in 1:nrow(training_samples)) {
		class <- classes[i]
		my_sample <- training_samples[i, ]
		d_t <- discriminant(my_sample, siginv, mean_threes, pi_threes)
		d_f <- discriminant(my_sample, siginv, mean_fives, pi_fives)

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
r_train<- test_lda(as.matrix(subset(train, V1==3 | V1==5)))
r_train

# test error rate
r_test <- test_lda(as.matrix(subset(test, V1==3 | V1==5)))
r_test
