# --------------------------------
# MCB 293S
# Week 2
# Hypothesis Testing
# --------------------------------

# read in the data from the CSV file
data <- read.csv('~/Projects/mcb293s/Week-2/scores.csv')

# make a column of differences
data$difference <- data$after - data$before

# histogram of differences
hist(data$difference)

#### Was the instruction helpful?
#### Is the average difference greater than 0?
#### Does mu = 0?

# -----------------------------------
# 1. (Approximate) Classical Approach
# -----------------------------------

# setup
n <- nrow(data)
x.bar <- mean(data$difference)
s2 <- var(data$difference)

abline(v = x.bar, col='red', lwd=3)

# APPROXIMATE: population variance = sample variance
# so the variance of the sample mean = s2 / n

# If we assume mu = 0, then the Central Limit Theorem tells us x.bar is drawn
# from a normal distribution with mean 0 and variance s2 / n

curve(4*dnorm(x, mean=0, sd=sqrt(s2/n)), from=-10, to=10, add=TRUE, lwd=2, col='green')

pnorm(x.bar, mean=0, sd=sqrt(s2/n))

1 - pnorm(x.bar, mean=0, sd=sqrt(s2/n))

# Alternate route:

# calculate the Z-statistic (how many standard deviations separate the
# observed mean from the assumed mean)
z.stat <- (x.bar - 0) / sqrt( s2/n )

# compare this to a standard normal distribution (with mean 0 and variance 1)
pnorm(z.stat, mean=0, sd=1)

1 - pnorm(z.stat, mean=0, sd=1) # p-value


# ------------------------------
# 2. Modern approach (bootstrap)
# ------------------------------

# setup
n <- nrow(data)
x.bar <- mean(data$difference)

# set number of resamples
B <- 10000

x.bar.boots <- sapply(1:B, function(i){
    boot.samp <- sample(data$difference, replace = TRUE)
    boot.mean <- mean(boot.samp)
    return(boot.mean)
})

# or, equivalently

x.bar.boots <- rep(NA, B)
for(i in 1:B){
    boot.samp <- sample(data$difference, replace = TRUE)
    boot.mean <- mean(boot.samp)
    x.bar.boots[i] <- boot.mean
}

hist(x.bar.boots)
abline(v=0, lwd=3, col='green')
abline(v=x.bar, lwd=3, col='red')

sum(x.bar.boots <= 0) / B  # p-value


# -----------------------------
# 3. (Exact) Classical Approach
# -----------------------------

# setup
n <- nrow(data)
x.bar <- mean(data$difference)
s2 <- var(data$difference)

# calculate the T-statistic (how many standard deviations separate the
# observed mean from the assumed mean)
t.stat <- (x.bar - 0) / sqrt( s2/n )

# The sample variance is still our best estimate of the population variance, 
# but it's just an estimate. Since the true variance could be larger (or 
# smaller), we use the more "heavy-tailed" T-distribution in place of the
# standard normal.

# compare this to a T-distribution (with n-1 degrees of freedom)
pt(t.stat, df = n-1)

1 - pt(t.stat, df = n-1) # p-value

# Notice that the p-value is larger than when we used the standard normal. The
# T-distribution makes our test more conservative (and accurate).
