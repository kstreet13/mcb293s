# --------------------------------
# MCB 293S
# Week 4
# Linear Models
# --------------------------------

# read in the data from the CSV file
dat <- Seatbelts
dat <- as.data.frame(Seatbelts)

# Exploratory Data Analysis

hist(dat$DriversKilled)
hist(dat$drivers)
hist(dat$PetrolPrice)
hist(dat$law)

plot(dat$drivers, dat$DriversKilled)

plot(dat$kms, dat$DriversKilled)

plot(dat$PetrolPrice, dat$DriversKilled)

plot(dat$law, dat$DriversKilled)

### Did the seatbelt law help?

# ----------------------------
# 1. Classic Linear Regression
# ----------------------------

fit <- lm(DriversKilled ~ drivers + kms + PetrolPrice + law, data = dat)

fit

summary(fit)

# ------------------------------
# 2. Bootstrap Linear Regression
# ------------------------------

# setup
n <- nrow(dat)

# get the estimates from the actual data
beta.hat <- coef(fit)

# set number of resamples
B <- 10000

beta.boots <- sapply(1:B, function(i){
    boot.samp.ind <- sample(1:n, replace = TRUE)
    boot.dat <- dat[boot.samp.ind,]
    boot.beta.hat <- coef(lm(DriversKilled ~ drivers + kms + PetrolPrice + law, data = boot.dat))
    return(boot.beta.hat)
})
beta.boots <- t(beta.boots)
beta.boots <- data.frame(beta.boots)

# or, equivalently

beta.boots <- matrix(NA, nrow = B, ncol = 5)
for(i in 1:B){
    boot.samp.ind <- sample(1:n, replace = TRUE)
    boot.dat <- dat[boot.samp.ind,]
    boot.beta.hat <- coef(lm(DriversKilled ~ drivers + kms + PetrolPrice + law, data = boot.dat))
    beta.boots[i,] <- boot.beta.hat
}
beta.boots <- data.frame(beta.boots)


hist(beta.boots$drivers, breaks=50, col='darkred')
bounds <- quantile(beta.boots$drivers, probs = c(.025, .975))
abline(v=bounds, lwd=2)
abline(v=beta.hat[2], lwd=2, col='green4')

hist(beta.boots$law, breaks=50, col='darkred')
bounds <- quantile(beta.boots$law, probs = c(.025, .975))
abline(v=bounds, lwd=2)
abline(v=beta.hat[5], lwd=2, col='green4')


# ---------------------------------
# 3. Repeat with different outcomes
# ---------------------------------

fit <- lm(front ~ drivers + kms + PetrolPrice + law, data = dat)
fit
summary(fit)


fit <- lm(front ~ drivers + kms + PetrolPrice + law, data = dat)
fit
summary(fit)
