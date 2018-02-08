# --------------------------------
# MCB 293S
# Week 3
# A/B Testing
# --------------------------------

# read in the data from the Excel file
require(readxl)
dat <- read_excel("~/Projects/mcb293s/Week-3/two_groups.xlsx")

# format as separate vectors
group1 <- dat$`Group 1`
group1 <- group1[ !is.na(group1) ] # remove NA values
group2 <- dat$`Group 2`

# format as data.frame with column for group
df <- data.frame(value = c(group1,group2), 
                 label = c( rep(1,length(group1)), rep(2,length(group2))))

# histograms
hist(group1, xlim = range(df$value))
hist(group2, xlim = range(df$value))

#### Do these samples have the same mean?
#### Do they have the same variance?

# --------------------
# 1. Two-sample T-test
# --------------------

t.test(group1, group2)

t.test( df$value[df$label==1], df$value[df$label==2] )

# -----------------------------------------
# 2. Permutation test (difference in means)
# -----------------------------------------

# setup
get_stat <- function(values, labels){
    return(abs(mean(values[labels==1]) - mean(values[labels==2])))
}

stat <- get_stat(df$value, df$label)

# set number of permutations
B <- 10000

stats.permute <- sapply(1:B, function(i){
    labels.permute <- sample(df$label)
    stat.i <- get_stat(df$value, labels.permute)
    return(stat.i)
})

# or, equivalently

stats.permute <- rep(NA, B)
for(i in 1:B){
    labels.permute <- sample(df$label)
    stat.i <- get_stat(df$value, labels.permute)
    stats.permute[i] <- stat.i
}

hist(stats.permute)
abline(v = stat, lwd=3, col='green')

# think about which values are "more extreme"
sum(stats.permute >= stat) / B  # p-value


# ---------------------------------------------
# 3. Permutation test (difference in variances)
# ---------------------------------------------

# setup
get_stat <- function(values, labels){
    return(mean(values[labels==1] >= 6) - mean(values[labels==2] >= 6))
    #return(abs(var(values[labels==1]) - 
    #               var(values[labels==2])))
}
stat <- get_stat(df$value, df$label)

# set number of permutations
B <- 10000

stats.permute <- sapply(1:B, function(i){
    labels.permute <- sample(df$label)
    stat.i <- get_stat(df$value, labels.permute)
    return(stat.i)
})

# or, equivalently

stats.permute <- rep(NA, B)
for(i in 1:B){
    labels.permute <- sample(df$label)
    stat.i <- get_stat(df$value, labels.permute)
    stats.permute[i] <- stat.i
}

hist(stats.permute)
abline(v=stat, lwd=3, col='green')

# think about which values are "more extreme"
sum(stats.permute >= stat) / B  # p-value


# --------------------
# 4. Linear regression
# --------------------

x <- rnorm(100)
y <- x + rnorm(100)

fit <- lm(y ~ x)

fit
summary(fit)

plot(x, y, asp=1)
abline(coefficients(fit), lwd=2, col='green')


