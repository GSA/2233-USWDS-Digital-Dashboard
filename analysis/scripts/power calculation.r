## POWER CALCULATION WITH VARIABLE TREATMENT EFFECTS

# Simulation parameters.
# How do each of these 3 affect the power of the test of
#   H_0 : average treatment effect = 0?
n <- 100
var.e <- 0.45                         # error variance
ATE = 0.10                            # average treatment effect in population

# Individual treatment effects in sample
# effect <- runif(n, min=0, max=2*ATE)  
# effect <- rnorm(n, mean=ATE, sd=0.05)
effect <- rgamma(n, shape=ATE)        # very right skewed

B <- 5000
betahat <- matrix(nrow=B, ncol=2, NA)
tStats <- rep(NA, B)
for (b in 1:B) {
    D <- sample(0:1, n, prob=c(0.5, 0.5), replace=TRUE) # randomize treatment
    Y <- 1 + effect*D + rnorm(n, sd=sqrt(var.e))
    reg <- lm(Y~D)
    betahat[b, ] <- coef(reg)
    tStats[b] <- coef(reg)["D"]/sqrt(vcov(reg)["D","D"])
}
colnames(betahat) <- names(coef(reg))

# Why does this line give us the power of the test?
power <- sum( abs(tStats) >= qnorm(0.975) )/B

cat(paste("\nPower|alpha=0.05 = ", power, "\n"))

cat(paste("\n    population ATE =", ATE))
cat(paste("\n        sample ATE =", mean(effect)))
cat(paste("\nmean estimated ATE =", mean(betahat[ ,"D"]), "\n"))

hist(betahat[ ,"D"], breaks=50)
