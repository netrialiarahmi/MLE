# generate some pareto data with known parameter values
set.seed(123)
x = rpareto(100, 1, 2) # pareto data with a=1, b=2

# histogram
hist(x, freq=FALSE, col='tan')
lines(density(x), col='red', lwd=2)

# likelihood function for pareto distribution
pareto_lik = function(x, a, b) {
  ifelse(x >= b, a*b^a/x^(a+1), 0)
}

# log-likelihood function for pareto distribution
llik = function(x, par) {
  a = par[1]
  b = par[2]
  n = length(x)
  # log of the pareto likelihood
  ll = sum(log(pareto_lik(x, a, b)))
  # return the negative to maximize rather than minimize
  return(-ll)
}

# negative log-likelihood curve
curve(-1*llik(x=x, par=c(2, 10)), from=0.1, to=4, xlab="a", ylab="-log L(a,b)", lwd=2)

# call optim with the starting values 'par',
# the function (here 'llik'),
# and the observations 'x'
res0 = optim(par=c(1, 1), llik, x=x)

library(knitr)
print(kable(
  cbind('direct' = c('a'=1, 'b'=2),
        'optim' = res0$par), digits=3))

