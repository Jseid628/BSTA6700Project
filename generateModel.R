factors <- list(
  c = seq(0.5, 1, length.out = t_total+1) # hand-code factor (common part)
)
factors$i <- matrix(0,nrow=t_total+1, ncol = k_total) # outcome idiosyncratic part of factor
set.seed(1)
for ( k in 1:k_total ) {
  series <- arima.sim(model = list(ar = 0.5), n = t_total+1) # independent AR series across outcomes
  factors$i[,k] <- 0.5 + (series - min(series)) * (1 - 0.5) / (max(series) - min(series))  # rescale so factors are in same range
  factors$i[,k] <- sort(factors$i[,k])
}

set.seed(1)
mu$i <- matrix(0,nrow=n, ncol = k_total) # outcome idiosyncratic part of factor
for (k in (1:k_total)) {
  mu$i[,k] <- rnorm(n)
  mu$i[,k] <- 1 + mu$i[,k] * (5 - 1) / (max(mu$i[,k] ) - min(mu$i[,k] )) # rescale so bias is in same range
}
cor(mu$i[,1],mu$c)
cor(mu$i[,1],mu$i[,2])

theo_w <- synth_qp(mu$c[trt], as.matrix(mu$c[-trt])); # confirm we can find the oracle weights
SCMbias(mu$c[-trt]*factors$c[t_total+1], mu$c[trt]*factors$c[t_total+1],theo_w)


mu$i[trt,] <- t(theo_w)%*%mu$i[-trt,] # overwrite the idiosyncratic loadings to ensure oracle weights exist

theo_w <- synth_qp(c(mu$c[trt],mu$i[trt,]), as.matrix(cbind(mu$c[-trt],mu$i[-trt,]))); # confirm we can find the oracle weights
SCMbias(mu$c[-trt]*factors$c[t_total+1] + mu$i[-trt,1]*factors$i[t_total+1,1],
        mu$c[trt]*factors$c[t_total+1] + mu$i[trt,1]*factors$i[t_total+1,1], 
        theo_w)

# create the model components
factors$i[,1] <- factors$c
# Repeat each row of factors N times (expand to NT x K)
factors_expanded <- factors$i[rep(1:(t_total+1), each = n), ]
mu$i[,1] <- mu$c
# Repeat mu T times (expand to NT x K)
mu_expanded <- mu$i[rep(1:n, (t_total+1)), ]

# Element-wise multiplication
model <- rho * as.vector(factors$c %x% mu$c) + (1-rho) * factors_expanded * mu_expanded

return (model)