
#### RMSE
rmse <- function(Y0,Y1,weights) {
  synthY0 <- (Y0%*%weights)
  gap <- Y1-synthY0 
  return(sqrt(mean(gap^2)))
}