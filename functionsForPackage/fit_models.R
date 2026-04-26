#' Given the factors and factor loadings, simulate the data
#' @param factors Factors
#' @param mu Factor loadings
#' @param n number of units
#' @param trt treatment indicator vector
#' @param k_total number of outcomes
#' @param t_total number of pre-treatment periods
#' @param variance variance of the noise term
#' @param num_timepoints Number of training timepoints to use. Defaults to t_total.
fit_models <- function(model,n,trt,k_total,t_total,variance, num_timepoints=NULL) {
  # importing the functions we want.
  # Morph reshapes the data, lq fits my model to it.
  # Need to figure out how to make these paths reproducible. - probably will need to replace with path to the files on own machine.
  morph <- reticulate::import_from_path("morphData", path = "/Users/jseid1/BSTA6700FinalProject/ScratchWork")
  lqorth <- reticulate::import_from_path("learnQorthogonal", path = "/Users/jseid1/BSTA6700FinalProject/ScratchWork")
    
 
  
  # Use only first num_timepoints if specified
  if (!is.null(num_timepoints) & t_total == 40) {
    t_train <- num_timepoints
  } else {
    t_train <- t_total
  }
  
  epsilon <- matrix(rnorm((n * t_train) * k_total, sd = sqrt(variance)), ncol = k_total)
  out <- model[1:(n * t_train),] + epsilon # (NXT) x (K) matrix of outcomes
  t_trt <- rep(trt,t_train) # select the periods for the treated unit
  out_trt <- matrix(out[which(t_trt==1),],nrow = t_train,ncol =k_total)  # T x K matrix of outcomes for the treated unit
  out_control <- out[which(t_trt==0),] # ((N-1)xT) x (K) matrix of outcomes for the control units
  
  ## calculate SCM weights
  out_trt_sep <- out_trt[,1]
  out_control_sep <- matrix(out_control[,1], nrow = n-1,ncol=t_train)
  w_sep <- synth_qp(out_trt_sep, out_control_sep);
  r.svd <- svd(rbind(out_control_sep,out_trt_sep))
  largest_svd <- r.svd$d[1]^2/sum(r.svd$d^2)
  cond <-  r.svd$d[1]/r.svd$d[t_train]
  
  out_trt_cat <- matrix(out_trt, nrow = t_train*k_total, ncol = 1)
  out_control_cat <- matrix(out_control, nrow = n-1,ncol=t_train*k_total)
  w_cat <- synth_qp(out_trt_cat, out_control_cat);
  r.svd <- svd(rbind(out_control_cat,t(out_trt_cat)))
  largest_svd <- cbind(largest_svd, r.svd$d[1]^2/sum(r.svd$d^2))
  cond <- cbind(cond,  r.svd$d[1]/r.svd$d[length(r.svd$d)])
  
  out_trt_avg <- rowMeans(out_trt)
  out_control_avg <- matrix(rowMeans(out_control), nrow = n-1,ncol=t_train)
  w_avg <- synth_qp(out_trt_avg, out_control_avg);
  r.svd <- svd(rbind(out_control_avg,t(out_trt_avg)))
  largest_svd <- cbind(largest_svd, r.svd$d[1]^2/sum(r.svd$d^2))
  cond <- cbind(cond,  r.svd$d[1]/r.svd$d[t_total])
  
  ### ### ### ### adding my method here ### ### ### ###
  
  treated_data <- out_trt
  control_data <- out_control
  
  result <- morph$morph(treated_data, control_data)
  
  train_target_vectors <- result[[1]]
  train_covariate_matrices <- result[[2]]
  test_target_vector <- result[[3]]
  test_covariate_matrix <- result[[4]]
  
  # using the orthogonal, fixed weights version of the estimator
  result <- lqorth$learnQorthogonal(train_target_vectors, train_covariate_matrices, 10L, 1000L, 0.0, 0.0, FALSE, NULL, "eye", TRUE)
  Q_matrix <- result[[1]]
  w_learnQ <- result[[2]]
  
  # we use the Q_weights just as w_sep, w_cat, w_avg
  Q_weights <- Q_matrix %*% w_learnQ
  
  ## calculate bias
  model_t1 <- model[((n * t_total)+1):(n * (t_total+1)),]
  
  # just calculate the bias in the first outcome.
  oracle_bias_sep <- SCMbias(model_t1[-trt,1],model_t1[trt,1],w_sep)
  oracle_bias_cat <- SCMbias(model_t1[-trt,1],model_t1[trt,1],w_cat)
  oracle_bias_avg <- SCMbias(model_t1[-trt,1],model_t1[trt,1],w_avg)
  
  # Add bias calculation for Q weights here:
  oracle_bias_Q <- SCMbias(model_t1[-trt,1],model_t1[trt,1],Q_weights) # this may not be the correct dimensions.
  
  return(list("oracle_bias" = c(oracle_bias_sep, oracle_bias_cat, oracle_bias_avg,as.numeric(oracle_bias_Q)), # added bias_Q
              "out" = out,
              "out_trt" = out_trt,
              "out_control" = out_control,
              "model_t1" = model_t1,
              "w_sep" = w_sep,
              "w_cat" = w_cat,
              "w_avg" = w_avg
  ))
}
