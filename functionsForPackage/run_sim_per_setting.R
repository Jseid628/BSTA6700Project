
# potentially don't need this

source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/synth_qp.R")
source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/SCMbias.R")
source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/rmse.R")


run_sim_per_setting <- function(rho, setting) {
  # factor loadings
  n <- 50 # number of units, the first unit is treated
  # N x 1 vector
  mu <- list(c = seq(5, 1, length.out = n))
  mu$c[c(1, 2)] <- mu$c[c(2, 1)]; # hand-code factor loadings for n = 50 (common part)
  trt <- numeric(n); trt[1] <- 1; # select the unit with the second largest loadings to be the treated unit
  t_total <- setting$t_total
  k_total <- setting$k_total

  model <- generateModel(t_total, k_total, trt, mu, rho, n)

  # simulation
  n_sim <- 20
  columns_bias <- c("sep","cat","avg","Q")
  bias_sim <- data.frame(matrix(nrow = 0, ncol = length(columns_bias)))
  colnames(bias_sim) <- columns_bias

  all_results <- lapply(1:n_sim, function(s) {
    use_virtualenv("/Users/jseid1/venv311", required = TRUE)
    iter_start = proc.time()
    result <-fit_models(model, n, trt, k_total, t_total, variance = 1, num_timepoints = 40)
    result$iter_time <- (proc.time() - iter_start)["elapsed"]
    return(result)
  })

  bias_sim <- data.frame(matrix(nrow = 0, ncol = length(columns_bias)))
  colnames(bias_sim) <- columns_bias
  iter_lengths <- data.frame(setting = character(), sim = integer(), iter_length = numeric())

  # reassemble
  for (s in 1:n_sim) {
    result <- all_results[[s]]
    iter_lengths <- rbind(iter_lengths, data.frame(
      setting = setting$file_suffix,
      sim = s,
      iter_length = result$iter_time
    ))
    bias_sim[s,] <- result$oracle_bias
  }

  # bias info
  bias_sim_long <- bias_sim %>%
    pivot_longer(columns_bias, names_to = "method", values_to = "bias") %>%
    mutate(setting = setting$file_suffix)

  return(list(bias_sim_long = bias_sim_long, iter_lengths = iter_lengths))
}


