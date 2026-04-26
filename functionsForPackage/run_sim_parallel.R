run_sim_parallel <- function(rho, n_sim) {
  settings_list <- list(
    list(t_total=10, k_total=4, file_suffix="T_0=10,K=4"),
    list(t_total=10, k_total=10, file_suffix="T_0=10,K=10"),
    list(t_total=40, k_total=4, file_suffix="T_0=40,K=4"),
    list(t_total=40, k_total=10, file_suffix="T_0=40,K=10")
  ) 
  
  all_bias_sim_long <- data.frame()
  iteration_lengths <- data.frame(
    setting = character(),
    sim = integer(),
    iter_length = numeric()
  )
  
  # Claude was very helpful with writing the parallelization piece here:
  # mclapply did not work with Reticulate and Claude suggested the future package.
  plan(multisession, workers = 14)
  results <- furrr::future_map(settings_list, function(setting) {
    library(reticulate)
    library(osqp)
    library(dplyr)
    library(tidyverse)
    
    use_virtualenv("/Users/jseid1/venv311", required = TRUE)

    source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/generateModel.R")
    source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/fit_models.R")
    source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/run_sim_per_setting.R")
    
    ## This is the function ##
    n <- 50 # number of units
    # N x 1 vector
    mu <- list(c = seq(5, 1, length.out = n))
    mu$c[c(1, 2)] <- mu$c[c(2, 1)]; # hand-code factor loadings for n = 50 (common part)
    trt <- numeric(n); trt[1] <- 1; # select the unit with the second largest loadings to be the treated unit
    t_total <- setting$t_total
    k_total <- setting$k_total   
    
    model <- generateModel(t_total, k_total, trt, mu, rho, n)
    
    columns_bias <- c("sep","cat","avg","Q")
    bias_sim <- data.frame(matrix(nrow = 0, ncol = length(columns_bias))) 
    colnames(bias_sim) <- columns_bias
    
    all_results <- lapply(1:n_sim, function(s) {
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
    ## end function ##
  }, .options = furrr_options(globals = c("rho","n_sim"), seed = TRUE))
  
  plan(sequential)
  
  # reassemble across settings
  all_bias_sim_long <- data.frame()
  iteration_lengths <- data.frame(setting = character(), sim = integer(), iter_length = numeric())
  
  for (i in 1:length(settings_list)) {
    all_bias_sim_long <- rbind(all_bias_sim_long, results[[i]]$bias_sim_long)
    iteration_lengths <- rbind(iteration_lengths, results[[i]]$iter_lengths)
  }
  
  # summary statistics for iteration lengths by setting.
  iteration_summary <- iteration_lengths %>%
    group_by(setting) %>%
    summarize(
      mean_time = mean(iter_length),
      median_time = median(iter_length),
      sd_time = sd(iter_length),
      min_time = min(iter_length),
      max_time = max(iter_length)
    )
  
  saveRDS(all_bias_sim_long, file = "results/sim_biasOrtho_4-26.rds")
  saveRDS(iteration_summary, file = "results/sim_timeOrtho_4-26.rds")
  
}
