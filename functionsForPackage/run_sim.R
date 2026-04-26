source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/synth_qp.R")
source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/SCMbias.R")
source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/rmse.R")

run_sim <- function(rho,n_sim) {
  # factor loadings
  n <- 50 # number of units, the first unit is treated
  
  mu <- list(
    c = seq(5, 1, length.out = n) # N x 1 vector
  )
  mu$c[c(1, 2)] <- mu$c[c(2, 1)]; # hand-code factor loadings for n = 50 (common part)
  
  trt <- numeric(n); trt[1] <- 1; # select the unit with the second largest loadings to be the treated unit
  
  # incorporate all settings into a loop
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
  
  
  for (setting in settings_list) {
    t_total <- setting$t_total
    k_total <- setting$k_total   
    
    model <- generateModel(t_total, k_total, trt, mu, rho, n)
    
    # simulation
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
    
    # reassemble 
    for (s in 1:n_sim) {
      result <- all_results[[s]]
      iteration_lengths <- rbind(iteration_lengths, data.frame(
        setting = setting$file_suffix,
        sim = s,
        iter_length = result$iter_time
      ))
      bias_sim[s,] <- result$oracle_bias 
    }
    
    # bias info
    bias_sim_long <- bias_sim %>% 
      pivot_longer(columns_bias, names_to = "method", values_to = "bias") %>% mutate(setting = setting$file_suffix)
    all_bias_sim_long <- rbind(all_bias_sim_long, bias_sim_long)
    
    # summarize the simulation time info
    iteration_summary <- iteration_lengths %>%
      # there are four different settings
      group_by(setting) %>%
      summarize(
        mean_time = mean(iter_length),
        median_time = median(iter_length),
        sd_time = sd(iter_length),
        min_time = min(iter_length),
        max_time = max(iter_length)
      )
    
    # save the data
    saveRDS(all_bias_sim_long, file = "results/sim_biasOrtho_4-26.rds")
    saveRDS(iteration_summary, file = "results/sim_timeOrtho_4-26.rds")
  }
}


  