
# ## Install devtools if not already installed
# install.packages("devtools", repos='http://cran.us.r-project.org')
# library(devtools)
# ## Install augsynth from github
# devtools::install_github("ebenmichael/augsynth", force = T)
# library(augsynth)
## general libraries

message("Loading Libraries...")
suppressWarnings(suppressMessages({
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  library(MASS)
  library(osqp)
  library(parallel)
  library(future)
  library(furrr)
}))

rm(list = ls())

source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/create_figure.R")

# rho says how much common factors matter
rho <- 1
n_sim = 1
parallel = TRUE
set.seed(215)
# only creates if folder does not exist
dir.create("figure", showWarnings = FALSE) 
# store the simulation results.
dir.create("results", showWarnings = FALSE)

message("Running Simulation...(Ignore silly error messages)")
# run the simulation in parallel or serially.
if (parallel) {
  source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/run_sim_parallel.R")
  suppressWarnings(suppressMessages({
    run_sim_parallel(rho,n_sim)
  }))
} else {
  # helper functions 
  source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/generateModel.R")
  source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/fit_models.R")
  source("/Users/jseid1/BSTA6700FinalProject/functionsForPackage/run_sim.R")
  suppressWarnings(suppressMessages({
    results <- run_sim(rho, n_sim)
  }))
}

message("Creating figure...")
create_figure()

message("Done!")

