library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)

source("r/process_mediation_analysis.R")
source("r/find_variances.R")
source("r/find_mediator_coefs.R")
source("r/simulate_mediator_binary.R")
source("r/simulate_normal_outcome.R")
source("r/simulate_clustertrial.R")
source("r/calculate_coefficients_linear.R")
# Set target-specific options such as packages:
 tar_option_set(packages = c("dplyr", "mediation", "lme4")) # nolint

# EBP/PTSD values
n_per_arm_ebp <- 320

n_clusters_per_arm_ebp <- 20

cluster_size_ebp <- 320/20

icc_ebp <- 0.3

outcome_sd_ebp <- 12

cluster_sd_ebp <- find_cluster_variance(icc_ebp, outcome_sd_ebp)

total_effect_ebp <- 7.5

n_sims_ebp <- 1000

n_batches_ebp <- 2

n_sims_batch_ebp <- n_sims_ebp/n_batches_ebp

mediator_intercept_ebp <- 0

# HARPP values
n_per_arm_harp <- 13
n_clusters_harp <- 14
n_clusters_tx_harp <- 7

icc_harp <- 0.007

outcome_sd_harp <- 12

cluster_sd_harp <- find_cluster_variance(icc_harp, outcome_sd_harp^2)

total_effect_harp <- 7.4

baseline_risk_harp <- .05

risk_difference_harp <- 0.25


n_sims_total <- 1000

n_batches <- 10

n_sims_batch <- n_sims_total/n_batches
# End this file with a list of target objects.
list(
## HARP sims
     tar_target(p_te_harp, c(.6, .7, .8)), 
     tar_target(coefs_harp, find_mediator_coefs(total_effect_harp, p_te_harp, baseline_risk_harp, risk_difference_harp), pattern = map(p_te_harp), iteration = "list"),
     tar_target(sim_batch, 1:n_batches), 
     tar_target(harp_sims, simulate_clustertrials(n_sims = n_sims_batch, n_clusters_harp, n_per_arm_harp, n_clusters_tx_harp, cluster_sd_harp, mediator_function = simulate_mediator_binary, mediator_args = list(alpha_0 = coefs_harp["alpha_0"], alpha_1 = coefs_harp["alpha_1"]), link_function = simulate_normal_outcome, outcome_transformation = I, p_missing = 0.2, coefficient_vector = c(1, coefs_harp["tx_coef"], coefs_harp["mediation_coef"]), residual_sd = outcome_sd_harp) %>% mutate(p_te = p_te_harp, sim_batch = sim_batch), pattern = cross(map(p_te_harp, coefs_harp), sim_batch))
          ## EBP/PTSD sims
     tar_target(p_te_ebp, c(.5), iteration = "list"), 
     tar_target(coefs_ebp, calculate_coefficients_linear(total_effect_ebp, p_te_ebp, 1), pattern = map(p_te_ebp), iteration = "list"), 
     tar_target(sim_batch_ebp, 1:n_batches_ebp), 
     tar_target(ebp_ptsd_sims, simulate_clustertrials(n_sims = n_sims_batch_ebp, 
						      n_clusters_per_arm_ebp * 2, 
						      cluster_size_ebp, 
						      n_clusters_per_arm_ebp, 
						      cluster_sd_ebp, 
						      mediator_function = simulate_mediator_normal, 
						      mediator_args = list(coefficient_vector = c(mediator_intercept_ebp, coefs_ebp["exposure_mediator_coef"]), mediator_sd = mediator_sd_ebp), 
						      link_function = simulate_normal_outcome, 
						      outcome_transformation = I, 
						      coefficient_vector = c(1, coefs_ebp["direct_effect"], coefs_ebp["mediator_outcome_coef"]), 
						      residual_sd = outcome_sd_ebp ) %>% 
       mutate(p_te = p_te_ebp, sim_batch = sim_batch_ebp), pattern = cross(map(p_te_ebp, coefs_ebp), sim_batch_ebp))



)
