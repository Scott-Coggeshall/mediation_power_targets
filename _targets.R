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
# Set target-specific options such as packages:
 tar_option_set(packages = c("dplyr", "mediation", "lme4")) # nolint

# EBP/PTSD values
n_per_arm <- 320

n_clusters_per_arm <- 20

cluster_size <- 320/20

icc <- 0.3

outcome_sd <- 12

cluster_sd <- find_cluster_variance(icc, outcome_sd)

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
)
