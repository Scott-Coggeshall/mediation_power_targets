source("r/process_mediation_analysis.R")
source("r/find_variances.R")
source("r/find_mediator_coefs.R")
source("r/simulate_mediator_binary.R")
source("r/simulate_normal_outcome.R")
source("r/simulate_clustertrial.R")
#

targets::tar_load(coefs_harp)

n_per_cluster <- 13
n_clusters <- 14
n_clusters_tx <- 7

icc <- 0.007

outcome_sd <- 12

random_intercept_sd <- find_cluster_variance(icc, outcome_sd^2)

total_effect <- 7.4

baseline_risk <- .05

risk_difference <- 0.25

mediator_function <- simulate_mediator_binary

mediator_args <- list(alpha_0 = coefs_harp[[1]]["alpha_0"], alpha_1 = coefs_harp[[1]]["alpha_1"])



	if(length(n_per_cluster) == 1){
		n_per_cluster  <- rep(n_per_cluster, n_clusters)
	}

	if(length(n_per_cluster) != n_clusters) stop("If you are providing a vector of cluster sizes, then length(n_per_cluster) must be equal to n_clusters")

	total_n  <- sum(n_per_cluster)

	# creating vectors of cluster ids and treatment assignment indicators
	cluster_ids <- factor(rep(1:n_clusters, n_per_cluster))

	# if n_clusters_tx isn't an integer (e.g. if the value for n_clusters_tx was obtained by dividing an odd interger by 2)
	# then we take the ceiling
	n_clusters_tx  <- ceiling(n_clusters_tx)

	# vector of treatment assignments
	tx <- c(rep(0, sum(n_per_cluster[(n_clusters_tx + 1):n_clusters])), rep(1, sum(n_per_cluster[1:n_clusters_tx])))

	# simulate cluster-level random intercepts
	# to induce correlation among observations in the same cluster.
	random_intercepts  <- rnorm(n_clusters, mean = 0, sd = random_intercept_sd)

	# replicate so that observations in each cluster have same random intercept
	random_intercepts_vec  <- rep(random_intercepts, n_per_cluster)
	

	# start our simulated dataset with columns for cluster id, (unobserved) random intercepts,  and treatment assignment
	dat  <- data.frame(cluster_id = cluster_ids, random_intercept = random_intercepts_vec, tx = tx)

	dat$mediator <- do.call(mediator_function, c(list(dat = dat),  mediator_args))

	dat$outcome  <- outcome_transformation(link_function(dat, ...))

	dat$n_clusters <- n_clusters
	dat$n_per_cluster <- n_per_cluster
	dat$n_clusters_tx  <- n_clusters_tx


	# analyze data
	outcome_model <- lmer(outcome ~ tx + mediator + (1 | cluster_id), data = dat)

	mediator_model <- glm(mediator ~ tx, family = 'binomial', data = dat)

	mediation_analysis <- mediate(mediator_model, outcome_model, treat = "tx", mediator = "mediator")

	output_dat <- process_mediation_analysis(mediation_analysis)


