# Function for simulating a dataset from a cluster
# randomized trial using a data generating mechanism
# corresponding to a random effects model

simulate_clustertrial  <- function(n_clusters, n_per_cluster, n_clusters_tx, 
				   random_intercept_sd, mediator_function, mediator_args,   
				   link_function,  outcome_transformation, p_missing,...){


	# n_per_cluster can be supplied as an integer, in which case cluster sizes are all equal,
	# or a vector of cluster sizes to allow for varying cluster sizes.
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
	
	dat$missing <- as.vector(sapply(1:n_clusters, function(x) rbinom(unique(n_per_cluster), 1, p_missing)))


	# analyze data
	outcome_model <- lmer(outcome ~ tx + mediator + (1 | cluster_id), data = dat[dat$missing == 0, ])

	mediator_model <- glmer(mediator ~ tx + (1 | cluster_id),  family = 'binomial', data = dat[dat$missing == 0, ])

	mediation_analysis <- mediate(mediator_model, outcome_model, treat = "tx", mediator = "mediator")

	output_dat <- process_mediation_analysis(mediation_analysis)

	return(output_dat)

}



simulate_clustertrials <- function(n_sims, n_clusters, n_per_cluster, n_clusters_tx, 
				   random_intercept_sd,  
				   mediator_function, 
				   mediator_args,
				   link_function, outcome_transformation, p_missing, ...){


	datasets_list <- lapply(1:n_sims, function(x){
					simulate_clustertrial(n_clusters, n_per_cluster, n_clusters_tx, random_intercept_sd, mediator_function, mediator_args, link_function, outcome_transformation, p_missing, ...) %>% mutate(sim_id = x) 
				   })

	Reduce("rbind", datasets_list)


}
