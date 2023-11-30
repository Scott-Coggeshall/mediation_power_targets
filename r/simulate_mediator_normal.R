simulate_mediator_normal <- function(dat, coefficient_vector, mediator_sd){


	rnorm(nrow(dat), mean = coefficient_vector[1] + coefficient_vector[2]*dat$tx, sd = mediator_sd)





}
