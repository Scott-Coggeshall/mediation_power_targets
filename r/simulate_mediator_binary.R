expit <- function(x){

	1/(1 + exp(-x))

}


simulate_mediator_binary <- function(dat, alpha_0, alpha_1){


	rbinom(nrow(dat), 1, expit(alpha_0 + alpha_1*dat$tx))


}
