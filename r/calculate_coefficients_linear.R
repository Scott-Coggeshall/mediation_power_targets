# function for finding coefficient values
# given a specified total effect and a specified
# proportion of the total effect mediated through
# the mediator, 
# and a specified ratio for splitting up the
# mediator effect between the effect of the 
# exposure on the mediator and the effect of the
# mediator on the outcome

calculate_coefficients_linear <- function(total_effect, 
					  p_total_effect, 
					  mediator_coefficient_ratio){


	direct_effect <- (1 - p_total_effect) * total_effect


	indirect_effect <- p_total_effect * total_effect


	exposure_mediator_coef  <- sqrt(indirect_effect/mediator_coefficient_ratio)

	mediator_outcome_coef <- mediator_coefficient_ratio * exposure_mediator_coef



	return(c(direct_effect, exposure_mediator_coef, 
		 mediator_outcome_coef))


}
