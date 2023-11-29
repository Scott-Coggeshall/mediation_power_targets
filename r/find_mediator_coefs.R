find_mediator_coefs <- function(total_effect, p_te, baseline_risk, risk_difference){


	tx_coef <- total_effect*p_te

	mediation_coef <- (1 - p_te) * total_effect/risk_difference


	alpha_0 <- log(baseline_risk/(1 - baseline_risk))

	alpha_1  <- log((risk_difference + baseline_risk)/(1 - (risk_difference + baseline_risk))) - log(baseline_risk/(1 - baseline_risk))


	return(c(tx_coef = tx_coef,
		 mediation_coef = mediation_coef, 
		 alpha_0 = alpha_0, 
		 alpha_1 = alpha_1))

}
