process_mediation_analysis <- function(mediation_analysis){


	indirect_effect <- mediation_analysis$d1
	indirect_effect_pval <- mediation_analysis$d1.p

	tau_effect <- mediation_analysis$tau.coef

	tau_pval <- mediation_analysis$tau.p

	output <- data.frame(indirect_effect = indirect_effect, indirect_effect_pval, tau_effect = tau_effect, 
	tau_pval = tau_pval)




}
