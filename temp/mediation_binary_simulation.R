library(mediation)
library(lme4)

set.seed(4434)

n_clusters <- 14
n_per_cluster <- 13

n_treated_clusters <- 7

residual_sd <- 12

icc <- .007

cluster_sd  <- sqrt(icc/(1-icc)*residual_sd^2)

random_effects  <- rnorm(n_clusters, 0, cluster_sd)

random_effects_vec <- rep(random_effects, each = n_per_cluster)

dat <- data.frame(cluster_id = rep(1:n_clusters, each = n_per_cluster), tx = c(rep(0, 7*13), rep(1, 7*13)), random_intercept = random_effects_vec)

dat$missing <- as.vector(sapply(1:n_clusters,function(x) rbinom(n_per_cluster, 1, 0.2)))

TE <- 7.4

p_TE <- .8

tx_coef <- TE*p_TE

risk_difference <- 0.25

mediation_coef <- (1 - p_TE)*TE/risk_difference

p_baseline <- 0.05

alpha_0 <- log(p_baseline/(1 - p_baseline))

alpha_1  <- log((risk_difference + p_baseline)/(1 - (risk_difference + p_baseline))) - log(p_baseline/(1 - p_baseline))

dat$mediator <- rbinom(n_clusters*n_per_cluster, 1, 1/(1 + exp(-(alpha_0 + alpha_1*dat$tx))))

dat$outcome  <- tx_coef*dat$tx + mediation_coef*dat$mediator + dat$random_intercept + rnorm(n_clusters*n_per_cluster, 0, residual_sd)

mediator_model <- glmer(mediator ~ tx + (1 | cluster_id), family = "binomial", nAGQ = 3,  data = dat[dat$missing == 0, ])

outcome_model <- lmer(outcome ~ mediator + tx + (1 | cluster_id), data = dat[dat$missing == 0, ])

mediation_analysis <- mediate(mediator_model, outcome_model, mediator = "mediator", treat = "tx")




summary(mediation_analysis)





