library(mediation)

set.seed(122424)

n  <- 1000

x <- rbinom(n, 1, .5)

m <- rbinom(n, 1, .2 + .3*x)

y <- rnorm(n, mean = 1 + 1*x + 1*m, sd = 1)

m_model  <- glm(m ~ x, family = 'binomial')

y_model <- lm(y ~ m + x)


mediation_outcome <- mediate(m_model, y_model, treat = "x", mediator = "m")


TE <- 5

p_TE <- 0.3

p_mediator_IDE <- 0.4


source("r/calculate_coefficients_linear.R")

my_coef <- calculate_coefficients_linear(TE, p_TE, p_mediator_IDE)

x <- rbinom(n, 1, .5)

m <- my_coef[3] * x + rnorm(n)

y  <- my_coef[1] * x + my_coef[2] * m + rnorm(n)


n <- 2000

mediation_model <- mediate(lm(m ~ x), lm(y ~ m + x), 
			   treat = "x", mediator  = "m")

summary(mediation_model)
