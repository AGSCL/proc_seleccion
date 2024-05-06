if (!require("BayesSurvival")) {
  install.packages("BayesSurvival", repos = "http://cran.us.r-project.org")
  library(BayesSurvival)
}

# Generar datos simulados
set.seed(123)
n <- 100  # número de pacientes
times <- rexp(n, rate = 0.1)  # tiempos de supervivencia
status <- sample(0:1, n, replace = TRUE)  # estado: 1 si el evento ocurrió, 0 censurado

# Modelo de supervivencia bayesiano con prior no informativo
fit <- bsreg(Surv(times, status) ~ 1,  # Modelo con solo intercepto
             data = data.frame(times, status),
             dist = "exponential",  # Suponemos una distribución exponencial
             prior = list(beta = c(0, 10000)))  # Prior no informativo para el intercepto









# JAGS model code as a string vector
model_code <- c(
  "model {
  for (i in 1:N) { 
    # Likelihood for time to event
    diff_bet_proc[i] ~ dweib(r, mu[i]) 
    log(mu[i]) <- beta0 + beta1 * sex[i] + beta2 * fech_ter_num[i]

    # Censoring indicator
    event_n[i] ~ dbern(p[i])
    logit_p[i] <- log(mu[i]) + log(r)) * diff_bet_proc[i]^r
    p[i] <- inv_logit(logit_p[i]) # Where inv_logit is the logistic function, bounding the pr
  }

  # Priors
  beta0 ~ dnorm(0, 0.01) # Non-informative prior for intercept
  beta1 ~ dnorm(0, 0.01) # Non-informative prior for sex effect
  beta2 ~ dnorm(0, 0.01) # Non-informative prior for fech_ter_num effect
  r ~ dgamma(0.1, 0.1)   # Non-informative prior for Weibull shape parameter
}"
)

# Write the model code to a text file named "model.txt"
writeLines(model_code, "model.txt")

library(rjags)


# Load data
data_list <- list(
  N = nrow(part_17_first_ev_naomit),
  diff_bet_proc = part_17_first_ev_naomit$diff_bet_proc,
  event_n = part_17_first_ev_naomit$event_n2,
  sex = part_17_first_ev_naomit$sex_num,
  fech_ter_num = log(part_17_first_ev_naomit$fech_ter_num)
)

K <- 3 # number of intervals
a <- seq(0, max(part_17_first_ev_naomit$diff_bet_proc) + 0.001, length.out = K + 1)
# int.obs: vector that tells us at which interval each observation is
int.obs <- matrix(data = NA, nrow = nrow(part_17_first_ev_naomit), ncol = length(a) - 1)
d <- matrix(data = NA, nrow = nrow(part_17_first_ev_naomit), ncol = length(a) - 1)
for(i in 1:nrow(part_17_first_ev_naomit)) {
  for(k in 1:(length(a) - 1)) {
    d[i, k] <- ifelse(part_17_first_ev_naomit$diff_bet_proc[i] - a[k] > 0, 1, 0) * ifelse(a[k + 1] - part_17_first_ev_naomit$diff_bet_proc[i] > 0, 1, 0)
    int.obs[i, k] <- d[i, k] * k
  }
}
int.obs <- rowSums(int.obs)

X <- model.matrix(~ fech_ter_num + sex_num, data = part_17_first_ev_naomit)

d.jags <- list(n = nrow(part_17_first_ev_naomit), m = length(a) - 1, delta = part_17_first_ev_naomit$diff_bet_proc,
               time = part_17_first_ev_naomit$diff_bet_proc, X = X[,-1], a = a, int.obs = int.obs, Nbetas = ncol(X) - 1,
               zeros = rep(0, nrow(part_17_first_ev_naomit)))

i.jags <- function() {
  list(beta = rnorm(ncol(X) - 1), lambda = runif(3, 0.1))
}

model_hh<-
  "
model {
  # Calculate pieces of the cumulative hazard function for each observation
  for (i in 1:n) {
    for (k in 1:int.obs[i]) {
      cond[i, k] <- step(time[i] - a[k + 1])
      HH[i, k] <- cond[i, k] * (a[k + 1] - a[k]) * lambda[k] +
                  (1 - cond[i, k]) * (time[i] - a[k]) * lambda[k]
    }
    # Cumulative hazard function
    H[i] <- sum(HH[i, 1:int.obs[i]])
  }

  for (i in 1:n) {
    # Linear predictor using exponential of the inner product
    elinpred[i] <- exp(inprod(beta[], X[i,]))
    # Log-hazard function
    logHaz[i] <- log(lambda[int.obs[i]] * elinpred[i])
    # Log-survival function
    logSurv[i] <- -H[i] * elinpred[i]

    # Definition of the log-likelihood using the zero trick
    phi[i] <- 100000 - delta[i] * logHaz[i] - logSurv[i]
    zeros[i] ~ dpois(phi[i])
  }

  # Prior distributions for regression coefficients
  for (l in 1:Nbetas) {
    beta[l] ~ dnorm(0, 0.001)
  }

  # Prior distributions for piecewise lambda parameters
  for (k in 1:m) {
    lambda[k] ~ dgamma(0.01, 0.01)
  }
}
"
writeLines(model_hh, "modelhr.txt")


p.jags <- c("beta", "lambda")

m2 <- jags.model(data = d.jags, file = "modelhr.txt", inits = i.jags, n.chains = 3)


# Initial values
inits <- function() {
  list(beta_sex = 0, beta_fech_ter_num = 0, alpha = 0, shape = 1)
}

# Parameters to monitor
params <- c("beta_sex", "beta_fech_ter_num", "alpha", "shape")

# Model
model_file <- "model.txt"  # Path to the JAGS model file

model_code2 <- c(
  "model {
# Uncensored and right-censored observations
for (i in 1:n) {
  is.censored[i] ~ dinterval(time[i], cen[i])   # Indicator for censoring
  time[i] ~ dweib(alpha, lambda[i])             # Weibull distribution for time
  lambda[i] <- exp(-mu[i] * alpha)              # Scale parameter
  mu[i] <- inprod(beta[], X[i,])                # Linear predictor
}

# Prior distributions
for (l in 1:Nbetas) {
  beta[l] ~ dnorm(0, 0.001)                     # Normal priors for regression coefficients
}
alpha ~ dunif(0, 10)                            # Uniform prior for shape parameter of Weibull distribution
}")
d.jags <- list(n = nrow(X), time = time, cen = cen, X = Xnew, is.censored = is.censored, Nbetas = ncol(X))
i.jags <- function() list(beta = rnorm(ncol(X)), alpha = runif(1))
p.jags <- c("beta", "alpha")


# Running the model
mod <- jags.model(model_file, data = data_list, inits = inits, n.chains = 6, n.adapt = 500)
#The n.chains argument indicates the number of Markov chains selected. Secondly, a burn-in period is considered (here the first 1000 simulations) using the update function
update(mod, 1000)  # Burn-in
#the model is run using coda.samples function for a specific number of iterations to monitor (here n.iter=50000) as well as a specific thinning value (here thin=10) in order to reduce autocorrelation in the saved samples

jags_model <- jags.model(textConnection(model_string), data = data_list, n.chains = 3, n.adapt = 1000)
update(jags_model, n.iter = 5000) # Number of burn-in iterations
coda_samples <- coda.samples(jags_model, variable.names = c("beta0", "beta1", "beta2"), n.iter = 10000)


# Checking results
print(summary(samples))

traceplot(samples)

densplot(samples, main="Density Plot")
gelman.diag(samples)