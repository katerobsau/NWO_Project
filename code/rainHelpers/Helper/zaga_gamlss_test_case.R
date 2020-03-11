# rule
# mu = 1 +  2 * mean0
# nu = emp/num_members
# sigma = constant

library(tidyverse)
library(gamlss)
library(patchwork)
library(SpecsVerification)

num_days = 365*2
num_members = 50
nu_emp_truth = runif(num_days)
mean_covar = 1/4*(num_members - num_members*nu_emp_truth)

plot(nu_emp_truth, mean_covar)

# -----------------------------------------------------------------------------

zaga_members = 1:num_days %>% sapply(function(i){
  member_vals = rZAGA(n = num_members,
                      mu = 1 + mean_covar[i],
                      sigma = 1,
                      nu = nu_emp_truth[i])
  return(member_vals)
}) %>% t()

y_obs = 1:num_days %>% sapply(function(i){
  member_vals = rZAGA(n = 1,
                      mu = 1 + mean_covar[i],
                      sigma = 1,
                      nu = nu_emp_truth[i])
  return(member_vals)
})

plot(rowMeans(zaga_members), mean_covar)
plot(rowSums(zaga_members == 0), nu_emp_truth)

# -----------------------------------------------------------------------------

fit_data = data.frame(RH6 = y_obs,
                      mean0 = rowMeans(zaga_members),
                      sd0 = apply(zaga_members, 1, sd),
                      nu_emp = apply(zaga_members, 1,
                                     function(r){sum(r == 0)/num_members})
                      )
groups = rep(c(1,2),each = 365)

# -----------------------------------------------------------------------------

warning("Had should not hard code RH6")

# Fit the model using the true formulas
fit_truth <- gamlss::gamlss(formula = RH6 ~ nu_emp,
                        sigma.formula = ~ 1,
                        nu.formula = ~ nu_emp,
                        data = fit_data,
                        family = gamlss.dist::ZAGA,
                        silent=T)

# Compare the predicted parameters to the truth
predict_truth <- predict_ZAGA_parameters(test_data = fit_data,
                                         fit = fit_truth) %>%
  bind_cols() %>%
  mutate(true_mu = 1 + mean_covar,
         true_sigma = rep(1, num_days),
         true_nu = nu_emp_truth)

# Plot the truth vs predictions
mu_plot <- ggplot(data = predict_truth) +
  geom_point(aes(x = mu, y = true_mu)) +
  geom_abline(col = "red", linetype = "dotted")

sigma_plot <- ggplot(data = predict_truth) +
  geom_point(aes(x = sigma, y = true_sigma)) +
  geom_point(aes(x = 1, y = 1), col = "red", shape = 3) +
  xlim(c(0.9, 1.1))

nu_plot <- ggplot(data = predict_truth) +
  geom_point(aes(x = nu, y = true_nu)) +
  geom_abline(col = "red", linetype = "dotted")

mu_plot + sigma_plot + nu_plot

# Observe non-linearity in mu, existed from the beginning
# but not sure if this is the best representation of this
# parameter
# -----------------------------------------------------------------------------

# Now see which model is selected using the fit_ZAGA function
names(fit_data)
fit_test <- fit_ZAGA(fit_data)

fit_test$mu.coefficients
fit_truth$mu.coefficients

fit_test$sigma.coefficients
fit_truth$sigma.coefficients

fit_test$nu.coefficients
fit_truth$nu.coefficients

# covariates selected are the same as the true model
# if I add in more data this isn't the case

# -----------------------------------------------------------------------------

cv_list <- CV_ZAGA(fit_data, groups)
cv_list$vars_mu
cv_list$vars_sigma
cv_list$vars_nu

# (two years)
# Didn't select the right variables for sigma in group 1
# Overfitted

# CV doens't have the same overfit problem (? not sure why ?)
# -----------------------------------------------------------------------------

# Scoring Set Up
cv_data = cv_list

# get obs
y_obs <- fit_data %>% pull(RH6)

# get raw ensemble
raw_ensemble = zaga_members

# simulate an forecast ensemble
pars = cv_data$results
sim_ensemble <- mapply(rZAGA, mu = pars$mu,
                           sigma = pars$sigma,
                           nu = pars$nu,
                           USE.NAMES = TRUE,
                           MoreArgs = list(n = num_members)) %>% t()

# -----------------------------------------------------------------------------

# Rank Histograms

pp_ranks <-  apply(cbind(y_obs, sim_ensemble), 1, rank, ties.method = "random")[1,]

raw_ranks <- apply(cbind(y_obs, raw_ensemble), 1, rank, ties.method = "random")[1,]

q = qbinom(c(0.01, 0.99), num_days, 1/(num_members+1))

pp_rankhist <- ggplot() +
  geom_histogram(aes(pp_ranks), breaks = seq(0.5, num_members + 1.5)) +
  geom_hline(yintercept = q, col = "red", linetype = "dashed")
raw_rankhist <- ggplot() +
  geom_histogram(aes(raw_ranks), breaks = seq(0.5, num_members + 1.5)) +
  geom_hline(yintercept = q, col = "red", linetype = "dashed")
raw_rankhist + pp_rankhist

# Rankhist(sim_ensemble, y_obs) %>% PlotRankhist()
# Rankhist(raw_ensemble, y_obs) %>% PlotRankhist()

# -----------------------------------------------------------------------------

# Pit histograms
raw_pit <- sapply(1:nrow(raw_ensemble),
                  function(j, raw_ensemble, y_obs){
                    ecdf(raw_ensemble[j,] %>% as.numeric())(y_obs[j])},
                  raw_ensemble, y_obs)

emos_pit <- sapply(1:nrow(sim_ensemble),
                   function(j,sim_ensemble, y_obs){
                     ecdf(sim_ensemble[j,] %>% as.numeric())(y_obs[j])},
                   sim_ensemble, y_obs)

stop('Something is currently worng with these pit histograms')

emos_pithist <- ggplot() +
  geom_histogram(aes(emos_pit), breaks = seq(0.5, num_members + 1.5)/51) +
  geom_hline(yintercept = q, col = "red", linetype = "dashed")
raw_pithist <- ggplot() +
  geom_histogram(aes(raw_pit), breaks = seq(0.5, num_members + 1.5)/51) +
  geom_hline(yintercept = q, col = "red", linetype = "dashed")
raw_pithist + pp_pithist

# -----------------------------------------------------------------------------
# CRPS

# get crps for emos
crps_emos <- crps_ensemble(ens = sim_ensemble, obs = y_obs)

# get crps for emos
crps_raw <- crps_ensemble(ens = raw_ensemble, obs = y_obs)

# combine
crps_data <- data.frame(crps_raw, crps_emos, group = groups)

# Get the best fits
group_summaries <- crps_data %>%
  group_by(group) %>%
  summarise(mean_emos = mean(crps_emos),
            median_emos = median(crps_emos),
            mean_raw = mean(crps_raw),
            median_raw = median(crps_raw)) %>%
  ungroup()
group_summaries

best_group <- group_summaries %>%
  slice(which.min(median_emos)) %>%
  pull(group)

