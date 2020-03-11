fit_ZAGA <- function(train_data){

  assign("data_temp", train_data, envir = .GlobalEnv)

  t1 <- Sys.time()

  # Fit the ZAGA
  fit_0 <- gamlss::gamlss(formula = RH6 ~ 1,
                          sigma.formula = ~ 1,
                          nu.formula = ~ 1,
                          data = data_temp,
                          family = gamlss.dist::ZAGA,
                          silent=T)

  par_formula <- paste("~", paste(names(data_temp)[-c(1)], collapse="+"),sep="")
  scope <- list(upper = as.formula(par_formula))

  fit_mu <- gamlss::stepGAIC(fit_0, scope = scope,
                             what = "mu", direction = "both")

  fit_sigma <- gamlss::stepGAIC(fit_mu, scope = scope,
                                what = "sigma", direction ="both")

  fit_nu <- gamlss::stepGAIC(fit_sigma, scope = scope,
                             what = "nu", direction ="both")

  # ideally should automate the stepwise mu, nu, sigma for flexibility
  # order of stepwise operations and GAIC needs checking

  t2 <- Sys.time()
  print(paste("GAMLSS Fit Time:", round(t2 - t1, 2)))
  remove(data_temp, envir=.GlobalEnv)

  fit = fit_nu
  return(fit)

}

predict_ZAGA_parameters <- function(test_data, fit){

  # ideally should automate the link function transformation

  mu <- as.matrix(cbind(1,test_data[names(fit$mu.coefficients)[-1]])) %*%
    fit$mu.coefficients %>%
    exp()

  sigma <- as.matrix(cbind(1,test_data[names(fit$sigma.coefficients)[-1]])) %*%
    fit$sigma.coefficients %>%
    exp()

  nu <- as.matrix(cbind(1,test_data[names(fit$nu.coefficients)[-1]])) %*%
    fit$nu.coefficients %>%
    (function(.){exp(.)/(1 + exp(.))})

  return(list(mu=mu, sigma=sigma, nu=nu))

}

CV_ZAGA <- function(fit_data, groups){
  warning("Hard coded RH6 into fit_ZAGA function")
  warning("Hard coded link functions into predict")

  un_groups <- unique(groups)
  CV_res <- un_groups %>% lapply(function(g){
    print(g)
    bool_test <- groups == g
    test_data <- fit_data[bool_test,]
    train_data <- fit_data[!bool_test,]

    fit <- fit_ZAGA(train_data)
    pred <- predict_ZAGA_parameters(test_data, fit)

    return(list(mu=pred$mu, sigma=pred$sigma, nu = pred$nu,
                vars_mu = names(fit$mu.coefficients)[-1],
                vars_sigma = names(fit$sigma.coefficients)[-1],
                vars_nu = names(fit$nu.coefficients)[-1]))
  })

  mu <- CV_res %>% lapply(function(x) x$mu) %>% unlist()
  sigma <- CV_res %>% lapply(function(x) x$sigma) %>% unlist()
  nu <- CV_res %>% lapply(function(x) x$nu) %>% unlist()
#
#   vars_mu = CV_res %>% lapply(function(x) x$vars_mu)
#   max_length <- max(sapply(vars_mu,length))
#   vars_mu <- vars_mu %>% sapply(function(x){
#     if(length(x) == 0)  return(res)
#     res <- character(max_length)
#     res[1:length(x)] <- x
#     return(res)
#   }) %>% matrix(ncol = length(un_groups)) %>%
#     as.data.frame() %>%
#     setNames(un_groups)
#
#   vars_sigma = CV_res %>% lapply(function(x) x$vars_sigma)
#   max_length <- max(sapply(vars_sigma,length))
#   warning("Need an NA handler here")
#   vars_sigma <- vars_sigma %>% sapply(function(x){
#     if(length(x) == 0)  return(res)
#     res <- character(max_length)
#     res[1:length(x)] <- x
#     return(res)
#   }) %>% matrix(ncol = length(un_groups)) %>%
#     as.data.frame() %>%
#     setNames(un_groups)
#
#   vars_nu = CV_res %>% lapply(function(x) x$vars_nu)
#   max_length <- max(sapply(vars_nu,length))
#   vars_nu <- vars_nu %>% sapply(function(x){
#     res <- character(max_length)
#     if(length(x) == 0)  return(res)
#     res[1:length(x)] <- x
#     return(res)
#   }) %>% matrix(ncol = length(un_groups)) %>%
#     as.data.frame() %>%
#     setNames(un_groups)

  warning("Did some odd handling of drop and no covariate selection")

  return(list(results = data.frame(mu, sigma, nu),
              # vars_mu=vars_mu, vars_sigma=vars_sigma, vars_nu=vars_nu))
              vars_mu=NA, vars_sigma=NA, vars_nu=NA))

}
