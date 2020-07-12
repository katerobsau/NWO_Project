#' Perform a cross validation with NGR
#'
#' @param y The vector of response variables
#' @param X The matrix of covariates
#' @param groups The group devision for the cross validation
#' @param mu_steps The number of variables to select for mu
#' @param sigma_steps The number of variables to select for sigma
#' @return results data frame with observations, mu and sigma. Two dataframes with variables selected for mu and for sigma
#' @export
#'
CV_NGR <- function(y,X,groups){

  un_groups <- unique(groups)
  CV_res <- un_groups %>% lapply(function(g){
    bool_test <- groups == g
    X_test <- X[bool_test,]
    X_train <- X[!bool_test,]
    y_train <- y[!bool_test]

    fit <- fit_NGR(y_train,X_train)
    pred <- predict_NGR(fit,X_test)

    return(list(mu=pred$mu,sigma=pred$sigma,
                vars_mu = names(fit$mu.coefficients)[-1],
                vars_sigma = names(fit$sigma.coefficients)[-1]))
  })

  mu <- CV_res %>% lapply(function(x) x$mu) %>% unlist()
  sigma <- CV_res %>% lapply(function(x) x$sigma) %>% unlist()

  vars_mu = CV_res %>% lapply(function(x) x$vars_mu)# %>% do.call('cbind',.) %>% as.data.frame() %>% setNames(un_groups)
  max_length <- max(sapply(vars_mu,length))
  vars_mu <- vars_mu %>% sapply(function(x){
    res <- character(max_length)
    res[1:length(x)] <- x
    return(res)
  }) %>% as.data.frame() %>% setNames(un_groups)

  vars_sigma = CV_res %>% lapply(function(x) x$vars_sigma)
  max_length <- max(sapply(vars_sigma,length))
  vars_sigma <- vars_sigma %>% sapply(function(x){
    res <- character(max_length)
    res[1:length(x)] <- x
    return(res)
  }) %>% as.data.frame() %>% setNames(un_groups)

  return(list(results = data.frame(y,mu,sigma),
              vars_mu=vars_mu,vars_sigma=vars_sigma))
}

#' Estimate a NGR model
#'
#' @param y The vector of response variables
#' @param X The matrix of covariates
#' @param mu_steps The number of variables to select for mu
#' @param sigma_steps The number of variables to select for sigma
#' @return The estimated NGR model
#' @export
#'
fit_NGR <- function(y,X){

  train_data <- cbind(y,X)

  assign("data_temp", train_data, envir = .GlobalEnv)

  fit_0 <- gamlss::gamlss(formula = y ~ 1,
                          sigma.formula = ~ 1,
                          data = data_temp,
                          family=gamlss.dist::NO, silent=T)

  scope <- list(upper = as.formula(paste("~", paste(names(train_data)[-c(1)],
                                                    collapse="+"),sep="")))
  fit_mu <- gamlss::stepGAIC(fit_0, scope = scope,
                             what = "mu", direction = "both")

  fit_sigma <- gamlss::stepGAIC(fit_mu, scope = scope,
                                what = "sigma", direction ="both")
  fit = fit_sigma

  remove(data_temp,envir=.GlobalEnv)

  return(fit)
}

#' Predict a NGR model
#'
#' @param fit The fitted NGR model
#' @param X_test The covariates for which to make a prediction
#' @return A list with a vector for mu estimates and a vector of sigma esimates
#' @export
predict_NGR <- function(fit, X_test){

  mu <- as.matrix(cbind(1,X_test[names(fit$mu.coefficients)[-1]])) %*% fit$mu.coefficients
  sigma <- as.matrix(cbind(1,X_test[names(fit$sigma.coefficients)[-1]])) %*% fit$sigma.coefficients %>% exp()

  return(list(mu=mu,sigma=sigma))
}
