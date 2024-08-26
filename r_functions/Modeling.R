# Convert epsg to epsg KM
epsgKM <- function(x) {
  crs <- st_crs(x)
  proj4KM <- gsub(pattern = "+.units=m", replacement = "+units=km", 
                  crs$proj4string)
  return(proj4KM)
}

# Envelope for variogram
variog_envelope <- function (geodata, coords = geodata$coords, data = geodata$data, 
                             obj.variog, nsim = 99, save.sim = FALSE, messages) 
{
  call.fc <- match.call()
  if (missing(geodata)) 
    geodata <- list(coords = coords, data = data)
  if (missing(messages)) 
    messages.screen <- as.logical(ifelse(is.null(getOption("geoR.messages")), 
                                         TRUE, getOption("geoR.messages")))
  else messages.screen <- messages
  obj.variog$v <- NULL
  if ((is.matrix(data) | is.data.frame(data))) 
    if (ncol(data) > 1) 
      stop("envelops can be computed for only one data set at once")
  if (!is.null(obj.variog$estimator.type)) 
    estimator.type <- obj.variog$estimator.type
  else estimator.type <- "classical"
  if (abs(obj.variog$lambda - 1) > 1e-04) {
    if (abs(obj.variog$lambda) < 1e-04) 
      data <- log(data)
    else data <- ((data^obj.variog$lambda) - 1)/obj.variog$lambda
  }
  xmat <- unclass(trend.spatial(trend = obj.variog$trend, geodata = geodata))
  if (obj.variog$trend != "cte") {
    if (is.vector(data)) {
      data <- lm(data ~ xmat + 0)$residuals
      names(data) <- NULL
    }
    else {
      only.res <- function(y, x) {
        lm(y ~ xmat + 0)$residuals
      }
      data <- apply(data, 2, only.res, x = xmat)
    }
  }
  if (messages.screen) 
    cat(paste("variog.env: generating", nsim, "simulations by permutating data values\n"))
  simula <- list(coords = coords)
  n.data <- length(data)
  perm.f <- function(i, data, n.data) {
    return(data[sample(1:n.data)])
  }
  simula$data <- apply(as.matrix(1:nsim), 1, perm.f, data = data, 
                       n.data = n.data)
  if (messages.screen) 
    cat(paste("variog.env: computing the empirical variogram for the", 
              nsim, "simulations\n"))
  nbins <- length(obj.variog$bins.lim) - 1
  if (obj.variog$direction == "omnidirectional") {
    bin.f <- function(sim) {
      cbin <- vbin <- sdbin <- rep(0, nbins)
      temp <- .C("binit", as.integer(obj.variog$n.data), 
                 as.double(as.vector(coords[, 1])), as.double(as.vector(coords[, 
                                                                               2])), as.double(as.vector(sim)), as.integer(nbins), 
                 as.double(as.vector(obj.variog$bins.lim)), as.integer(estimator.type == 
                                                                         "modulus"), as.double(max(obj.variog$u)), as.double(cbin), 
                 vbin = as.double(vbin), as.integer(FALSE), as.double(sdbin), 
                 PACKAGE = "geoR")$vbin
      return(temp)
    }
    simula.bins <- apply(simula$data, 2, bin.f)
  }
  else {
    variog.vbin <- function(x, ...) {
      variog(geodata = geodata, 
             data = x, uvec = obj.variog$uvec, estimator.type = obj.variog$estimator.type, 
             nugget.tolerance = obj.variog$nugget.tolerance, max.dist = obj.variog$max.dist, 
             pairs.min = obj.variog$pairs.min, direction = obj.variog$direction, 
             tolerance = obj.variog$tolerance, messages.screen = FALSE,...)$v
    }
    simula.bins <- apply(simula$data, 2, variog.vbin)
  }
  simula.bins <- simula.bins[obj.variog$ind.bin, ]
  if (save.sim == FALSE) 
    simula$data <- NULL
  if (messages.screen) 
    cat("variog.env: computing the envelops\n")
  limits <- apply(simula.bins, 1, quantile, prob = c(0.025, 0.975))
  res.env <- list(u = obj.variog$u, v.lower = limits[1, ], 
                  v.upper = limits[2, ])
  if (save.sim) 
    res.env$simulations <- simula$data
  res.env$call <- call.fc
  oldClass(res.env) <- "variogram.envelope"
  return(res.env)
}

# Calculate and plot the variogram
ggvario <- function(coords, 
                    data, 
                    bins = 15, 
                    maxdist = max(dist(coords))/3, 
                    uvec = NULL, 
                    nsim = 999,
                    color = "royalblue1", 
                    xlab = "distance", 
                    show_nbins = T) {
  require(geoR)
  coords <- as.matrix(coords)
  min_dist <- min(dist(coords))
  if(is.null(uvec)) uvec <- seq(min_dist, maxdist, l = bins)
  empvario <- variog(coords = coords, data = data, uvec = uvec, messages = F)
  envmc <- variog_envelope(coords = coords, data = data, 
                           obj.variog = empvario, nsim = nsim, messages = F)
  dfvario <- data.frame(distance = empvario$u, empirical = empvario$v,
                        lowemp = envmc$v.lower, upemp = envmc$v.upper, 
                        nbins = empvario$n)
  p1 <- ggplot(dfvario, aes(y = empirical, x = distance, label = nbins)) +
    geom_ribbon(aes(ymin = lowemp, ymax = upemp), fill = color, alpha = .3) +
    geom_point(aes(y = empirical), col = "black", fill = color, shape = 21, size = 3) +
    scale_x_continuous(name = xlab, limits = c(0, uvec[length(uvec)]),
                       breaks = round(seq(0, uvec[length(uvec)], l = 6))) +
    scale_y_continuous(name = "semivariance", 
                       limits = c(0, max(dfvario$upemp, dfvario$empirical))) +
    ggtitle("Empirical semivariogram") +
    theme_classic()
  p2 <- p1 + geom_text(vjust = 1, nudge_y = - diff(range(dfvario$empirical)) / 22)
  if(show_nbins) p2 else p1
}

# Function to fit a logistic binomial geostatistical model using
# a binomila model as input
geobinomial <- function(model,
                        coords,
                        kappa) {
  data <- model$data
  data$weights <-  model.weights(model.frame(model))
  data$infected <- as.numeric(model$y) * data$weights
  data$logitp <- log((data$infected + 0.5) / (data$weights - data$infected + 0.5))
  
  f <- model$formula
  f_linear <- update(f, logitp ~ .)
  f_binom <- update(f, infected ~ .)
  
  fit_MLE <- linear.model.MLE(formula = f_linear,
                              coords = coords, 
                              data = data, 
                              start.cov.pars = c(30, 0.2), 
                              kappa = kappa,
                              messages = F)
  
  
  par0 <- c(coef(model), coef(fit_MLE)[-c(1:length(coef(model)))]) 
  
  c.mcmc <- control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8,
                              h = (1.65) / (nrow(loaloa) ^ (1/6)))
  init_pars <- c(par0["phi"], par0["tau^2"] / par0["sigma^2"])
  
  fit_sp <- binomial.logistic.MCML(formula = f_binom,
                                   units.m = ~ weights, 
                                   coords = coords, 
                                   par0 = par0, 
                                   data = data, 
                                   control.mcmc = c.mcmc, 
                                   kappa = kappa, 
                                   start.cov.pars = init_pars, 
                                   plot.correlogram = F, 
                                   messages = T)
  fit_sp$formula <- f_binom
  return(fit_sp)
}


# Function for spatial prediction
spatial.pred.binomial.MCML <- function (object, grid.pred, predictors = NULL, control.mcmc, 
          type = "marginal", scale.predictions = c("logit", "prevalence", 
                                                   "odds"), quantiles = c(0.025, 0.975), standard.errors = FALSE, 
          thresholds = NULL, scale.thresholds = NULL, plot.correlogram = FALSE, 
          messages = TRUE) 
{
  if (nrow(grid.pred) < 2) 
    stop("prediction locations must be at least two.")
  if (length(predictors) > 0 && class(predictors) != "data.frame") 
    stop("'predictors' must be a data frame with columns' names matching those in the data used to fit the model.")
  if (length(predictors) > 0 && any(is.na(predictors))) 
    stop("missing values found in 'predictors'.")
  p <- object$p <- ncol(object$D)
  kappa <- object$kappa
  n.pred <- nrow(grid.pred)
  coords <- object$coords
  if (type == "marginal" & length(object$knots) > 0) 
    warning("only joint predictions are avilable for the low-rank approximation.")
  if (any(type == c("marginal", "joint")) == FALSE) 
    stop("type of predictions should be marginal or joint")
  for (i in 1:length(scale.predictions)) {
    if (any(c("logit", "prevalence", "odds") == scale.predictions[i]) == 
        FALSE) 
      stop("invalid scale.predictions.")
  }
  if (length(thresholds) > 0) {
    if (any(scale.predictions == scale.thresholds) == FALSE) {
      stop("scale thresholds must be equal to a scale prediction")
    }
  }
  if (length(thresholds) == 0 & length(scale.thresholds) > 
      0 | length(thresholds) > 0 & length(scale.thresholds) == 
      0) 
    stop("to estimate exceedance probabilities both thresholds and scale.thresholds.")
  if (object$p == 1) {
    predictors <- matrix(1, nrow = n.pred)
  }
  else {
    if (length(dim(predictors)) == 0) 
      stop("covariates at prediction locations should be provided.")
    predictors <- as.matrix(model.matrix(delete.response(terms(object$formula)), 
                                         data = predictors))
    if (nrow(predictors) != nrow(grid.pred)) 
      stop("the provided values for 'predictors' do not match the number of prediction locations in 'grid.pred'.")
    if (ncol(predictors) != ncol(object$D)) 
      stop("the provided variables in 'predictors' do not match the number of explanatory variables used to fit the model.")
  }
  out <- list()
  if (length(object$mesh) > 0) {
    if (type == "marginal") 
      warning("only joint predictions are available when using the SPDE approximation.")
    beta <- object$estimate[1:p]
    sigma2 <- exp(object$estimate[p + 1])
    phi <- exp(object$estimate[p + 2])
    sigma2.t <- 4 * pi * sigma2/(phi^2)
    mu <- as.numeric(object$D %*% beta)
    mu.pred <- as.numeric(predictors %*% beta)
    S.samples <- Laplace.sampling.SPDE(mu, sigma2, phi, 
                                       kappa = object$kappa, y = object$y, units.m = object$units.m, 
                                       coords = object$coords, mesh = object$mesh, control.mcmc = control.mcmc, 
                                       messages = messages, plot.correlogram = plot.correlogram, 
                                       poisson.llik = FALSE)
    A.pred <- INLA::inla.spde.make.A(object$mesh, loc = as.matrix(grid.pred))
    n.samples <- nrow(S.samples$samples)
    eta.sim <- sapply(1:n.samples, function(i) as.numeric(mu.pred + 
                                                            A.pred %*% S.samples$samples[i, ]))
  }
  else if (length(dim(object$knots)) > 0) {
    beta <- object$estimate[1:p]
    sigma2 <- exp(object$estimate["log(sigma^2)"])/object$const.sigma2
    rho <- exp(object$estimate["log(phi)"]) * 2 * sqrt(object$kappa)
    knots <- object$knots
    U.k <- as.matrix(pdist(coords, knots))
    K <- matern.kernel(U.k, rho, kappa)
    mu.pred <- as.numeric(predictors %*% beta)
    object$mu <- object$D %*% beta
    Z.sim.res <- Laplace.sampling.lr(object$mu, sigma2, 
                                     K, object$y, object$units.m, control.mcmc, plot.correlogram = plot.correlogram, 
                                     messages = messages, poisson.llik = FALSE)
    Z.sim <- Z.sim.res$samples
    U.k.pred <- as.matrix(pdist(grid.pred, knots))
    K.pred <- matern.kernel(U.k.pred, rho, kappa)
    eta.sim <- sapply(1:(dim(Z.sim)[1]), function(i) mu.pred + 
                        K.pred %*% Z.sim[i, ])
  }
  else {
    beta <- object$estimate[1:p]
    sigma2 <- exp(object$estimate[p + 1])
    phi <- exp(object$estimate[p + 2])
    if (length(object$fixed.rel.nugget) == 0) {
      tau2 <- sigma2 * exp(object$estimate[p + 3])
    }
    else {
      tau2 <- object$fixed.rel.nugget * sigma2
    }
    U <- dist(coords)
    U.pred.coords <- as.matrix(pdist(grid.pred, coords))
    Sigma <- geoR::varcov.spatial(dists.lowertri = U, cov.model = "matern", 
                                  cov.pars = c(sigma2, phi), nugget = tau2, kappa = kappa)$varcov
    Sigma.inv <- solve(Sigma)
    C <- sigma2 * geoR::matern(U.pred.coords, phi, kappa)
    A <- C %*% Sigma.inv
    mu.pred <- as.numeric(predictors %*% beta)
    object$mu <- object$D %*% beta
    if (length(object$ID.coords) > 0) {
      S.sim.res <- Laplace.sampling(object$mu, Sigma, 
                                    object$y, object$units.m, control.mcmc, object$ID.coords, 
                                    plot.correlogram = plot.correlogram, messages = messages)
      S.sim <- S.sim.res$samples
      mu.cond <- sapply(1:(dim(S.sim)[1]), function(i) mu.pred + 
                          A %*% S.sim[i, ])
    }
    else {
      S.sim.res <- Laplace.sampling(object$mu, Sigma, 
                                    object$y, object$units.m, control.mcmc, plot.correlogram = plot.correlogram, 
                                    messages = messages)
      S.sim <- S.sim.res$samples
      mu.cond <- sapply(1:(dim(S.sim)[1]), function(i) mu.pred + 
                          A %*% (S.sim[i, ] - object$mu))
    }
    if (type == "marginal") {
      if (messages) 
        cat("Type of predictions:", type, "\n")
      sd.cond <- sqrt(sigma2 - diag(A %*% t(C)))
    }
    else if (type == "joint") {
      if (messages) 
        cat("Type of predictions: ", type, " (this step might be demanding) \n")
      Sigma.pred <- geoR::varcov.spatial(coords = grid.pred, 
                                         cov.model = "matern", cov.pars = c(sigma2, phi), 
                                         kappa = kappa)$varcov
      Sigma.cond <- Sigma.pred - A %*% t(C)
      sd.cond <- sqrt(diag(Sigma.cond))
    }
    if ((length(quantiles) > 0) | (any(scale.predictions == 
                                       "prevalence")) | (any(scale.predictions == "odds")) | 
        (length(scale.thresholds) > 0)) {
      if (type == "marginal") {
        eta.sim <- sapply(1:(dim(S.sim)[1]), function(i) rnorm(n.pred, 
                                                               mu.cond[, i], sd.cond))
      }
      else if (type == "joint") {
        Sigma.cond.sroot <- t(chol(Sigma.cond))
        eta.sim <- sapply(1:(dim(S.sim)[1]), function(i) mu.cond[, 
                                                                 i] + Sigma.cond.sroot %*% rnorm(n.pred))
      }
    }
    if (any(scale.predictions == "logit")) {
      if (messages) 
        cat("Spatial predictions: logit \n")
      out$logit$predictions <- apply(mu.cond, 1, mean)
      if (standard.errors) {
        out$logit$standard.errors <- sqrt(sd.cond^2 + 
                                            diag(A %*% cov(S.sim) %*% t(A)))
      }
      if (length(quantiles) > 0) {
        out$logit$quantiles <- t(apply(eta.sim, 1, function(r) quantile(r, 
                                                                        quantiles)))
      }
      if (length(thresholds) > 0 && scale.thresholds == 
          "logit") {
        out$exceedance.prob <- matrix(NA, nrow = n.pred, 
                                      ncol = length(thresholds))
        colnames(out$exceedance.prob) <- paste(thresholds, 
                                               sep = "")
        for (j in 1:length(thresholds)) {
          out$exceedance.prob[, j] <- apply(eta.sim, 
                                            1, function(r) mean(r > thresholds[j]))
        }
      }
    }
    if (any(scale.predictions == "odds")) {
      if (messages) 
        cat("Spatial predictions: odds \n")
      odds.sim <- exp(eta.sim)
      out$odds$predictions <- apply(exp(mu.cond + 0.5 * 
                                          sd.cond^2), 1, mean)
      if (standard.errors) {
        out$odds$standard.errors <- apply(odds.sim, 
                                          1, sd)
      }
      if (length(quantiles) > 0) {
        out$odds$quantiles <- t(apply(odds.sim, 1, function(r) quantile(r, 
                                                                        quantiles)))
      }
      if (length(thresholds) > 0 && scale.thresholds == 
          "odds") {
        out$exceedance.prob <- matrix(NA, nrow = n.pred, 
                                      ncol = length(thresholds))
        colnames(out$exceedance.prob) <- paste(thresholds, 
                                               sep = "")
        for (j in 1:length(thresholds)) {
          out$exceedance.prob[, j] <- apply(odds.sim, 
                                            1, function(r) mean(r > thresholds[j]))
        }
      }
    }
  }
  appr <- length(object$knots) > 0 | length(object$mesh) > 
    0
  if (any(scale.predictions == "logit") & appr) {
    if (messages) 
      cat("Spatial predictions: logit \n")
    out$logit$predictions <- apply(eta.sim, 1, mean)
    if (standard.errors) {
      out$logit$standard.errors <- apply(eta.sim, 1, sd)
    }
    if (length(quantiles) > 0) {
      out$logit$quantiles <- t(apply(eta.sim, 1, function(r) quantile(r, 
                                                                      quantiles)))
    }
    if (length(thresholds) > 0 && scale.thresholds == "logit") {
      out$exceedance.prob <- matrix(NA, nrow = n.pred, 
                                    ncol = length(thresholds))
      colnames(out$exceedance.prob) <- paste(thresholds, 
                                             sep = "")
      for (j in 1:length(thresholds)) {
        out$exceedance.prob[, j] <- apply(eta.sim, 1, 
                                          function(r) mean(r > thresholds[j]))
      }
    }
  }
  if (any(scale.predictions == "odds") & appr) {
    if (messages) 
      cat("Spatial predictions: odds \n")
    odds.sim <- exp(eta.sim)
    out$odds$predictions <- apply(odds.sim, 1, mean)
    if (standard.errors) {
      out$odds$standard.errors <- apply(odds.sim, 1, sd)
    }
    if (length(quantiles) > 0) {
      out$odds$quantiles <- t(apply(odds.sim, 1, function(r) quantile(r, 
                                                                      quantiles)))
    }
    if (length(thresholds) > 0 && scale.thresholds == "odds") {
      out$exceedance.prob <- matrix(NA, nrow = n.pred, 
                                    ncol = length(thresholds))
      colnames(out$exceedance.prob) <- paste(thresholds, 
                                             sep = "")
      for (j in 1:length(thresholds)) {
        out$exceedance.prob[, j] <- apply(odds.sim, 
                                          1, function(r) mean(r > thresholds[j]))
      }
    }
  }
  if (any(scale.predictions == "prevalence")) {
    if (messages) 
      cat("Spatial predictions: prevalence \n")
    prev.sim <- exp(eta.sim)/(1 + exp(eta.sim))
    out$prevalence$predictions <- apply(prev.sim, 1, mean)
    if (standard.errors) {
      out$prevalence$standard.errors <- apply(prev.sim, 
                                              1, sd)
    }
    if (length(quantiles) > 0) {
      out$prevalence$quantiles <- t(apply(prev.sim, 1, 
                                          function(r) quantile(r, quantiles)))
    }
    if (length(thresholds) > 0 && scale.thresholds == "prevalence") {
      out$exceedance.prob <- matrix(NA, nrow = n.pred, 
                                    ncol = length(thresholds))
      colnames(out$exceedance.prob) <- paste(thresholds, 
                                             sep = "")
      for (j in 1:length(thresholds)) {
        out$exceedance.prob[, j] <- apply(prev.sim, 
                                          1, function(r) mean(r > thresholds[j]))
      }
    }
  }
  if (any(scale.predictions == "odds") | any(scale.predictions == 
                                             "prevalence") | appr) {
    out$samples <- eta.sim
  }
  out$grid <- grid.pred
  class(out) <- "pred.PrevMap"
  out
}


spatial_pred <- function(model, 
                         grid.pred, 
                         predictors) {
  
  c.mcmc <- control.mcmc.MCML(n.sim = 10000, burnin = 2000, thin = 8,
                              h = (1.65) / (nrow(loaloa) ^ (1/6)))
  
  
  spatial.pred.binomial.MCML(object = model, grid.pred = grid.pred, 
                             predictors = predictors, 
                             control.mcmc = c.mcmc, 
                             type = "marginal", 
                             scale.predictions = "prevalence", 
                             quantiles = c(0.025, 0.5, 0.975), 
                             standard.errors = TRUE,
                             messages = F)
}
