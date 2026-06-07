#' Simulate BHM / CBHM BOP2-basket operating characteristics
#'
#' Simulates basket trial outcomes using either:
#' \itemize{
#'   \item \strong{BHM}: Bayesian hierarchical model borrowing
#'   \item \strong{CBHM}: calibrated BHM with \eqn{\sigma^2 = \exp(a + b \log(T))}
#' }
#'
#' Requires \pkg{R2jags} and an external JAGS installation.
#'
#' @param cohortsize Matrix of interim cohort sizes (rows = looks, cols = baskets).
#' @param ntype Number of baskets.
#' @param p.true Vector of true response rates by basket.
#' @param p.null Vector of null response rates by basket.
#' @param ntrial Number of simulated trials.
#' @param mu.par Prior mean for the common effect (log-odds scale).
#' @param v Prior precision for \code{mu} in BHM (JAGS precision parameter).
#' @param a,b Hyperparameters for the inverse-gamma prior on variance (BHM)
#'   or calibration parameters (CBHM uses \code{a,b} in the variance link).
#' @param lam,gam Tuning parameters used in interim futility/efficacy boundaries.
#' @param type1 Target type I error / family-wise error threshold.
#' @param method Either \code{"BHM"} or \code{"CBHM"}.
#'
#' @return A list with type I error estimate, efficacy rates by basket, sample size summaries,
#'   FWER estimate, power estimate, and early termination indicators.
#' @export
design <- function(cohortsize,
                   ntype = 5,
                   p.true,
                   p.null,
                   ntrial,
                   mu.par,
                   v = 0.01,
                   a, b,
                   lam, gam,
                   type1,
                   method = c("BHM", "CBHM")) {
  
  method <- match.arg(method)
  
  if (!requireNamespace("R2jags", quietly = TRUE)) {
    stop("Package 'R2jags' is required. Please install it and ensure JAGS is installed.")
  }
  
  effectsize <- function(pa) log(pa / (1 - pa))
  
  p.est <- matrix(0, nrow = ntrial, ncol = ntype)
  sample.size <- matrix(0, nrow = ntrial, ncol = ntype)
  ncohort <- nrow(cohortsize)
  arm.count <- matrix(0, nrow = ntrial, ncol = ncohort)
  test.stat <- matrix(0, nrow = ntrial, ncol = ncohort)
  
  efficacy <- NULL
  eff.prob.store <- matrix(0, nrow = ntrial, ncol = ntype)
  
  basket.true <- numeric(ntype)
  basket.true[p.true != p.null] <- 1
  
  terminate <- matrix(0, nrow = ntrial, ncol = ntype)
  
  if (method == "BHM") {
    
    bhm <- function() {
      for (j in 1:ntype) {
        y[j] ~ dbin(p[j], n[j])
        p[j] <- exp(theta[j]) / (1 + exp(theta[j]))
        theta[j] ~ dnorm(mu, tau)
      }
      mu ~ dnorm(mu.par, v)
      tau ~ dgamma(a, b)
      sigma <- 1 / sqrt(tau)
    }
    
    jags.params <- c("mu", "theta", "p", "tau", "sigma")
    jags.inits <- function() {
      list(theta = effectsize(p.true), mu = mu.par, tau = a / b)
    }
    
    for (trial in 1:ntrial) {
      
      set.seed(3000 + trial)
      n <- numeric(ntype)
      y <- numeric(ntype)
      stopping <- numeric(ntype)
      presponse <- p.true
      csize <- cohortsize
      
      for (i in 1:ncohort) {
        
        y <- y + stats::rbinom(rep(1, ntype), cohortsize[i, ], presponse)
        n <- n + csize[i, ]
        
        if (i != ncohort) {
          
          jags.data <- list(y = y, n = n, ntype = ntype, mu.par = mu.par, v = v, a = a, b = b)
          
          jagsfit <- R2jags::jags(data = jags.data, inits = jags.inits,
                                  parameters.to.save = jags.params,
                                  n.iter = 20000, n.burnin = 1000,
                                  model.file = bhm)
          jagsfit.upd <- R2jags::autojags(jagsfit, n.update = 100, n.iter = 10000)
          
          pres.est <- jagsfit.upd[[2]]$sims.list$p
          fut.prob <- sapply(seq_len(ntype), function(x) mean(pres.est[, x] <= p.null[x]))
          
          futstop <- 1 - lam * (n / colSums(cohortsize))^gam
          
          stopping[which(fut.prob > futstop)] <- 1
          sample.size[trial, which(stopping == 1)] <- n[which(stopping == 1)]
          terminate[trial, ] <- stopping
          
          if (1 %in% stopping) {
            presponse[which(stopping == 1)] <- 0
            csize[(i + 1), which(stopping == 1)] <- 0
          }
          
          if (!(0 %in% stopping)) {
            arm.count[trial, i + 1] <- 0
            eff.tmp <- numeric(ntype)
            efficacy <- rbind(efficacy, eff.tmp)
            p.est[trial, ] <- jagsfit.upd[[2]]$mean$p
            sample.size[trial, ] <- n
            terminate[trial, ] <- stopping
            break
          }
          
        } else {
          
          arm.count[trial, i] <- length(y[which(stopping == 0)])
          
          jags.data <- list(y = y, n = n, ntype = ntype, mu.par = mu.par, v = v, a = a, b = b)
          
          jagsfit <- R2jags::jags(data = jags.data, inits = jags.inits,
                                  parameters.to.save = jags.params,
                                  n.iter = 20000, n.burnin = 1000,
                                  model.file = bhm)
          jagsfit.upd <- R2jags::autojags(jagsfit, n.update = 100, n.iter = 10000)
          
          pres.est <- jagsfit.upd[[2]]$sims.list$p
          eff.prob <- sapply(seq_len(ntype), function(x) mean(pres.est[, x] <= p.null[x]))
          eff.prob.store[trial, ] <- eff.prob
          
          eff.ref <- 1 - lam
          eff.tmp <- (eff.prob < eff.ref) * 1
          eff.tmp[which(stopping == 1)] <- 0
          
          efficacy <- rbind(efficacy, eff.tmp)
          
          p.est[trial, ] <- jagsfit.upd[[2]]$mean$p
          sample.size[trial, which(stopping == 0)] <- n[which(stopping == 0)]
        }
      }
    }
  }
  
  if (method == "CBHM") {
    
    cbhm <- function() {
      for (j in 1:ntype) {
        y[j] ~ dbin(p[j], n[j])
        p[j] <- exp(theta[j]) / (1 + exp(theta[j]))
      }
      for (j in 1:ntype) {
        theta[j] ~ dnorm(mu, tau)
      }
      mu ~ dnorm(mu.par, 0.01)
    }
    
    jags.params <- c("mu", "theta", "p")
    jags.inits <- function() list(theta = effectsize(p.true), mu = mu.par)
    
    for (trial in 1:ntrial) {
      
      set.seed(100 + trial)
      n <- numeric(ntype)
      y <- numeric(ntype)
      stopping <- numeric(ntype)
      presponse <- p.true
      csize <- matrix(cohortsize, nrow = ncohort, ncol = ntype)
      
      for (i in 1:ncohort) {
        
        y <- y + stats::rbinom(rep(1, ntype), cohortsize[i, ], presponse)
        n <- n + csize[i, ]
        
        phat <- sum(y) / sum(n)
        obs <- cbind(y, n - y)
        E <- cbind(n * phat, n * (1 - phat))
        T <- sum((abs(obs - E))^2 / E)
        if (is.nan(T) || (T < 1)) T <- 1
        test.stat[trial, i] <- T
        
        sigma2 <- exp(a + b * log(T))
        if (is.infinite(sigma2)) sigma2 <- 1e4
        tau <- 1 / sigma2
        
        arm.count[trial, i] <- length(y[which(stopping == 0)])
        
        jags.data <- list(y = y, n = n, ntype = ntype, tau = tau, mu.par = mu.par)
        
        jagsfit <- R2jags::jags(data = jags.data, inits = jags.inits,
                                parameters.to.save = jags.params,
                                n.iter = 20000, n.burnin = 1000,
                                model.file = cbhm)
        jagsfit.upd <- R2jags::autojags(jagsfit, n.update = 100, n.iter = 10000)
        
        pres.est <- jagsfit.upd[[2]]$sims.list$p
        fut.prob <- sapply(seq_len(ntype), function(x) mean(pres.est[, x] <= p.null[x]))
        futstop <- 1 - lam * (n / colSums(cohortsize))^gam
        
        if (i != ncohort) {
          stopping[which(fut.prob > futstop)] <- 1
          sample.size[trial, which(stopping == 1)] <- n[which(stopping == 1)]
          terminate[trial, ] <- stopping
          
          if (1 %in% stopping) {
            presponse[which(stopping == 1)] <- 0
            csize[(i + 1), which(stopping == 1)] <- 0
          }
          
          if (!(0 %in% stopping)) {
            arm.count[trial, i + 1] <- 0
            eff.tmp <- numeric(ntype)
            efficacy <- rbind(efficacy, eff.tmp)
            p.est[trial, ] <- jagsfit.upd[[2]]$mean$p
            sample.size[trial, ] <- n
            terminate[trial, ] <- stopping
            break
          }
          
        } else {
          eff.prob <- sapply(seq_len(ntype), function(x) mean(pres.est[, x] <= p.null[x]))
          eff.prob.store[trial, ] <- eff.prob
          
          eff.ref <- 1 - lam
          eff.tmp <- (eff.prob <= eff.ref) * 1
          eff.tmp[which(stopping == 1)] <- 0
          
          efficacy <- rbind(efficacy, eff.tmp)
          p.est[trial, ] <- jagsfit.upd[[2]]$mean$p
          sample.size[trial, which(stopping == 0)] <- n[which(stopping == 0)]
        }
      }
    }
  }
  
  efficacy <- as.matrix(efficacy)
  
  type1.error <- sum((rowSums(efficacy) > 0) * 1) / ntrial
  result <- as.integer(type1.error >= type1)
  
  power1 <- array(NA_real_, ntrial)
  FWER <- array(NA_real_, ntrial)
  
  for (trial in 1:ntrial) {
    power1[trial] <- any(efficacy[trial, ] == 1 & basket.true == 1) * 1
    FWER[trial]  <- any(efficacy[trial, ] == 1 & basket.true == 0) * 1
  }
  
  message("probability of claiming efficacy by subgroup")
  cat(formatC(colMeans(efficacy), digits = 3, format = "f"), sep = "  ")
  cat("\n")
  
  message("average number of patients used")
  cat(formatC(mean(rowSums(sample.size)), digits = 1, format = "f"), "\n")
  
  message("estimated response rate by subgroup")
  cat(formatC(colMeans(p.est), digits = 2, format = "f"), sep = "  ")
  cat("\n")
  
  message("FWER")
  cat(formatC(mean(FWER, na.rm = TRUE), digits = 3, format = "f"), "\n")
  
  message("Power")
  cat(formatC(mean(power1, na.rm = TRUE), digits = 3, format = "f"), "\n")
  
  message("stop early")
  cat(formatC(colMeans(terminate), digits = 3, format = "f"), sep = " ")
  cat("\n")
  
  eff.est <- colMeans(efficacy)
  sp.est <- mean(rowSums(sample.size))
  sp.basket <- colMeans(sample.size)
  
  FWER.est <- mean(FWER, na.rm = TRUE)
  pow.est <- mean(power1, na.rm = TRUE)
  
  list(
    result = result,
    type1.error = type1.error,
    eff.est = eff.est,
    sp.est = sp.est,
    sp.basket = sp.basket,
    FWER = FWER.est,
    power = pow.est,
    terminate = terminate
  )
}