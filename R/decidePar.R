#' Calibrate CBHM variance parameters (a, b)
#'
#' Computes calibration parameters for the calibrated Bayesian hierarchical model (CBHM)
#' by linking the between-basket variance to a homogeneity statistic \eqn{T} using:
#' \deqn{\sigma^2 = \exp(a + b \log(T)).}
#'
#' @param cohortsize Matrix of interim cohort sizes (rows = looks, cols = baskets).
#' @param ntype Number of baskets.
#' @param ntrial Number of simulated trials for calibration.
#' @param p0 Vector of null response rates by basket.
#' @param p1 Vector of alternative response rates by basket.
#' @param var.small Pre-defined "small" variance target.
#' @param var.big Pre-defined "large" variance target.
#'
#' @return A list with elements \code{a} and \code{b}.
#' @export
decidePar <- function(cohortsize, ntype, ntrial, p0, p1, var.small, var.big) {
  
  presponse <- array(NA_real_, c(ntype, choose(ntype, floor(ntype / 2)), ntype))
  
  for (i in 1:ntype) {
    sig.bask <- combn(ntype, i)
    for (j in 1:ncol(sig.bask)) {
      presponse[i, j, ] <- p0
      sig.bask.num <- sig.bask[, j]
      presponse[i, j, sig.bask.num] <- p1[sig.bask.num]
    }
  }
  
  ncohort <- nrow(cohortsize)
  medianT.dim <- array(NA_real_, c(ntype, choose(ntype, floor(ntype / 2))))
  
  for (i in 1:ntype) {
    n.element <- choose(ntype, i)
    medianT <- NULL
    
    for (j in 1:n.element) {
      
      test.stat <- matrix(NA_real_, nrow = ntrial, ncol = ncohort)
      
      for (sim in 1:ntrial) {
        set.seed(100 + sim)
        n <- numeric(ntype)
        y <- numeric(ntype)
        
        for (r in 1:ncohort) {
          y <- y + stats::rbinom(rep(1, ntype), cohortsize[r, ], presponse[i, j, ])
          n <- n + cohortsize[r, ]
          p <- sum(y) / sum(n)
          
          x <- cbind(y, n - y)
          E <- cbind(n * p, n * (1 - p))
          T <- sum((abs(x - E))^2 / E)
          if (is.nan(T)) T <- 0
          
          test.stat[sim, r] <- T
        }
      }
      
      medianT <- c(medianT, stats::median(test.stat[, ncohort], na.rm = TRUE))
    }
    
    medianT.dim[i, seq_along(medianT)] <- medianT
  }
  
  medianT <- rowMeans(medianT.dim, na.rm = TRUE)
  heteroT <- min(medianT[1:(ntype - 1)])
  homoT <- medianT[ntype]
  
  b <- (log(var.big) - log(var.small)) / (log(heteroT) - log(homoT))
  a <- log(var.small) - b * log(homoT)
  
  list(a = a, b = b)
}
