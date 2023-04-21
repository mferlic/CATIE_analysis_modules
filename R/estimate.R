#' estimate
#'
#' @description
#' Defines a rough equivalent to proc genmod's estimate statement.
#
#'
#' @param fit model object of class geepack or geem
#' @param combos vector of linear combinations of parameter estimates.
#'
#' @return prints Coefmat
#'
#'
#' @export
#'
estimate <- function(fit, combos) {
  # Compute the mean estimate
  est <- combos %*% as.matrix(coef(fit))

  # Check fit class
  # Get the appropriate variance estimate
  if (is(fit, "geem")){
    var <- as.matrix(fit$var)
  } else if (is(fit, "geeglm")) {
    if(with(fit$geese, all(weights == 1) & max(clusz) == 1)) {
      var <- fit$geese$vbeta.naiv
    } else {var <- fit$geese$vbeta}
  } else {
    stop("fit is not of class geeM or geeglm")
  }


  # Compute standard error of mean estimate and confidence bounds
  se.est <- sqrt(diag(combos %*% var %*% t(combos)))
  lcl <- est - se.est * qnorm(0.975)
  ucl <- est + se.est * qnorm(0.975)

  # Perform a 1-degree-of-freedom Wald test on the estimate
  pvalue <- 1 - pchisq((est/se.est)^2, df = 1)

  # Combine and format output
  out <- cbind(est, lcl, ucl, se.est, pvalue)
  rownames(out) <- rownames(combos)
  colnames(out) <- c("Estimate", "95% LCL", "95% UCL", "SE", "p-value")
  class(out) <- c(class(out), "estimate") # inherit matrix class
  out
}


# Define a print function for the "estimate" class
#' print.estimate
#'
#' @param object of class "estimate"
#' @param digits number of sig digits to display
#' @param signif.stars show stars?
#' @param ...
#'
#' @return printCoefmat
#' @export
#'
print.estimate <- function(object, digits = 4, signif.stars = TRUE, ...) {
  ## print 4 decimal places by default, like SAS
  printCoefmat(object, digits = digits,
               signif.stars = signif.stars, has.Pvalue = TRUE,
               eps.Pvalue = 0.0001, ...)
}

