# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> pValT <<
# FFFFFFFFFFFF
#' Calculates t, 2-tailed p value, pAlpha CI
#'
#' @param pDiff The difference between the two raw scores of interest, or
#'     a parameter estimate.
#'
#' @param sE The applicable standard error for the difference or estimate.
#' @param pTails Number of tails (default 2). Any value other than 2 produces
#'     a 1-tailed result.
#'
#' @return A vector with three values: the t-ratio, the two-tailed p-value, and
#'     the pAlpha confidence limits.
#' @export
pValT <- function(pDiff, sE, pAlpha = 0.95, pTails = 2){
  tRat<-pDiff/sE
  # If 1-tailed:
  if (pTails != 2)
  {pTails <- 1
  #p-Value
  pVal <- (1-pnorm(abs(tRat)))*2

  # critical value
  critVal <- qnorm(pAlpha)
  } else{
    # If 2-tailed:
    # p-value
    pVal <- (1-pnorm(abs(tRat)))

    # critical value
    critVal <- qnorm(pAlpha + (1-pAlpha)/2)
  }
  # pAlpha Confidence Interval (critVal depends on 1 or 2-tailed)
  ciAlpha<-c(pDiff-(critVal*sE),pDiff+(critVal*sE))

  # Output
  cat(pTails, "-tailed distribution requested", "\n")
  cat("t-ratio = ", tRat, "\n")
  cat("p Value = ", pVal, "\n")
  cat("Conf Interval for alpha of", pAlpha, "= (",
      ciAlpha[1], ",", ciAlpha[2],")")
}
