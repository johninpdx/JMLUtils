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
  pVal <- (1-pnorm(abs(tRat)))

  # critical value
  critVal <- qnorm(pAlpha)
  } else{
    # If 2-tailed:
    # p-value
    pVal <- (1-pnorm(abs(tRat)))*2

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

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#     Two sienaGOF functions that are not included in RSiena or RSienaTest
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> GeodesicDistribution <<
#______________________________________________________________________________
#' Calculates geodesic distribution stats for sienaGOF
#'
#' For details see ?sna::geodist
#' @note The default for \code{levls} reflects that geodesic distances
#' larger than 5 do not differ appreciably with respect to interpretation.
#' @note Levels of the result are named, and used in the \code{plot} method
#' @export
GeodesicDistribution <- function (i, data, sims, period, groupName,
                                  varName, levls=c(1:5,Inf),
                                  cumulative=TRUE, ...) {
#' @import sna
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  require(sna)
  a <- sna::geodist(symmetrize(x))$gdist
  if (cumulative)
  {
    gdi <- sapply(levls, function(i){ sum(a<=i) })
  }
  else
  {
    gdi <- sapply(levls, function(i){ sum(a==i) })
  }
  names(gdi) <- as.character(levls)
  gdi
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >> TriadCensus <<
#______________________________________________________________________________
#' Holland and Leinhardt Triad Census; see ?sna::triad.census.
#'
#' @export
TriadCensus <- function(i, data, sims, wave, groupName, varName, levls=1:16){
#'@import sna
  unloadNamespace("igraph") # to avoid package clashes
  require(sna)
  require(network)
  x <- networkExtraction(i, data, sims, wave, groupName, varName)
  if (network.edgecount(x) <= 0){x <- symmetrize(x)}
  # because else triad.census(x) will lead to an error
  tc <- sna::triad.census(x)[1,levls]
  # triad names are transferred automatically
  tc
}
