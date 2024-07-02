# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> betaDiffZ <<
#______________________________________________________________________________
#' Z score and 2-tailed p of the difference between two regression betas
#'
#' @param b1 A regression coefficient
#' @param b2 A regression coefficient
#' @param seb1 The standard error of b1
#' @param seb2 The standard error of b2
#'
#' @return Prints Z and 2-tailed p value to the console
#' @export
betaDiffZ <- function(b1, b2, seb1, seb2){
  zDiff <- abs(b1-b2)/((seb1^2 + seb2^2)^0.5)
  cat("Z =", zDiff, "  2-tailed p=", 2*(1-pnorm(zDiff)))
}

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

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#        >> JNSiena <<
#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#'
#' Calculates Johnson-Neyman significance regions and MOM signficance
#'   values (raw and Bonferroni-Holm adjusted) for SAOM 2-way
#'   interactions.
#'
#' @param siena07out An object of class 'sienaFit'
#' @param theta1  Number (from RSiena output) of 1st effect involved in
#'     a 2-way interaction effect included in 'siena07out'. This effect
#'     will be the primary predictor for the first table of pick-a-point
#'     results in the output, and the moderator in the second such table.
#' @param theta2 Number (from RSiena output) of 2st effect involved in
#'     a 2-way interaction effect included in 'siena07out'. This effect
#'     will be the moderator for the first table of pick-a-point
#'     results in the output, and the primary predictor in the second
#'     such table.
#' @param thetaInt Number (from RSiena output) of the multiplicative
#'     interaction of the two effects identified by theta1 and theta2.
#' @param theta1vals A vector of numerical values of effect 1 for which raw and
#'     B-H-adjusted significance should be obtained, applying both to
#'     effect 1 as a predictor (in the 1st output table) and as a
#'     moderator (in the 2nd output table).
#' @param theta2vals A vector of numerical values of effect 2 for which raw and
#'     B-H-adjusted significance should be obtained, applying both to
#'     effect 2 as a predictor (in the 2nd output table) and as a
#'     moderator (in the 1st output table).
#' @param sigRegion Logical; default TRUE. If TRUE, Johnson-Neyman (raw)
#'     significance limits are calculated for the case of effect 1 as
#'     primary and effect 2 as moderator, and vice versa.
#' @param alpha Number (default = 0.05) corresponding the 2-tailed significance
#'     level used to calculate whether a moderated effect is significant for a
#'     given value (for points in theta1vals and theta2vals) or range
#'     (for JN significance region computation).
#' @return A list. The first two elements of the list are dataframes containing
#'     both raw and B-H-adjusted significance (the former a p-value, the latter
#'     simply TRUE or FALSE) of the primary effect for each value of the
#'     moderator given by thetaXvals (X=1 or 2). The first such table treats
#'     effect 1 as primary, moderated by effect 2, and the second table is the
#'     reverse. Additionally, if sigRegion is TRUE, a third dataframe is included
#'     containing the significance region boundaries for each effect as primary,
#'     moderated by the other.
#' @export
JNSiena <- function(siena07out, # siena07 output
                    theta1, #number of the first parameter involved in the
                    # interaction - check the model results
                    theta2, # number of the 2nd parameter
                    thetaInt, # number of the interaction
                    theta1vals, # range of the statistics corresponding the 1st parameter
                    theta2vals,  # range for the statistics corresponding the 2nd parameter
                    sigRegion= TRUE, #Logical: calculate Region of Significance limits
                    alpha = 0.05) # significance range,
{
  if (class(siena07out) != 'sienaFit') {
    stop('sienaout needs to be a sienaFit object created by siena07().')
  }

  if (theta1 > length(siena07out$effects$effectName) ||
      theta2 > length(siena07out$effects$effectName)) {
    cat('theta1 and theta2 need to be the number of the effect in the sienaFit')
    cat('object. The provided numbers exceed the existing parameters.')
    stop()
  }
  #Get the effectName strings for the effects with numbers theta1 & theta2
  # (e.g. "RF ego", "RF ego x RF alter", etc.).
  # 'effects' is the effects table subset of model-included rows
  theta1n <- siena07out$effects$effectName[theta1]
  theta2n <- siena07out$effects$effectName[theta2]

  # _________________________________________________
  # theta1 is primary predictor, theta2 is moderator
  # _________________________________________________
  # Vector of g1+g3 multiplied by each bespoke moderator (x2) value.
  theta1s <- siena07out$theta[theta1] + siena07out$theta[thetaInt] * theta2vals

  #For each bespoke value of x2 in 'theta2vals' (the moderator vals),
  # calculate the SE for the parameter g1 (the main predictor).
  # See eq.(13) in Bauer&Curran(2005). It's the SE of the 'simple slope' w1 of
  # g1+g3*x2.
  seT1 <- sqrt(siena07out$covtheta[theta1,theta1] +
                 theta2vals * 2 * siena07out$covtheta[thetaInt,theta1] +
                 theta2vals ^ 2 * siena07out$covtheta[thetaInt,thetaInt])

  #Calculate a t-ratio for g1 at each bespoke value of x2.
  z1 <- theta1s/seT1

  #For each element of theta2vals, calculate the lesser of
  # the upper or lower end of the N distribution.
  # NOTICE that this is basically the pick-a-point method for
  # establishing conditional significance of theta1 depending
  # on the value of theta2 (or vice versa). But by selecting
  # different sets of values of theta1vals and theta2vals,
  # you can infer the critical value (s) of a moderator, for
  # which the relationship between the primary predictor becomes
  # a significant predictor (e.g. of tie formation)...(and of course
  # you can designate either predictor as primary or moderator).
  p1 <- c()
  for (i in 1:length(theta2vals)) {
    #Trick to select the correct double-sided critical region
    p1[i] <-  2 * pmin(pnorm(z1[[i]]), (1 - pnorm(z1[[i]])))
  }

  p1O <- p1[order(p1)]
  #This is the Bonferroni-Holm adjustment:
  bhT1 <- alpha * c(1:length(theta2vals))/length(theta2vals)
  sig1 <- p1O < bhT1 #Are the t's significant at the BH-adjusted levels?
  sig11 <- vector(length = length(theta2vals))
  for (i in 1:length(theta2vals)) {
    sig11[order(p1)[i]] <- sig1[i]
  }
  #The dataframe of x1 as primary and x2 as moderator
  t1d <- data.frame(theta      = rep(theta1n,length(theta2vals)),
                    moderator  = rep(theta2n,length(theta2vals)),
                    modvalues = round(theta2vals,4),
                    thetavals  = round(theta1s,4),
                    thetase    = round(seT1,4),
                    thetap     = round(p1,3),
                    significance_adjusted = sig11)

  # ___________________________________________________________________
  # Calculate JN Region of Significance based on unajusted significance
  #   with theta1 as primary and theta2 as moderator
  # ___________________________________________________________________
  if(sigRegion){
    mod <- siena07out
    g1 <- mod$theta[theta1] #Beta: primary (ego)
    g2 <- mod$theta[theta2] #Beta: moderator (alt)
    g3 <- mod$theta[thetaInt] #Beta: interaction
    g1Var <- mod$covtheta[theta1,theta1] #Var primary
    g2Var <- mod$covtheta[theta2,theta2] #Var moderator
    g3Var <- mod$covtheta[thetaInt,thetaInt] #Var (prim x mod)
    g1g3Cov <-mod$covtheta[theta1,thetaInt] #Cov(prim, int)
    tSq <- (qnorm((1-alpha) + (1-(1-alpha))/2))^2 #squared critical value (for alpha 2-tailed)

    # Calculate quadratic coefficients for x^2, x, and constant terms
    acoeff <- (tSq*g3Var) - g3^2
    bcoeff <- 2*((tSq*g1g3Cov) - (g1*g3))
    ccoeff <- (tSq*g1Var) - g1^2

    # Will the equation have real roots?
    isThisPos<-(bcoeff^2)-(4*acoeff*ccoeff) #If not, the equation has no real roots.
    if(isThisPos<0){
      cat(paste("When ", theta1n, " is primary, the significance region cannot be calculated (no real roots)."))
      rootMin.1 <- NA
      rootMax.1 <- NA}
    else{
      # Quadratic formula
      rootPos.1 <- (-bcoeff + sqrt((bcoeff^2)-(4*acoeff*ccoeff)))/(2*acoeff)
      rootNeg.1 <- (-bcoeff - sqrt((bcoeff^2)-(4*acoeff*ccoeff)))/(2*acoeff)
      rootMin.1 <- min(rootPos.1, rootNeg.1)
      rootMax.1 <- max(rootPos.1, rootNeg.1)
    }
  }


  # ________________________________________________________
  #theta2 is the primary predictor, theta1 is the moderator
  #_________________________________________________________
  theta2s <- siena07out$theta[theta2] + siena07out$theta[thetaInt] * theta1vals
  seT2 <- sqrt(siena07out$covtheta[theta2,theta2] +
                 theta1vals * 2 * siena07out$covtheta[thetaInt,theta2] +
                 theta1vals ^ 2 * siena07out$covtheta[thetaInt,thetaInt])

  z2 <- theta2s/seT2
  p2 <- c()
  for (i in 1:length(theta1vals)) {
    p2[i] <-  2 * pmin(pnorm(z2[[i]]), (1 - pnorm(z2[[i]])))
  }

  p2O <- p2[order(p2)]
  bhT2 <- alpha * c(1:length(theta1vals))/length(theta1vals)
  sig2 <- p2O < bhT2
  sig22 <- vector(length = length(theta1vals))
  for (i in 1:length(theta1vals)) {
    sig22[order(p2)[i]] <- sig2[i]
  }


  t2d <- data.frame(theta      = rep(theta2n,length(theta1vals)),
                    moderator  = rep(theta1n,length(theta1vals)),
                    modvalues = round(theta1vals,4),
                    thetavals = round(theta2s,4),
                    thetase   = round(seT2,4),
                    thetap    = round(p2,3),
                    significance_adjusted = sig22)

  # ___________________________________________________________________
  # Calculate JN Region of Significance based on unajusted significance
  #   with theta2 as primary and theta1 as moderator
  # ___________________________________________________________________
  if(sigRegion){
    mod <- siena07out
    g1 <- mod$theta[theta2] #Beta: primary (ego)
    g2 <- mod$theta[theta1] #Beta: moderator (alt)
    g3 <- mod$theta[thetaInt] #Beta: interaction
    g1Var <- mod$covtheta[theta2,theta2] #Var primary
    g2Var <- mod$covtheta[theta1,theta1] #Var moderator
    g3Var <- mod$covtheta[thetaInt,thetaInt] #Var (prim x mod)
    g1g3Cov <-mod$covtheta[theta2,thetaInt] #Cov(prim, int)
    tSq <- (qnorm((1-alpha) + (1-(1-alpha))/2))^2 #squared critical value (for alpha 2-tailed)

    # Calculate quadratic coefficients for x^2, x, and constant terms
    acoeff <- (tSq*g3Var) - g3^2
    bcoeff <- 2*((tSq*g1g3Cov) - (g1*g3))
    ccoeff <- (tSq*g1Var) - g1^2

    # Will the equation have real roots?
    isThisPos<-(bcoeff^2)-(4*acoeff*ccoeff) #If it isn't, the equation has no real roots.
    if(isThisPos<0){
      cat(paste("When ", theta2n, " is primary, the significance region cannot be calculated (no real roots)."))
      rootMin.2 <- NA
      rootMax.2 <- NA}
    else{
      # Quadratic formula
      rootPos.2 <- (-bcoeff + sqrt((bcoeff^2)-(4*acoeff*ccoeff)))/(2*acoeff)
      rootNeg.2 <- (-bcoeff - sqrt((bcoeff^2)-(4*acoeff*ccoeff)))/(2*acoeff)
      # Order with min before max for clearer output
      rootMin.2 <- min(rootPos.2, rootNeg.2)
      rootMax.2 <- max(rootPos.2, rootNeg.2)
    }

    sigRegions <- data.frame(theta = c(theta1n, theta2n),
                             moderator = c(theta2n, theta1n),
                             modMins=c(rootMin.1, rootMin.2),
                             modMaxes=c(rootMax.1, rootMax.2))
    return(list(t1d, t2d, sigRegions))
  } else{
    return(list(t1d,t2d))
  }
}



