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
#        >> JNSiena.2way <<
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
#' @return A list of 5 elements.
#'     \enumerate{
#'        \item{1. *thetas* Three matrices of the predicted theta values
#'        of each of predictors 1, 2, and 3, calculated on each pair of
#'        values in the range (or discrete values) provided for each
#'        of the remaining two moderators.}
#'        \item{2. *standard_errors* SEs calculated conditionally in the
#'        same way as for the thetas.}
#'        \item{3. *p_values* P-values calculated conditionally in the
#'        same way as for the thetas.}
#'        \item{4. *significance* TRUE if the p-value falls within the
#'        Region Of Significance, as specified by alpha and, possibly also
#'        any adjustment for multiple tests; otherwise FALSE. Calculated
#'        in a matrix with the same conditions as for thetas, etc.}
#'        \item{5. *plots* A list with three class gg and ggplot elements as
#'        its members, corresponding to each of the three predictors being
#'        taken as the primary predictor, with the same ordering as for the
#'        previous list elements (theta, etc.). }
#'        }
#' @export
JNSiena.2way <- function(siena07out, # siena07 output
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

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#        >> JNSiena5.3way <<
#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#'
#' Calculates Johnson-Neyman significance regions and MOM signficance
#'   values (raw and Bonferroni-Holm adjusted) for SAOM 2-way
#'   interactions.
#'
#' @param siena07out An object of class 'sienaFit'
#' @param theta1  Number (from RSiena output) of 1st effect involved in
#'     a 2/3-way interaction effect included in 'siena07out'.
#' @param theta2 Number (from RSiena output) of 2st effect involved in
#'     a 2/3-way interaction effect included in 'siena07out'.
#' @param theta3 Number (from RSiena output) of 3rd effect involved in
#'     a 2/3-way interaction effect included in 'siena07out'. If it is
#'     not included in the function call, a 2-way interaction is
#'     assumed involving theta1 and theta2
#' @param thetaInt12 Number (from RSiena output) of the multiplicative
#'     interaction of the two effects identified by theta1 and theta2.
#'     This is always required.
#' @param thetaInt13 If theta3 is included, the number (from RSiena output)
#'     of the multiplicative interaction of the effects identified as
#'     theta1 and theta3.
#' @param thetaInt23 If theta3 is included, the number (from RSiena output)
#'     of the multiplicative interaction of the effects identified as
#'     theta2 and theta3.
#' @param thetaInt123 If theta3 is included, the number (from RSiena output)
#'     of the multiplicative interaction of the effects identified as
#'     theta1, theta2 and theta3.
#' @param theta1vals A vector of numerical values of effect 1 for which raw and
#'     B-H-adjusted significance should be obtained, applying both to
#'     effect 1 as a predictor (in the 1st output table) and as a
#'     moderator (in the 2nd output table).
#' @param theta2vals A vector of numerical values of effect 2 for which raw and
#'     B-H-adjusted significance should be obtained, applying both to
#'     effect 2 as a predictor (in the 2nd output table) and as a
#'     moderator (in the 1st output table).
#' @param theta3vals (Default NULL) A vector of numerical values of effect 3
#'     for which significance should be obtained, applying both to
#'     effect 3 as a predictor and as a moderator.
#' @param control_fdr Logical (T/F). Should the program output moderated
#'     significance results using Bonferroni-Holm-adjusted p-value
#'     criteria? Default= FALSE.
#' @param alpha Number (default = 0.05) corresponding the 2-tailed significance
#'     level used to calculate whether a moderated effect is significant for a
#'     given value (for points in theta1vals, theta2vals, and theta3vals)
#'     ranges.
#' @param roundres Number of significant digits in output. Default = 3.
#' @param color_low Color to use in heat maps of 3-way results for the low end of
#'     the moderated parameter values.
#' @param color_high Color to use in heat maps of 3-way results for the high end
#'     of the moderated parameter values.
#' @param color_values Color to use in heat maps of 3-way results for actual
#'     numerical values. This option appears not to be implemented yet.
#' @param color_grid Color of the grid in the plot.
#' @param grid_density (Default 0.01) How many values per unit area in the grid?
#' @param grid_spacing (Default 0.1) How far apart should the gridlines be?
#' @param gtitle (Default TRUE) Logical; add the primary predictor as a
#'     left-justified  title to the graph?
#' @return A list of 5 elements.
#'     \enumerate{
#'        \item{1. *thetas* Three matrices of the predicted theta values
#'        of each of predictors 1, 2, and 3, calculated on each pair of
#'        values in the range (or discrete values) provided for each
#'        of the remaining two moderators.}
#'        \item{2. *standard_errors* SEs calculated conditionally in the
#'        same way as for the thetas.}
#'        \item{3. *p_values* P-values calculated conditionally in the
#'        same way as for the thetas.}
#'        \item{4. *significance* TRUE if the p-value falls within the
#'        Region Of Significance, as specified by alpha and, possibly also
#'        any adjustment for multiple tests; otherwise FALSE. Calculated
#'        in a matrix with the same conditions as for thetas, etc.}
#'        \item{5. *plots* A list with three class gg and ggplot elements as
#'        its members, corresponding to each of the three predictors being
#'        taken as the primary predictor, with the same ordering as for the
#'        previous list elements (theta, etc.).}
#'        }
#' @export
JNSiena5.3way <- function(siena07out,
                     theta1,
                     theta2,
                     theta3 = NULL,
                     thetaInt12,
                     thetaInt13 = NULL,
                     thetaInt23 = NULL,
                     thetaInt123 = NULL,
                     theta1Vals,
                     theta2Vals,
                     theta3Vals = NULL,
                     control_fdr = FALSE,
                     alpha = 0.05,
                     round_res = 3,
                     color_mid = 'white',
                     color_low = '#F05039',
                     color_high = '#000066',
                     color_values = 'grey40',
                     color_grid = 'black',
                     grid_density = 0.01,
                     grid_spacing = 0.1,
                     g_title = TRUE) {


  library(ggplot2)
  library(ggpattern)
  library(tidyverse)
  library(scales)

  if (class(siena07out) != 'sienaFit') {
    stop('sienaout needs to be a sienaFit object created by siena07().')
  }

  sn <- siena07out$effects$effectName

  if (any(c(theta1 ,theta2, theta3, thetaInt12, thetaInt13, thetaInt23,
            thetaInt123) > length(sn))) {
    cat('The following parameter numbers are incorrect:\n',
        if (theta1 > length(sn)) {'theta1\n'},
        if (theta2 > length(sn)) {'theta2\n'},
        if (theta3 > length(sn)) {'theta3\n'},
        if (thetaInt12 > length(sn)) {'thetaInt12\n'},
        if (thetaInt13 > length(sn)) {'thetaInt13\n'},
        if (thetaInt23 > length(sn)) {'thetaInt23\n'},
        if (thetaInt123 > length(sn)) {'thetaInt123\n'},
        'Numbers need to be the number of the effect in the sienaFit object.\n',
        'See most left column of your siena07() result call.\n\n')
    stop()
  }

  theta1n <- sn[theta1]
  theta2n <- sn[theta2]

  covT <- siena07out$covtheta
  thetas <- siena07out$theta

  if (is.null(theta3)) {
    theta1s <- thetas[theta1] + thetas[thetaInt12] * theta2Vals
    seT1 <- sqrt(covT[theta1,theta1] +
                   theta2Vals * 2 * covT[thetaInt12,theta1] +
                   theta2Vals ^ 2 * covT[thetaInt12,thetaInt12])

    z1 <- theta1s/seT1
    p1 <- c()
    for (i in 1:length(theta2Vals)) {
      p1[i] <-  2 * pmin(pnorm(z1[[i]]), (1 - pnorm(z1[[i]])))
    }
    if (control_fdr) {
      p1O <- p1[order(p1)]
      bhT1 <- alpha * c(1:length(theta2Vals))/length(theta2Vals)
      sig1 <- p1O < bhT1
      sig11 <- vector(length = length(theta2Vals))
      for (i in 1:length(theta2Vals)) {
        sig11[order(p1)[i]] <- sig1[i]
      }
    } else {
      sig11 <- p1 < alpha
    }



    t1d <- data.frame(theta      = rep(theta1n,length(theta2Vals)),
                      moderator  = rep(theta2n,length(theta2Vals)),
                      mod_values = round(theta2Vals,round_res),
                      thetaVals  = round(theta1s,round_res),
                      thetase    = round(seT1,round_res),
                      thetap     = round(p1,3),
                      significance_adjusted = sig11)


    theta2s <- thetas[theta2] + thetas[thetaInt12] * theta1Vals
    seT2 <- sqrt(covT[theta2,theta2] +
                   theta1Vals * 2 * covT[thetaInt12,theta2] +
                   theta1Vals ^ 2 * covT[thetaInt12,thetaInt12])

    z2 <- theta2s/seT2
    p2 <- c()
    for (i in 1:length(theta1Vals)) {
      p2[i] <-  2 * pmin(pnorm(z2[[i]]), (1 - pnorm(z2[[i]])))
    }



    for (i in 1:length(theta2Vals)) {
      p1[i] <-  2 * pmin(pnorm(z1[[i]]), (1 - pnorm(z1[[i]])))
    }
    if (control_fdr) {
      p2O <- p2[order(p2)]
      bhT2 <- alpha * c(1:length(theta1Vals))/length(theta1Vals)
      sig2 <- p2O < bhT2
      sig22 <- vector(length = length(theta1Vals))
      for (i in 1:length(theta1Vals)) {
        sig22[order(p2)[i]] <- sig2[i]
      }
    } else {
      sig22 <- p2 < alpha
    }

    t2d <- data.frame(theta      = rep(theta2n,length(theta1Vals)),
                      moderator  = rep(theta1n,length(theta1Vals)),
                      mod_values = round(theta1Vals,round_res),
                      theta_Vals = round(theta2s,round_res),
                      theta_se   = round(seT2,round_res),
                      theta_p    = round(p2,3),
                      significance_adjusted = sig22)
    return_list <- list(t1d,t2d)



  } else {
    theta3n <- sn[theta3]

    thetaMat <- vector(mode = 'list', length = 3)
    seMat    <- vector(mode = 'list', length = 3)
    ZMat     <- vector(mode = 'list', length = 3)
    pMat     <- vector(mode = 'list', length = 3)
    sigMat   <- vector(mode = 'list', length = 3)
    figures  <- vector(mode = 'list', length = 3)
    names(thetaMat) <- names(seMat) <- names(figures) <- c(theta1n,theta2n,
                                                           theta3n)
    names(ZMat) <- names(pMat) <- names(sigMat) <- c(theta1n,theta2n,theta3n)


    parFun <- function(m1,m2,
                       bx,
                       bm1x,
                       bm2x,
                       bm1m2x) {
      bx + m1 * bm1x  + m2 * bm2x  + m1 * m2 * bm1m2x
    }

    seFun <- function(m1,m2,
                      Vx,
                      Vm1x,
                      Vm2x,
                      Vm1m2x,
                      covX_m1x,
                      covX_m2x,
                      covX_m1m2x,
                      covm1x_m2x,
                      covm1x_m1m2x,
                      covm2x_m1m2x) {
      sqrt(Vx +
             m1^2 * Vm1x +
             m2^2 * Vm2x +
             (m1 * m2)^2 * Vm1m2x +
             2 * m1 * covX_m1x +
             2 * m2 * covX_m2x +
             2 * m1 * m2 * covX_m1m2x +
             2 * m1 * m2 * covm1x_m2x +
             2 * m1^2 * m2 * covm1x_m1m2x +
             2 * m1 * m2^2 * covm2x_m1m2x)
    }



    thetaMat[[1]] <- outer(theta2Vals,theta3Vals,
                           parFun,
                           bx     = thetas[theta1],
                           bm1x   = thetas[thetaInt12],
                           bm2x   = thetas[thetaInt13],
                           bm1m2x = thetas[thetaInt123])

    thetaMat[[2]] <- outer(theta1Vals,theta3Vals,
                           parFun,
                           bx     = thetas[theta2],
                           bm1x   = thetas[thetaInt12],
                           bm2x   = thetas[thetaInt23],
                           bm1m2x = thetas[thetaInt123])

    thetaMat[[3]] <- outer(theta1Vals,theta2Vals,
                           parFun,
                           bx     = thetas[theta3],
                           bm1x   = thetas[thetaInt13],
                           bm2x   = thetas[thetaInt23],
                           bm1m2x = thetas[thetaInt123])


    seMat[[1]] <-  outer(theta2Vals,theta3Vals,
                         seFun,
                         Vx           = covT[theta1,theta1],
                         Vm1x         = covT[thetaInt12,thetaInt12],
                         Vm2x         = covT[thetaInt13,thetaInt13],
                         Vm1m2x       = covT[thetaInt123,thetaInt123],
                         covX_m1x     = covT[theta1,thetaInt12],
                         covX_m2x     = covT[theta1,thetaInt13],
                         covX_m1m2x   = covT[theta1,thetaInt123],
                         covm1x_m2x   = covT[thetaInt12,thetaInt13],
                         covm1x_m1m2x = covT[thetaInt12,thetaInt123],
                         covm2x_m1m2x = covT[thetaInt13,thetaInt123])

    seMat[[2]] <- outer(theta1Vals,theta3Vals,
                        seFun,
                        Vx           = covT[theta2,theta2],
                        Vm1x         = covT[thetaInt12,thetaInt12],
                        Vm2x         = covT[thetaInt23,thetaInt23],
                        Vm1m2x       = covT[thetaInt123,thetaInt123],
                        covX_m1x     = covT[theta2,thetaInt12],
                        covX_m2x     = covT[theta2,thetaInt23],
                        covX_m1m2x   = covT[theta2,thetaInt123],
                        covm1x_m2x   = covT[thetaInt12,thetaInt23],
                        covm1x_m1m2x = covT[thetaInt12,thetaInt123],
                        covm2x_m1m2x = covT[thetaInt23,thetaInt123])

    seMat[[3]] <- outer(theta1Vals,theta2Vals,
                        seFun,
                        Vx           = covT[theta3,theta3],
                        Vm1x         = covT[thetaInt13,thetaInt13],
                        Vm2x         = covT[thetaInt23,thetaInt23],
                        Vm1m2x       = covT[thetaInt123,thetaInt123],
                        covX_m1x     = covT[theta3,thetaInt13],
                        covX_m2x     = covT[theta3,thetaInt23],
                        covX_m1m2x   = covT[theta3,thetaInt123],
                        covm1x_m2x   = covT[thetaInt13,thetaInt23],
                        covm1x_m1m2x = covT[thetaInt13,thetaInt123],
                        covm2x_m1m2x = covT[thetaInt23,thetaInt123])

    vals <- list(theta1Vals,theta2Vals,theta3Vals)
    ns <- list(theta1n,theta2n,theta3n)
    for (i in 1:3) {
      #naming with parameter name might interfere with plot
      # thus only values so far, otherwise:paste(ns[-i][[1]],vals[-i][[1]])
      row.names(thetaMat[[i]]) <- row.names(seMat[[i]]) <- vals[-i][[1]]
      colnames(thetaMat[[i]])  <- colnames(seMat[[i]])  <- vals[-i][[2]]
      ZMat[[i]]   <- thetaMat[[i]] / seMat[[i]]
      pMat[[i]]   <- 2 * pmin(pnorm(ZMat[[i]]), (1 - pnorm(ZMat[[i]])))
      sigMat[[i]] <- pMat[[i]] < alpha



      dat2 <- thetaMat[[i]] |>
        as.data.frame() |>
        rownames_to_column("Var1") |>
        pivot_longer(-Var1, names_to = "Var2", values_to = "Parameter Value")

      sig2 <- sigMat[[i]] |>
        as.data.frame() |>
        rownames_to_column("Var1") |>
        pivot_longer(-Var1, names_to = "Var2", values_to = "value")

      dat2$Var1 <- as.numeric(dat2$Var1)
      dat2$Var2 <- as.numeric(dat2$Var2)
      sig2$Var1 <- as.numeric(dat2$Var1)
      sig2$Var2 <- as.numeric(dat2$Var2)

      sig2$pattern <- ifelse(sig2$value, "none",'crosshatch')
      sig3 <- sig2[!sig2$value,]


      figures[[i]] <- ggplot(dat2, aes(Var1, Var2)) +
        geom_tile(aes(fill = `Parameter Value`)) +
        scale_color_identity() +
        scale_fill_gradient2(low = color_low,
                             high = color_high,
                             mid = color_mid,
                             midpoint = 0) +
        geom_tile_pattern(data = sig3, aes(pattern = pattern),
                          pattern_density = grid_density,
                          pattern_spacing = grid_spacing,
                          pattern_color = color_grid,
                          alpha = 0) +
        theme_bw() +
        guides(pattern = "none") +
        xlab(ns[-i][[1]]) +
        ylab(ns[-i][[2]])
      if(g_title){
        figures[[i]] <- figures[[i]] +  ggtitle(ns[i])
      }

      if (all(c(length(theta1Vals) < 8,
                length(theta3Vals) < 8,
                length(theta2Vals) < 8))) {

        figures[[i]] <- figures[[i]] + geom_text(aes(
          label = round(`Parameter Value`,
                        round_res)),
          color = color_values)
        # +
        #   scale_x_continuous(name = ns[-i][[2]],
        #                      breaks = vals[-i][[2]]) +
        #   scale_y_continuous(name = ns[-i][[1]],
        #                      breaks = vals[-i][[1]])
      } else {
        #
        # figures[[i]] <- figures[[i]] +
        #   xlab(ns[-i][[1]]) +
        #   ylab(ns[-i][[2]])
      }

    }
    # add fdr

    return_list <- list(thetas = thetaMat,
                        standard_errors = seMat,
                        p_values = pMat,
                        significance = sigMat,
                        plots = figures)
  }


  #cat(theta1n, 'is significant when ', theta2n, 'is above X and',
  #    theta3n, 'is above y')
  return(return_list)

}






