#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#' 'Without' operator (complement of %in%):
#' @details A binary operator, i.e. x %w/o% y (x and y vectors, or lists) gives
#'    the elements of x that are NOT IN y.
#' @return A vector or list of elements in x that are not in y
#' @export
"%w/o%" <- function(x,y) {x[!x %in% y]} #Bin operator, x NOT IN y (vectors)

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> inputSPSS <<
#______________________________________________________________________________
#' Read an SPSS-format network file into a 'wide-format' data.table
#'
#' This is usually the first function run when creating a network analysis.
#'
#' @param pInput A character string giving the full path to the SPSS file
#'     to be read in. If blank, the user is asked to select an input file
#'     from the file system.
#'
#' @return A tibble in 'wide' format ().
#'    Wide Format' means that each chooser has only 1 row per wave, and the
#'    relationship choices directed to other house members are all contained
#'    in that one row. The data.table is basically an image of the SPSS file
#'    (.sav) that it reads.
#' @export
inputSPSS <- function(pInput = ""){
  #' @importFrom haven read_spss
  #' @import dplyr
  # ____________________
  if(pInput == ""){
    cat("Input data set not given. Please select from file system...", "\n")
    pInput <- tryCatch(
      {
        pInput <- file.choose()
        tbOut <- haven::read_spss(pInput)
        return(tbOut)
      },
      error = function(cond){
        message("File not selected; here is the actual error message:")
        message(cond)
        #Return an empty tbl, i.e. with 1 col & 0 rows; dim=(0,1)
        mttb <- tibble(a = numeric(0))
        return(mttb)
      } # There is no 'finally'
    )
  } else {
    tbOut <- haven::read_spss(pInput)
    return(tbOut)
  }
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >> setAllMissing <<
#______________________________________________________________________________
#' Converts missing values to NA, including 88 or 99 (if in network ID vars)
#'
#' @details This function works for survey and/or network data tables.
#'
#' @param pInTB The input tibble from the output of 'inputSPSS'
#'
#' @return The same tibble as was input, except that NaNs (in any vars)
#'    and 88's or 99's (in network ID variables only, if there are any)
#'    are all replaced with 'NA'
#' @export
  setAllMissing <- function(pInTB){
#' @import dplyr
#'
  pInTB[is.na(pInTB)] <- NA #Fixes the NaNs

  pOutDT <- pInTB
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> wksp.size <<
#______________________________________________________________________________
#' Return the size of the workspace environment currently loaded
#'
#' This is usually the first function run when creating a network analysis.
#'
#' @return A number (size of current wksp in bytes).
#'    Wide Format' means that each chooser has only 1 row per wave, and the
#'    relationship choices directed to other house members are all contained
#'    in that one row. The data.table is basically an image of the SPSS file
#'    (.sav) that it reads.
#' @export
wksp.size <- function() {
  ws <- sum(sapply(ls(envir=globalenv()), function(x)object.size(get(x))))
  class(ws) <- "object_size"
  ws
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >> install.glmmADMB <<
#______________________________________________________________________________
#' Installs glmmADMB from source
#'
#' @description Installs glmmADMB package from source. This is needed
#'   whenever a new version of R is released that requires an update
#'   to the package library (viz. the "X" or "y" in version X.y.z).
#'   (You cannot simply transfer the package using the adm.savePkgLib
#'   and adm.restorePkgLib functions from this package)
#' @details Be sure you have the new version of R installed and are using
#'   it (if this is relevant) before running.
#'
#' @return Nothing. Simply installs the package in the default package lib.
#' @export
install.glmmADMB <- function(){
  install.packages("R2admb")
  install.packages("glmmADMB",
                   repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                           getOption("repos")),
                   type="source")
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >> getRSFit <<
#______________________________________________________________________________
#' Pulls RSiena model results with t and p-vals
#'
#' @description Extracts effect numbers, effect types, effect names,
#'   thetas (estimated parameters) and standard errors from a
#'   sientFit object, then calculates t-statistics and one or two-tailed
#'   p-values for each effect.
#' @details Uses a version of JMLUtils::pValT (pvT) which does not output
#'   anything, just calculates 1 or 2-tailed p-vals.
#' @param fitObj An RSiena 'sienaFit' object (output from siena07)
#' @param pVal Logical: if TRUE (default), calculates the t-ratio and
#'   normal-distribution p-value (1 or 2 tailed depending on the 'tails'
#'   parameter) for each effect.
#' @param tails Numeric: default is 2; enter 1 if 1-tailed pval preferred.
#'
#' @return A tibble with the 7 columns described earlier.
#' @export
getRSFit <- function(fitObj, pVal=TRUE, tails=2){
  if (class(fitObj) != "sienaFit"){
    cat("\nInput object is not class sienaFit")
    stop()
  }
  if(tails==1 | tails==2){
  }else{
    cat("\nWarning: tails not 1 or 2; 2-tailed is assumed")
    tails <- 2
  }
  col1.nbrFx <- 1:length(fitObj$requestedEffects$shortName)
  col2.fxNames <- fitObj$requestedEffects$effectName
  col3.theta <- fitObj$theta
  col4.sErr <- fitObj$se
  col5.tStat <- fitObj$theta/fitObj$se
  col6.pVal <- pvT(fitObj$theta, col4.sErr, tails)
  col7.tails <- rep(tails, length(fitObj$requestedEffects$shortName))
  outTbl <- tibble(Number = col1.nbrFx,
                   Effect = col2.fxNames,
                   Parameter = col3.theta,
                   SE = col4.sErr,
                   tStat = col5.tStat,
                   PVal = col6.pVal,
                   tails = tails)
  return(outTbl)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >> getglmmTMFit <<
#______________________________________________________________________________
#' Pulls glmmTMB model results with t and p-vals
#'
#' @description Extracts effect names, effect types,
#'   betas (estimated parameters) and standard errors from a
#'   glmmTMB object, then calculates t-statistics and one or two-tailed
#'   p-values for each effect.
#' @details Uses a version of JMLUtils::pValT (pvT) which does not output
#'   anything, just calculates 1 or 2-tailed p-vals.
#' @param fitObj A glmmTMB 'glmmTMB' object (output from glmmTMB)
#' @param pVal Logical: if TRUE (default), calculates the t-ratio and
#'   normal-distribution p-value (1 or 2 tailed depending on the 'tails'
#'   parameter) for each effect.
#' @param tails Numeric: default is 2; enter 1 if 1-tailed pval preferred.
#'
#' @return A tibble with the 7 columns described earlier.
#' @export
getglmmTMBFit <- function(fitObj, pVal=TRUE, tails=2){
  #xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  cat("This function not yet implemented")
  stop()
  #xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

  if (class(fitObj) != "glmmTMB"){
    cat("\nERROR: Input object is not class glmmTMB")
    stop()
  }
  if(tails==1 | tails==2){
  }else{
    cat("\nWarning: tails not 1 or 2; 2-tailed is assumed")
    tails <- 2
  }
  col1.fxNames <- fitObj$requestedEffects$effectName
  col3.theta <- fitObj$theta
  col4.sErr <- fitObj$se
  col5.tStat <- fitObj$theta/fitObj$se
  col6.pVal <- pvT(fitObj$theta, col4.sErr, tails)
  col7.tails <- rep(tails, length(fitObj$requestedEffects$shortName))
  outTbl <- tibble(Number = col1.nbrFx,
                   Effect = col2.fxNames,
                   Parameter = col3.theta,
                   SE = col4.sErr,
                   tStat = col5.tStat,
                   PVal = col6.pVal,
                   tails = tails)
  return(outTbl)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >> pvT <<
#______________________________________________________________________________
#' Calculates P-values
#'
#' @description Calculater 1 or 2-tailed pvalues.
#' @details A version of JMLUtils::pValT (pvT) which does not output
#'   anything, just calculates 1 or 2-tailed p-vals.
#' @param pDiff Number: Effect size
#' @param sE Number: standard error
#' @param pTails Number: number of tails (1 or 2)
#'
#' @return The p-value (number or vector)
#' @export
pvT <- function(pDiff, sE, pTails){
  tRat<-pDiff/sE
  # If 1-tailed:
  if (pTails == 1)
  {
  #p-Value
  pVal <- (1-pnorm(abs(tRat)))
  } else{
    # If 2-tailed:
    # p-value
    pVal <- (1-pnorm(abs(tRat)))*2
  }

  # Output
  return(pVal)
}



