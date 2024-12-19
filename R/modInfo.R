
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> bEfxList <<
#______________________________________________________________________________
#' Creates a table of the terms in a regression output object (of class lme or
#' glmmTMB)
#'
#' @param resultObj A regression output object of class lme or glmmTMB
#'
#' @return a tbl_df with two columns: the term (effect) number, and the term
#' name (description)
#' @note This function is particularly useful when doing moderation analysis
#' (e.g. with the function JNSiena5.3way), as the latter refers to terms in
#' the model by their number (in order, as used for vectors of lists of
#' coefficients, rows and cols of the vcov matrix, etc.)
#' @export
EfxList <- function(resultObj){
  #This version works with glmmTMB output objects
  if (class(resultObj) == 'glmmTMB'){
    #This is needed because the names for each part of the model are
    #included twice, for some reason. This might be a problem later on
    #but for now it seems to work OK.
    nCondCoef <- as.numeric(unlist(attributes(vcov(resultObj)$cond))[1])
    condNames <- unlist(attributes(vcov(resultObj)$cond))[3:(nCondCoef+2)]

    nZiCoef <-as.numeric(unlist(attributes(vcov(resultObj)$zi))[1])
    ziNames <- unlist(attributes(vcov(resultObj)$zi))[3:(nZiCoef+2)]

    tblOfEfcts <- tibble(num = 1:(nCondCoef+nZiCoef),
                         names = c(condNames, ziNames))
  } else if(class(resultObj) == 'lme') {
    numCoefs <- length(unlist(attributes(resultObj$coefficients$fixed)))
    tblOfEfcts <- tibble(num = 1:numCoefs,
                         names = unlist(attributes(resultObj$coefficients$fixed)))
  } else {
    cat("\nERROR: input object must be class lme or class glmmTMB")
    stop()
  }
  return(tblOfEfcts)
}
