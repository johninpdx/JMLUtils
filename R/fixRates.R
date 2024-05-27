
#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> fixRates <<
#______________________________________________________________________________
#' Fix all or selected rate parameters in an RSIena effects object
#'
#' This is useful for speeding up estimation once rate parameter values have
#' been found and are stable across different runs.
#'
#' @param efObj An RSiena 'effects' object.
#' @param efxNums A vector of effect numbers of the rates to be fixed. If
#'   this vector consists only of a 1 element vector with 0 in it, or is
#'   omitted (in which case, c(0) is the default), all rate parameters
#'   in the effects object are fixed, meaning they are not estimated,
#'   but remain at their starting values.
#'
#' @return The modified effects object, with specified (or all ) rate
#'   parameters fixed
#' @export
  # ____________________
fixRates <- function(efObj, efxNums=c(0)){
  #' @import dplyr
  #If no efxNums are specified, fix ALL rate parameters
  msg <- "No effect #'s given; all rate parms will be fixed. OK?"
  if(efxNums==c(0)){
    # answ <- askYesNo(msg, default = TRUE,
    #          prompts = getOption("askYesNo",
    #                              gettext(c("Yes", "No", "Cancel"))), ...)
    # if(!answ | is.na(answ)){
    #   cat("/nAborting ...")
    #   stop()
    } #Otherwise just continue
    effObjTbl <- efObj %>% as.tibble() %>%
      filter(shortName == "Rate") %>%
      select(name, effectName, effectNumber)
    efxNums <- effObjTbl$effectNumber

  #Set the 11th column (fix) to TRUE for all selected effects
  for(i in efxNums){
   efObj[i,11] <- TRUE
  }
  return(efObj)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> unfixRates <<
#______________________________________________________________________________
#' Frees rate parameters (e.g. that were fixed by 'fixRates')
#'
#' @param efObj An RSiena 'effects' object.
#' @param efxNums A vector of effect numbers of the rates to be fixed. If
#'   this vector consists only of a 1 element vector with 0 in it, or is
#'   omitted (in which case, c(0) is the default), all rate parameters
#'   in the effects object are fixed, meaning they are not estimated,
#'   but remain at their starting values.
#'
#' @return The modified effects object, with specified (or all ) rate
#'   parameters freed.
#' @export
unfixRates <- function(efObj, efxNums=c(0)){
  #If no efxNums are specified, fix ALL rate parameters
  msg <- "No effect #'s given; all rate parms will be freed. OK?"
  if(efxNums==c(0)){
    # answ <- askYesNo(msg, default = TRUE,
    #          prompts = getOption("askYesNo",
    #                              gettext(c("Yes", "No", "Cancel"))), ...)
    # if(!answ | is.na(answ)){
    #   cat("/nAborting ...")
    #   stop()
  } #Otherwise just continue
  effObjTbl <- efObj %>% as.tibble() %>%
    filter(shortName == "Rate") %>%
    select(name, effectName, effectNumber)
  efxNums <- effObjTbl$effectNumber

  #Set the 11th column (fix) to FALSE for all selected effects
  for(i in efxNums){
    efObj[i,11] <- FALSE
  }
  return(efObj)
}

