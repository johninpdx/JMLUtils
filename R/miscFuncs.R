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




