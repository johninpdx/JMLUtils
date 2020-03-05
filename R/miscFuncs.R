#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#' 'Without' operator (complement of %in%):
#' @details A binary operator, i.e. x %w/o% y (x and y vectors, or lists) gives
#'    the elements of x that are NOT IN y.
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
