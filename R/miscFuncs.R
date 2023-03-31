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
#  >> find_var <<
#______________________________________________________________________________
#' Finds a variable in a data.frame by matching a pattern, etc.
#'
#' @description This functions finds variables in a data frame, which variable
#'  names or variable (and value) label attribute match a specific pattern.
#'  Regular expression for the pattern is supported. Copied from package
#'  >sjmisc<, which has a lot of other useful functions of this general
#'  type.
#' @details This function searches for pattern in data's column names and
#'  - for labelled data - in all variable and value labels of data's variables
#'  (see sjmisc::get_label for details on variable labels and labelled data).
#'  Regular expressions are supported as well, by simply using
#'  pattern = stringr::regex(...) or regex = TRUE.
#' @param data A dataframe with the variables of interest
#' @param pattern A string with the pattern to look for, or a regular expression
#' @param ignore.case Logical, TRUE => match irrespective of case.
#' @param search A string indicating what to look for. Can be name_label,
#'  name_value, label_value, name, or label. Default is name_label
#' @param out A string requesting type of output; may be a table ("table"),
#'  dataframe ("df"), or index ("index").
#' @param fuzzy Logical, if TRUE use fuzzy grep.
#' @param regex Logical, if TRUE pattern is a regex.
#'
#' @return A table, dataframe or index (see @out).
#' @export
find_var <- function (data, pattern, ignore.case = TRUE, search = c("name_label",
                                                        "name_value", "label_value", "name", "label", "value", "all"),
          out = c("table", "df", "index"), fuzzy = FALSE, regex = FALSE)
{
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  search <- match.arg(search)
  out <- match.arg(out)
  if (regex)
    class(pattern) <- c("regex", class(pattern))
  pos1 <- pos2 <- pos3 <- c()
  fixed <- !inherits(pattern, "regex")
  if (.is_true(fixed))
    ignore.case <- FALSE
  if (search %in% c("name", "name_label", "name_value", "all")) {
    pos1 <- which(grepl(pattern = pattern, x = colnames(data),
                        ignore.case = ignore.case, fixed = fixed))
    if (sjmisc::is_empty(pos1) && fuzzy && !inherits(pattern,
                                                     "regex")) {
      pos1 <- fuzzy_grep(x = colnames(data), pattern = pattern)
    }
  }
  if (search %in% c("label", "name_label", "label_value", "all")) {
    labels <- sjlabelled::get_label(data)
    pos2 <- which(grepl(pattern, x = labels, ignore.case = ignore.case,
                        fixed = fixed))
    if (sjmisc::is_empty(pos2) && fuzzy && !inherits(pattern,
                                                     "regex")) {
      pos2 <- fuzzy_grep(x = labels, pattern = pattern)
    }
  }
  if (search %in% c("value", "name_value", "label_value", "all")) {
    labels <- sjlabelled::get_labels(data, attr.only = FALSE)
    pos3 <- which(sapply(labels, function(.x) any(grepl(pattern,
                                                        x = .x, ignore.case = ignore.case, fixed = fixed)),
                         simplify = TRUE))
    if (sjmisc::is_empty(pos3) && fuzzy && !inherits(pattern,
                                                     "regex")) {
      pos3 <- which(sapply(labels, function(.x) {
        p <- fuzzy_grep(x = .x, pattern = pattern)
        !sjmisc::is_empty(p[1])
      }, simplify = TRUE))
    }
  }
  pos <- unique(c(pos1, pos2, pos3))
  pos <- pos[which(pos != -1)]
  if (out == "df") {
    return(data[, pos, drop = FALSE])
  }
  if (out == "table") {
    return(data_frame(col.nr = pos, var.name = colnames(data)[pos],
                      var.label = sjlabelled::get_label(data[, pos, drop = FALSE],
                                                        def.value = colnames(data)[pos])))
  }
  names(pos) <- colnames(data)[pos]
  pos
}


