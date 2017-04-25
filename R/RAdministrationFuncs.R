# RAdministrationFunc.R
#' Save existing package library to a temporary file.
#' @details When upgrading your R version, run this first
#'    to save your existing package library. It can be
#'    restored after the upgrade.
#' @return Nothing; just a message indication completion
#'
#' @export
adm.savePkgLib <- function(){
  savedPkgs <- "C:/Users/jlight/AppData/RPkgUpdate/Rpackages"
  dir.create("C:/Users/jlight/AppData/RPkgUpdate")
  # If an old update exists,
  if(file.exists(savedPkgs)){
    cat("An old package lib is here already; renaming it to B4<today>...\n")
    file.rename(savedPkgs,
                paste(savedPkgs, ".B4.",
                      as.character(Sys.Date()), sep=""))
  }
  packages <- installed.packages()[,"Package"]
  save(packages,
       file=savedPkgs)
  if(file.exists(savedPkgs)){
    cat("Package lib successfully backed up\n")
  }
}

#' Reload packages after upgrade
#' @details If you saved your package library before upgrading R
#'    using 'savePkgLib', you then do the upgrade, and restore
#'    the package library with this function.
#' @return Nothing; just a message indicating completion
#'
#' @export
adm.restorePkgLib <- function(){
  savedPkgs <- "C:/Users/jlight/AppData/RPkgUpdate/Rpackages"
  savedPkgsDir <- "C:/Users/jlight/AppData/RPkgUpdate"

  if(file.exists(savedPkgs)){
    load(savedPkgs)
    for (p in setdiff(packages, installed.packages()[,"Package"]))
      install.packages(p)
  } else {
    cat("*Rpackages* does not exist in\n", savedPkgsDir,
        "\n Restore could not be completed...")
  }

  # An option might be added to delete the temporary package library
  # if the above goes successfully.
}
