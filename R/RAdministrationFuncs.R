# RAdministrationFunc.R
#' Save existing package library to a temporary file.
#' @details When upgrading your R version, run this first
#'    to save your existing package library. It can be
#'    restored after the upgrade.
#' @return Nothing; just a message indication completion
#'
#' @export
adm.savePkgLib <- function(){
  dir.create("C:/Users/jlight/Downloads/RPkgUpdate")
  # If an old update exists,
  if(file.exists("C:/Users/jlight/Downloads/RPkgUpdate/Rpackages")){
    cat("An old package lib is here already; renaming it to B4<today>...\n")
    file.rename("C:/Users/jlight/Downloads/RPkgUpdate/Rpackages",
                paste("C:/Users/jlight/Downloads/RPkgUpdate/Rpackages.B4.",
                      as.character(Sys.Date()), sep=""))
  }
  packages <- installed.packages()[,"Package"]
  save(packages,
       file="C:/Users/jlight/Downloads/RPkgUpdate/Rpackages")
  if(file.exists("C:/Users/jlight/Downloads/RPkgUpdate/Rpackages")){
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
  load("C:/Users/jlight/Downloads/RPkgUpdate/Rpackages")
  for (p in setdiff(packages, installed.packages()[,"Package"]))
    install.packages(p)
  # An option should be added to delete the temporary package library
  # if the above goes successfully.
}
