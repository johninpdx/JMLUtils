# RAdministrationFunc.R
#' Save existing package library to a temporary file.
#' @details When upgrading your R version, run this first
#'    to save your existing package library. It can be
#'    restored after the upgrade.
#' @return Nothing; just a message indication completion
#'
#' @export
adm.savePkgLib <- function(){
  savedPkgs <- paste(folderLocation(), "Rpackages", sep="")

  dir.create(folderLocation())
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
  savedPkgs <- paste(folderLocation(), "Rpackages", sep="")
  savedPkgsDir <- folderLocation()

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

#' Determines the folder loc on this machine for reading/writing
#'
#' @details This function is used to determine where the R package
#'    backup should be written, and where to retrieve it from.
#' @return A string describing a folder location, specific to the
#'    current machine.
#' @export
folderLocation <- function(){
  machineName <- Sys.info()['nodename']
  if(machineName == "JLIGHTWIN7") {
    folderLoc <- "L:/Software/RPkgUpdate/"
    return(folderLoc)
  }
  if(machineName == "JLIGHTWIN7HOME"){
    folderLoc <- "C:/Users/jlight/AppData/RPkgUpdate/"
    return(folderLoc)
  }
  cat("Computer name not found; must add to JMLUtils code!\n")
  return(" ")
}
