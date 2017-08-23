# RAdministrationFuncs.R
#' Save existing package library to a temporary file.
#' @details When upgrading your R version, run this first
#'    to save your existing package library. It can be
#'    restored after the upgrade.
#' @note It is not necessary to do this for minor upgrades, i.e. the 'x'
#'    in '3.4.x'; you only need to do so if your version changes in one
#'    of the first two positions, i.e. the 'x' in x.4.2, or 3.x.1...
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
  if(machineName == "RSTAT"){
    folderLoc <- "C:/Users/jlight.ORI-EUG/Desktop/RPkgUpdate/"
    return(folderLoc)
  }
  cat("Computer name not found; must add to JMLUtils code!\n")
  return(" ")
}

#' Changes library location for package installation
#'
#' @details Wrapper for the R function .libPaths
#' @return A message with the new library location to be used for installation
#' @export
changePkgLibLocation <- function(){
  currentLoc <- .libPaths()[1]
  cat("Current default package location:\n", "  ", currentLoc, "\n")
  cat("If you want to change it, select a different folder...\n")
  newLoc <- choose.dir(default = currentLoc,
                       caption = "...or CANCEL, to not change anything:")
  if(is.na(newLoc)){
    newLoc <- currentLoc
  }
  # Fix slashes to be consistent with .libPaths format
  newLoc <- gsub("\\", "/", newLoc, fixed = TRUE)

  if(currentLoc == newLoc){
    cat ("\n Default package location IS UNCHANGED:\n", currentLoc)
  }
  if(currentLoc != newLoc){
    .libPaths(newLoc)
    cat("\n Default package location CHANGED to:\n", newLoc)
  }
}
