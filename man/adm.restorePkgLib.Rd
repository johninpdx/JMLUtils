% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RAdministrationFuncs.R
\name{adm.restorePkgLib}
\alias{adm.restorePkgLib}
\title{Reload packages after upgrade}
\usage{
adm.restorePkgLib()
}
\value{
Just a message indicating completion
}
\description{
If you saved your package library before upgrading R
   using 'savePkgLib', you then do the upgrade, and restore
   the package library with this function.
}
\details{
Make sure you have installed the new R. Then open an R GUI
 (>> DO NOT RUN THIS FUNCTION FROM RSTUDIO<<), and start up with the
 new version of R.
}
