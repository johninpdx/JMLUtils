% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RAdministrationFuncs.R
\name{adm.restorePkgLib}
\alias{adm.restorePkgLib}
\title{Reload packages after upgrade}
\usage{
adm.restorePkgLib()
}
\value{
Nothing; just a message indicating completion
}
\description{
If you saved your package library before upgrading R
   using 'savePkgLib', you then do the upgrade, and restore
   the package library with this function.
}
\details{
>> DO NOT RUN THIS FUNCTION FROM RSTUDIO<. Run it from the
 R GUI instead.
}
