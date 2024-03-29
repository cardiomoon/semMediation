% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeEquation.R
\name{addCovarEquation}
\alias{addCovarEquation}
\title{Add covariates to equation}
\usage{
addCovarEquation(equation, covar = list(), prefix = "h")
}
\arguments{
\item{equation}{The equation}

\item{covar}{A list}

\item{prefix}{prefix}
}
\description{
Add covariates to equation
}
\examples{
equation="M ~ X*W"
covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M","Y"),"Y","Y"))
addCovarEquation(equation,covar=covar,prefix=NULL)
}
