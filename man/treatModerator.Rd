% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tripleInteraction.R
\name{treatModerator}
\alias{treatModerator}
\title{Treat moderator name with mean value}
\usage{
treatModerator(
  ind,
  moderatorNames,
  data = NULL,
  rangemode = 1,
  probs = c(0.16, 0.5, 0.84)
)
}
\arguments{
\item{ind}{An equation}

\item{moderatorNames}{character vectors}

\item{data}{A data.frame}

\item{rangemode}{range mode}

\item{probs}{numeric vector of probabilities with values in [0,1]}
}
\description{
Treat moderator name with mean value
}
\examples{
ind="(a1+a4*sex+a5*age)*(b1)"
moderatorNames=c("age","sex")
treatModerator(ind,moderatorNames)
}
