% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utilities_Private.R
\name{prior_distributions}
\alias{prior_distributions}
\title{Numerical prior distribution}
\usage{
prior_distributions(distribution, param1, param2)
}
\arguments{
\item{distribution}{string, prior distribution selected}

\item{param1}{real, first parameter for the distribution}

\item{param2}{real, second parameter for the distribution}
}
\value{
vector, 1000 random realizations of the distribution
}
\description{
Numerical prior distribution
}
\keyword{internal}
