% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plots.R
\name{plot_shallow_water}
\alias{plot_shallow_water}
\title{Plot 1D Shallow Water (1D SW) components}
\usage{
plot_shallow_water(pressure_SW, friction_SW, advection_SW, MaxPostOnly = FALSE)
}
\arguments{
\item{pressure_SW}{list or data frame, pressure gradient obtained by using \code{Prediction_Q_Qmec} function}

\item{friction_SW}{list or data frame, friction obtained by using \code{Prediction_Q_Qmec} function}

\item{advection_SW}{list or data frame, advection obtained by using \code{Prediction_Q_Qmec} function}

\item{MaxPostOnly}{logical, if \code{TRUE}, only plot of MaxPost estimation of all components of the 1D Shallow Water}
}
\value{
plot_SW: list of ggplots, plots of 1D SW components over time with uncertainties and their sum
}
\description{
Plot 1D Shallow Water (1D SW) components
}
