#' Plot prior vs posterior distribution
#'
#' Plot leading to compare conflict or problems on the calibration
#'
#' @param MCMC_results data frame, MCMC cooking results
#' @param DF_prior_posterior data frame, results of `Estimation` function givin prior vs posterior information
#'
#' @return ggplot, list with the following information :
#' \enumerate{
#'    \item histogram of prior vs posterior distribution
#' }
#' @import ggplot2
#' @export
Plot_prior_posterior <- function(MCMC_results,DF_prior_posterior){

  plot_conflicts=ggplot(DF_prior_posterior,aes(x = value ,fill= Distributions))+
    geom_density(alpha=0.4,show.legend = TRUE)+
    facet_wrap(~ id,scales="free",ncol=3)+
    theme_bw()+
    theme(legend.position="top",legend.key.size = unit(0.7, 'cm'))+
    scale_y_continuous(name = "Probability density function")

  return(list(plot_conflicts=plot_conflicts))
}
