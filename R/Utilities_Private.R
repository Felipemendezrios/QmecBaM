#' Numerical prior distribution
#'
#' @param distribution string, prior distribution selected
#' @param param1 real, first parameter for the distribution
#' @param param2 real, second parameter for the distribution
#'
#' @return vector, 1000 random realizations of the distribution
#' @importFrom stats rnorm rlnorm runif
#' @keywords internal
prior_distributions<-function(distribution,
                              param1,
                              param2){

  if(distribution!='Gaussian' & distribution!='LogNormal' &
     distribution!='Uniform'& distribution!='FIX')
     stop('Prior distribution is not supported. \nPlease ensure that distribution is Gaussian, \n LogNormal, Uniform or FIX')

  if(distribution=='Gaussian'){
    prior_realization=stats::rnorm(1000,
                            mean = as.numeric(param1),
                            sd = as.numeric(param2))

  }else if(distribution=='LogNormal'){
    prior_realization=stats::rlnorm(1000,
                             meanlog = as.numeric(param1),
                             sdlog = as.numeric(param2))

  }else if(distribution=='Uniform'){
    if(param1<=param2)stop('To use Uniform distribution, \nfirst value introduce must be lower than second value in the vector')
    prior_realization=stats::runif(1000,
                                   min = as.numeric(param1),
                                   max = as.numeric(param2))
  }
}
