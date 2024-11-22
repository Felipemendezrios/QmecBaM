





#' Estimation and simulation tidal river
#'
#' Parameter estimation for the 1D shallow water equation and simulation of
#' discharge time series with quantified uncertainties
#'
#' @param CalibrationData data frame, calibration data with a minimal information as shown in `Saint_Laurent_F2` dataset
#' @param Be.object object, real positive value indicating the cross-sectional width at the surface for hypothetical channel
#' @param be.object object, real positive value indicating the elevation of the riverbed relative to a reference system (sea mean level)
#' @param delta.object object,real value indicating leveling error
#' @param ne.object object, real positive value indicating the tunable Manning friction coefficient
#' @param Q0.object object, real value indicating initial discharge for calibration
#' @param dx.object object, real positive value indicating distance between two gauge stations
#' @param CD1 real value, chart-datum located upstream
#' @param CD2 real value, chart-datum located downstream
#' @param dt_model integer, positive value indicating time discretization in second of the input data
#' @param temp.folder directory, temporary directory to write computations
#' @param nCycles integer, number of MCMC adaptation cycles. Total number of simulations equal to 100*nCycles
#' @param burn real between 0 (included) and 1 (excluded), MCMC burning factor
#' @param nSlim integer, MCMC slim step
#'
#' @return list with the following components:
#' @export
#' @importFrom RBaM dataset parameter
#'
#' @examples
#'
#' priors=data.frame(Be=c(1500,'LogNormal',log(1500),0.1),
#'                   be=c(-14.6915,'Gaussian',-14.6915,1),
#'                   delta=c(0,'Gaussian',0,0.5),
#'                   ne=c(0.023,'LogNormal',log(0.023),0.2),
#'                   Q0=c(12500,'Gaussian',12500,10000),
#'                   dx=c(38000,'Gaussian',38000,1000))
#'
#'
#' Be=RBaM::parameter(name='Be',
#'                    init=as.numeric(priors$Be[1]),
#'                    prior.dist=priors$Be[2],
#'                    prior.par=as.numeric(priors$Be[c(3,4)]))
#'
#' be=RBaM::parameter(name='be',
#'                    init=as.numeric(priors$be[1]),
#'                    prior.dist=priors$be[2],
#'                    prior.par=as.numeric(priors$be[c(3,4)]))
#'
#' delta=RBaM::parameter(name='delta',
#'                       init=as.numeric(priors$delta[1]),
#'                       prior.dist=priors$delta[2],
#'                       prior.par=as.numeric(priors$delta[c(3,4)]))
#'
#' ne=RBaM::parameter(name='ne',
#'                    init=as.numeric(priors$ne[1]),
#'                    prior.dist=priors$ne[2],
#'                    prior.par=as.numeric(priors$ne[c(3,4)]))
#'
#' Q0=RBaM::parameter(name='Q0',
#'                    init=as.numeric(priors$Q0[1]),
#'                    prior.dist=priors$Q0[2],
#'                    prior.par=as.numeric(priors$Q0[c(3,4)]))
#'
#' dx=RBaM::parameter(name='dx',
#'                    init=as.numeric(priors$dx[1]),
#'                    prior.dist=priors$dx[2],
#'                    prior.par=as.numeric(priors$dx[c(3,4)]))
#'
#' d1= -1.379
#' d2= -1.958
#' dt_model= 60
#'
#' results_calibration=Estimation_Qmec(CalibrationData=Saint_Laurent_F2,
#'                                     Be.object=Be,
#'                                     be.object=be,
#'                                     delta.object=delta,
#'                                     ne.object=ne,
#'                                     Q0.object=Q0,
#'                                     dx.object=dx,
#'                                     CD1=d1,
#'                                     CD2=d2,
#'                                     nCycles = 10,
#'                                     burn=0.5,
#'                                     dt_model=dt_model)
#'
#'Plot_prior_posterior(DF_prior_posterior=results_calibration$prior_vs_posterior)
Estimation_Qmec <- function(CalibrationData,
                            Be.object,
                            be.object,
                            delta.object,
                            ne.object,
                            Q0.object,
                            dx.object,
                            CD1,
                            CD2,
                            dt_model,
                            nCycles = 100,
                            burn=0.5,
                            nSlim=max(nCycles/10, 1),
                            temp.folder = file.path(tempdir(), "BaM")){

  D=RBaM::dataset(X=CalibrationData[c('h1','h2')],
                  Y=CalibrationData[c('Q')],
                  Yu=CalibrationData[c('u_Q')],
                  data.dir = temp.folder)

  # Prior information
  c=RBaM::parameter(name='c',init=4/3,prior.dist='FIX')
  g=RBaM::parameter(name='g',init=9.81,prior.dist='FIX')
  dt=RBaM::parameter(name='dt',init=dt_model,prior.dist='FIX')
  d1=RBaM::parameter(name='d1',init=CD1,prior.dist='FIX')
  d2=RBaM::parameter(name='d2',init=CD2,prior.dist='FIX')

  param_list <-list(Be.object, # list of model parameters
                    be.object,
                    delta.object,
                    ne.object,
                    d1,
                    d2,
                    c,
                    g,
                    Q0.object,
                    dx.object,
                    dt)
  # Model
  M=RBaM::model(ID='SFDTidal_Qmec2',
                nX=2,nY=1, # number of input/output variables
                par=param_list)

  # Remnant error
  remnant=RBaM::remnantErrorModel(funk='Constant',
                                  par=list(RBaM::parameter(name='gamma1',
                                                           init=1000,
                                                           prior.dist='Uniform',
                                                           prior.par=c(0,10000))))

  # Set up configuration files : careful, same quantity of remnants configuration files as number of observations
  mcmc_temp=RBaM::mcmcOptions(nCycles=nCycles)
  cook_temp=RBaM::mcmcCooking(burn=burn,nSlim=nSlim)

  # Run BaM executable
  RBaM::BaM(mod=M,
            data=D,
            workspace=temp.folder,
            doCalib=TRUE,
            doPred=FALSE,
            mcmc=mcmc_temp,
            cook = cook_temp,
            remnant = list(remnant))

  # Analyse results
  # Read 'cooked' MCMC file in the workspace
  MCMC=RBaM::readMCMC(file.path(temp.folder,'Results_Cooking.txt'))

  # Prior and posterior densities
  priors_realization = c()
  j=1
  for(i in 1:length(param_list)){
    if(param_list[[i]]$prior$dist!='FIX'){

      priors_realization[[j]]=prior_distributions(distribution=param_list[[i]]$prior$dist,
                                                  param1=param_list[[i]]$prior$par[1],
                                                  param2=param_list[[i]]$prior$par[2])

      names(priors_realization)[[j]] <- param_list[[i]]$name
      j=j+1
    }
  }

  prior.density <- as.data.frame(priors_realization)

  prior_vs_posterior <- c()
  for(i in 1:ncol(prior.density)){
    prior.par.DF <- data.frame(value=prior.density[,i],
                               Distributions=rep('Prior',nrow(prior.density)),
                               id=rep(colnames(prior.density)[i]))
    posterior.par.DF <- data.frame(value=MCMC[,i],
                                   Distributions=rep('Posterior',nrow(MCMC)),
                                   id=rep(colnames(MCMC)[i]))
    prior_vs_posterior <- rbind(prior_vs_posterior,
                                prior.par.DF,
                                posterior.par.DF)
  }

  return(list(MCMC=MCMC,
              prior_vs_posterior=prior_vs_posterior))

}

