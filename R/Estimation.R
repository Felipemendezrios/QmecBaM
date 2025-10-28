#' Estimation and simulation tidal river
#'
#' Parameter estimation for the 1D shallow water equation and simulation of
#' discharge time series with quantified uncertainties
#'
#' @param CalibrationData data frame, calibration data with a minimal information as shown in `Saint_Laurent_F2` dataset
#' @param Be_object object, real positive value indicating the cross-sectional width at the surface for hypothetical channel
#' @param be_object object, real positive value indicating the elevation of the riverbed relative to a reference system (sea mean level)
#' @param delta_object object,real value indicating leveling error
#' @param ne_object object, real positive value indicating the tunable Manning friction coefficient
#' @param Q0_object object, real value indicating initial discharge for calibration
#' @param dx_object object, real positive value indicating distance between two gauge stations
#' @param CD1 real value, chart-datum located upstream
#' @param CD2 real value, chart-datum located downstream
#' @param dt_model integer, positive value indicating time discretization in second of the input data
#' @param temp_folder directory, temporary directory to write computations
#' @param nCycles integer, number of MCMC adaptation cycles. Total number of simulations equal to 100*nCycles
#' @param burn real between 0 (included) and 1 (excluded), MCMC burning factor
#' @param nSlim integer, MCMC slim step
#' @param Nb_column_time_CalData integer, column number of calibration data corresponding to temporal grid to write in temporal or specified folder
#'
#' @return list with the following components:
#' \enumerate{
#'    \item MCMC: data frame, MCMC results during calibration process
#'    \item prior_vs_posterior: data frame, summarize the random realization of all parameters
#'    \itemize{
#'        \item value: real value, random value during the random realization of the parameter
#'        \item Distributions: string, either "Prior" or "Posterior" string to indicate the separation of the two groups
#'        \item id: string, parameter's names
#'    }
#'    \item Model_object: object, model object obtained from `RBaM` package
#' }
#' @export
#' @importFrom RBaM dataset parameter
#'
#' @examples
#'
#' priors <- data.frame(
#'   Be = c(1500, "LogNormal", log(1500), 0.1),
#'   be = c(-14.6915, "Gaussian", -14.6915, 1),
#'   delta = c(0, "Gaussian", 0, 0.5),
#'   ne = c(0.023, "LogNormal", log(0.023), 0.2),
#'   Q0 = c(12500, "Gaussian", 12500, 10000),
#'   dx = c(38000, "Gaussian", 38000, 1000)
#' )
#'
#' Be <- RBaM::parameter(
#'   name = "Be",
#'   init = as.numeric(priors$Be[1]),
#'   prior.dist = priors$Be[2],
#'   prior.par = as.numeric(priors$Be[c(3, 4)])
#' )
#'
#' be <- RBaM::parameter(
#'   name = "be",
#'   init = as.numeric(priors$be[1]),
#'   prior.dist = priors$be[2],
#'   prior.par = as.numeric(priors$be[c(3, 4)])
#' )
#'
#' delta <- RBaM::parameter(
#'   name = "delta",
#'   init = as.numeric(priors$delta[1]),
#'   prior.dist = priors$delta[2],
#'   prior.par = as.numeric(priors$delta[c(3, 4)])
#' )
#'
#' ne <- RBaM::parameter(
#'   name = "ne",
#'   init = as.numeric(priors$ne[1]),
#'   prior.dist = priors$ne[2],
#'   prior.par = as.numeric(priors$ne[c(3, 4)])
#' )
#'
#' Q0 <- RBaM::parameter(
#'   name = "Q0",
#'   init = as.numeric(priors$Q0[1]),
#'   prior.dist = priors$Q0[2],
#'   prior.par = as.numeric(priors$Q0[c(3, 4)])
#' )
#'
#' dx <- RBaM::parameter(
#'   name = "dx",
#'   init = as.numeric(priors$dx[1]),
#'   prior.dist = priors$dx[2],
#'   prior.par = as.numeric(priors$dx[c(3, 4)])
#' )
#'
#' d1 <- -1.379
#' d2 <- -1.958
#' dt_model <- 60
#'
#' results_calibration <- Estimation_Qmec(
#'   CalibrationData = Saint_Laurent_F2,
#'   Be_object = Be,
#'   be_object = be,
#'   delta_object = delta,
#'   ne_object = ne,
#'   Q0_object = Q0,
#'   dx_object = dx,
#'   CD1 = d1,
#'   CD2 = d2,
#'   nCycles = 100,
#'   burn = 0.5,
#'   dt_model = dt_model
#' )
#'
#' Plot_prior_posterior(DF_prior_posterior = results_calibration$prior_vs_posterior)
#'
#' simulation_QMEC <- Prediction_Q_Qmec(
#'   CalibrationData = Saint_Laurent_F2,
#'   Model_object = results_calibration$Model_object
#' )
#'
#' plot_Q_sim_Qmec(
#'   Q_observed = simulation_QMEC$Data_obs_plot,
#'   Q_simulated = simulation_QMEC$Qsim
#' )
#'
#' plot_shallow_water(
#'   pressure_SW = simulation_QMEC$pressure,
#'   friction_SW = simulation_QMEC$friction,
#'   advection_SW = simulation_QMEC$advection
#' )
Estimation_Qmec <- function(CalibrationData,
                            Be_object,
                            be_object,
                            delta_object,
                            ne_object,
                            Q0_object,
                            dx_object,
                            CD1,
                            CD2,
                            dt_model,
                            nCycles = 100,
                            burn = 0.5,
                            nSlim = max(nCycles / 10, 1),
                            temp_folder = file.path(tempdir(), "BaM"),
                            Nb_column_time_CalData = 7) {
  set.seed(2024)
  CalData_object <- RBaM::dataset(
    X = CalibrationData[c("h1", "h2")],
    Y = CalibrationData[c("Q")],
    Yu = CalibrationData[c("u_Q")],
    data.dir = temp_folder
  )

  # Prior information
  c <- RBaM::parameter(name = "c", init = 4 / 3, prior.dist = "FIX")
  g <- RBaM::parameter(name = "g", init = 9.81, prior.dist = "FIX")
  dt <- RBaM::parameter(name = "dt", init = dt_model, prior.dist = "FIX")
  d1 <- RBaM::parameter(name = "d1", init = CD1, prior.dist = "FIX")
  d2 <- RBaM::parameter(name = "d2", init = CD2, prior.dist = "FIX")

  param_list <- list(
    Be_object, # list of model parameters
    be_object,
    delta_object,
    ne_object,
    d1,
    d2,
    c,
    g,
    Q0_object,
    dx_object,
    dt
  )
  # Model
  Model_object <- RBaM::model(
    ID = "SFDTidal_Qmec2",
    nX = 2, nY = 1, # number of input/output variables
    par = param_list
  )

  # Remnant error
  remnant <- RBaM::remnantErrorModel(
    funk = "Constant",
    par = list(RBaM::parameter(
      name = "gamma1",
      init = 1000,
      prior.dist = "Uniform",
      prior.par = c(0, 10000)
    ))
  )

  # Set up configuration files : careful, same quantity of remnants configuration files as number of observations
  mcmc_temp <- RBaM::mcmcOptions(nCycles = nCycles)
  cook_temp <- RBaM::mcmcCooking(burn = burn, nSlim = nSlim)

  # Run BaM executable
  RBaM::BaM(
    mod = Model_object,
    data = CalData_object,
    workspace = temp_folder,
    doCalib = TRUE,
    doPred = FALSE,
    mcmc = mcmc_temp,
    cook = cook_temp,
    remnant = list(remnant),
    summary = RBaM::mcmcSummary(
      fname = "Config_Summary.txt",
      result.fname = "Results_Summary.txt",
      DIC.fname = "Results_DIC.txt",
      xtendedMCMC.fname = "Results_extended.txt"
    )
  )

  # Export temporal grid in temporal folder
  write.table(CalibrationData[, Nb_column_time_CalData],
    file = file.path(temp_folder, "temporal_grid.txt"),
    col.names = TRUE
  )

  # Analyse results
  # Read 'cooked' MCMC file in the workspace
  MCMC <- RBaM::readMCMC(file.path(temp_folder, "Results_Cooking.txt"))

  # Prior and posterior densities
  priors_realization <- c()
  j <- 1
  for (i in 1:length(param_list)) {
    if (param_list[[i]]$prior$dist != "FIX") {
      priors_realization[[j]] <- prior_distributions(
        distribution = param_list[[i]]$prior$dist,
        param1 = param_list[[i]]$prior$par[1],
        param2 = param_list[[i]]$prior$par[2]
      )

      names(priors_realization)[[j]] <- param_list[[i]]$name
      j <- j + 1
    }
  }

  prior.density <- as.data.frame(priors_realization)

  prior_vs_posterior <- c()
  for (i in 1:ncol(prior.density)) {
    prior.par.DF <- data.frame(
      value = prior.density[, i],
      Distributions = rep("Prior", nrow(prior.density)),
      id = rep(colnames(prior.density)[i])
    )
    posterior.par.DF <- data.frame(
      value = MCMC[, i],
      Distributions = rep("Posterior", nrow(MCMC)),
      id = rep(colnames(MCMC)[i])
    )
    prior_vs_posterior <- rbind(
      prior_vs_posterior,
      prior.par.DF,
      posterior.par.DF
    )
  }

  return(list(
    MCMC = MCMC,
    prior_vs_posterior = prior_vs_posterior,
    Model_object = Model_object
  ))
}

#' Discharge simulation with uncertainties
#'
#' @param CalibrationData data frame, calibration data with a minimal information as shown in `Saint_Laurent_F2` dataset
#' @param Model_object object, model object obtained from `Estimation_Qmec` function
#' @param temp_folder directory, temporary directory to write computations (see `details`)
#' @param DoParam_Unc logical, if `TRUE` propagate parametric uncertainty
#' @param DoTotal_Unc logical, if `TRUE` propagate total uncertainty
#'
#' @return list of following components:
#' \enumerate{
#'    \item Data_obs_plot: data frame, observed data
#'    \item Qsim: list, discharge simulation with their uncertainties
#'     \itemize{
#'        \item TotalU: data frame, total uncertainty on discharge simulation
#'        \item ParamU: data frame, parametric uncertainty on discharge simulation
#'        \item MaxPost: data frame, discharge simulation following maximum posterior estimation
#'     }
#'    \item pressure: list, pressure component of the 1D shallow water equation
#'    \itemize{
#'        \item TotalU: data frame, total uncertainty on pressure simulation
#'        \item ParamU: data frame, parametric uncertainty on pressure simulation
#'        \item MaxPost: data frame, pressure simulation following maximum posterior estimation
#'     }
#'    \item friction: list, friction component of the 1D shallow water equation
#'    \itemize{
#'        \item TotalU: data frame, total uncertainty on friction simulation
#'        \item ParamU: data frame, parametric uncertainty on friction simulation
#'        \item MaxPost: data frame, friction simulation following maximum posterior estimation
#'     }
#'    \item advection: list, advection component of the 1D shallow water equation
#'    \itemize{
#'        \item TotalU: data frame, total uncertainty on advection simulation
#'        \item ParamU: data frame, parametric uncertainty on advection simulation
#'        \item MaxPost: data frame, advection simulation following maximum posterior estimation
#'     }
#' }
#' @details
#' Please ensure that the temp_folder is the same as the one used in `Estimation_Qmec` function.
#' Otherwise, an error message will appear
#'
#' Please enter the same CalibrationData as used during calibration. Dataset must have a column
#' called "date" in date-time format (mandatory). Otherwise, an error message will appear
#'
#' @importFrom RBaM dataset parameter remnantErrorModel prediction
#' @export
Prediction_Q_Qmec <- function(CalibrationData,
                              Model_object,
                              temp_folder = file.path(tempdir(), "BaM"),
                              DoParam_Unc = TRUE,
                              DoTotal_Unc = TRUE,
                              dt = 1) {
  set.seed(2024)
  CalData_object <- RBaM::dataset(
    X = CalibrationData[c("h1", "h2")],
    Y = CalibrationData[c("Q")],
    Yu = CalibrationData[c("u_Q")],
    data.dir = temp_folder
  )

  # Define a 'prediction' object with no uncertainty - this corresponds to the 'maxpost' discharge maximizing the posterior
  maxpost <- RBaM::prediction(
    X = CalData_object$data[c("h1", "h2")],
    spagFiles = "Q_maxpost.spag",
    data.dir = temp_folder,
    doParametric = FALSE, doStructural = FALSE,
    spagFiles_state = c(
      "pressureMax.spag",
      "frictionMax.spag",
      "advectionMax.spag"
    )
  )

  pred_list <- list(maxpost)
  inx <- 1 # track if more than one prediction experiment was defined

  if (DoParam_Unc) {
    inx <- inx + 1
    # Define a 'prediction' object for parametric uncertainty only - not the doStructural=FALSE
    paramU <- RBaM::prediction(
      X = CalData_object$data[c("h1", "h2")],
      spagFiles = "Q_paramU.spag",
      data.dir = temp_folder,
      doParametric = TRUE,
      doStructural = FALSE,
      spagFiles_state = c(
        "pressureParam.spag",
        "frictionParam.spag",
        "advectionParam.spag"
      )
    )

    pred_list[[inx]] <- paramU
  }

  if (DoTotal_Unc) {
    inx <- inx + 1
    # Define a 'prediction' object for total predictive uncertainty
    totalU <- RBaM::prediction(
      X = CalData_object$data[c("h1", "h2")], # stage values
      spagFiles = "Q_totalU.spag", # file where predictions are saved
      data.dir = temp_folder, # a copy of data files will be saved here
      doParametric = TRUE, # propagate parametric uncertainty, i.e. MCMC samples?
      doStructural = TRUE,
      spagFiles_state = c(
        "pressureTot.spag",
        "frictionTot.spag",
        "advectionTot.spag"
      )
    )
    pred_list[[inx]] <- totalU
  }
  # SOMETHIG IS WRONG WITH state spagFiles, total and parametric envelop are the same!
  # Remnant error
  remnant <- RBaM::remnantErrorModel(
    funk = "Constant",
    par = list(RBaM::parameter(
      name = "gamma1",
      init = 1000,
      prior.dist = "Uniform",
      prior.par = c(0, 10000)
    ))
  )
  # Re-run BaM, but in prediction mode
  RBaM::BaM(
    mod = Model_object,
    data = CalData_object,
    remnant = list(remnant),
    workspace = temp_folder,
    pred = pred_list, # list of predictions
    doCalib = FALSE,
    doPred = TRUE
  ) # Do not re-calibrate but do predictions

  # Transform -9999 to NA (missing values)
  CalibrationData$Q[which(CalibrationData$Q == -9999)] <- NA
  CalibrationData$u_Q[which(CalibrationData$u_Q == -9999)] <- NA

  Data_obs_plot <- CalibrationData[which(!is.na(CalibrationData$Q)), ]
  date_indx <- data.frame(date = CalibrationData$date)

  # Add time to the prediction experiments to return them
  i <- 0
  # Create list to store all components
  Qsim_list <- list()
  pressure_list <- list()
  friction_list <- list()
  advection_list <- list()

  if (DoTotal_Unc) {
    # Total uncertainty:
    QSim_TotalU <- data.frame(date_indx,
      read.table(
        file.path(
          temp_folder,
          "Q_totalU.env"
        ),
        header = T
      ),
      id = "totalU"
    )

    pressure_TotalU <- data.frame(date_indx,
      read.table(
        file.path(
          temp_folder,
          "pressureTot.env"
        ),
        header = T
      ) / dt,
      id = "totalPressure"
    )

    friction_TotalU <- data.frame(date_indx,
      read.table(
        file.path(
          temp_folder,
          "frictionTot.env"
        ),
        header = T
      ) / dt,
      id = "totalFriction"
    )

    advection_TotalU <- data.frame(date_indx,
      read.table(
        file.path(
          temp_folder,
          "advectionTot.env"
        ),
        header = T
      ) / dt,
      id = "totalAdvection"
    )
    i <- i + 1
    Qsim_list[[i]] <- QSim_TotalU
    names(Qsim_list)[[i]] <- "TotalU"
    pressure_list[[i]] <- pressure_TotalU
    names(pressure_list)[[i]] <- "TotalU"
    friction_list[[i]] <- friction_TotalU
    names(friction_list)[[i]] <- "TotalU"
    advection_list[[i]] <- advection_TotalU
    names(advection_list)[[i]] <- "TotalU"
  }

  if (DoParam_Unc) {
    # Parametric uncertainty:
    QSim_paramU <- data.frame(date_indx,
      read.table(
        file.path(
          temp_folder,
          "Q_paramU.env"
        ),
        header = T
      ),
      id = "paramU"
    )

    pressure_paramU <- data.frame(date_indx,
      read.table(
        file.path(
          temp_folder,
          "pressureParam.env"
        ),
        header = T
      ) / dt,
      id = "paramPressure"
    )

    friction_paramU <- data.frame(date_indx,
      read.table(
        file.path(
          temp_folder,
          "frictionParam.env"
        ),
        header = T
      ) / dt,
      id = "paramFriction"
    )

    advection_paramU <- data.frame(date_indx,
      read.table(
        file.path(
          temp_folder,
          "advectionParam.env"
        ),
        header = T
      ) / dt,
      id = "paramAdvection"
    )
    i <- i + 1
    Qsim_list[[i]] <- QSim_paramU
    names(Qsim_list)[[i]] <- "ParamU"
    pressure_list[[i]] <- pressure_paramU
    names(pressure_list)[[i]] <- "ParamU"
    friction_list[[i]] <- friction_paramU
    names(friction_list)[[i]] <- "ParamU"
    advection_list[[i]] <- advection_paramU
    names(advection_list)[[i]] <- "ParamU"
  }
  # MaxPost:
  i <- i + 1

  QSim_MaxPost <- data.frame(date_indx,
    read.table(file.path(
      temp_folder,
      "Q_maxpost.spag"
    )),
    id = "MaxPost"
  )

  pressure_MaxPost <- data.frame(date_indx,
    read.table(file.path(
      temp_folder,
      "pressureMax.spag"
    )) / dt,
    id = "MaxPostPressure"
  )

  friction_MaxPost <- data.frame(date_indx,
    read.table(file.path(
      temp_folder,
      "frictionMax.spag"
    )) / dt,
    id = "MaxPostFriction"
  )

  advection_MaxPost <- data.frame(date_indx,
    read.table(file.path(
      temp_folder,
      "advectionMax.spag"
    )) / dt,
    id = "MaxPostAdvection"
  )

  # Create list to store all components
  Qsim_list[[i]] <- QSim_MaxPost
  names(Qsim_list)[[i]] <- "MaxPost"
  pressure_list[[i]] <- pressure_MaxPost
  names(pressure_list)[[i]] <- "MaxPost"
  friction_list[[i]] <- friction_MaxPost
  names(friction_list)[[i]] <- "MaxPost"
  advection_list[[i]] <- advection_MaxPost
  names(advection_list)[[i]] <- "MaxPost"

  return(list(
    Data_obs_plot = Data_obs_plot,
    Qsim = Qsim_list,
    pressure = pressure_list,
    friction = friction_list,
    advection = advection_list
  ))
}
