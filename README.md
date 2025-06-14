Qmec BaM
================
Felipe MENDEZ (INRAE, RiverLy)
Novemeber 2024

# Acknowledgement

Felipe MENDEZ, Clara CHABRILLANGEAS, Benjamin RENARD, Jérôme LE COZ
(INRAE, RiverLy and RECOVER). November 2024. Acknowledgement to Pascal
MATTE (Enviroment Canada) and Daniel BOURGAULT (UQAR, Canada) for
developing and sharing the code of
[Qmec](https://codeocean.com/capsule/8881837/tree/v2),Bourgault and
Matte (2022), as well as for their support and collaboration throughout
this project.

# Introduction

[Qmec](https://doi.org/10.1029/2019JC015992) is a physically based,
fully nonlinear, and nonstead reach-averaged river model to provide
instantaneous freshwater discharge rates in tidal rivers using water
level measurements alone at two tide gauge stations developed by
Bourgault and Matte (2020).

The goal of `QmecBaM` is to incorporate the associated uncertainties
during the calibration and simulation of discharge time series for a
tidal river, achieved using Bayesian estimation. To achieve this, the
package [BaM!](https://github.com/BaM-tools/BaM) will be applied.

## Installation

You can install the development version of
[QmecBaM](https://github.com/Felipemendezrios/QmecBaM) from GitHub using
the following command. Please note that the
[RBaM](https://github.com/BaM-tools/RBaM) package developed by [Renard
(2017)](https://hal.inrae.fr/hal-02606929) is also required to run this
package.

``` r
# Before first use, install RatingShiftHappens and RBaM packages ones and for all, following these commands:

# devtools::install_github("Felipemendezrios/QmecBaM")
# devtools::install_github('BaM-tools/RBaM')

library(QmecBaM)
```

## Case Saint Lawrence in Canada

The first case study is the Saint Lawrence River, located in eastern
Canada, from Bécancourt station to Saint-François station. This section
was chosen because it is strongly influenced by the tide, being close to
the mouth of the Gulf of Saint Lawrence, which flows into the Atlantic
Ocean.

In this example, the reach concerned covers the section between Neuville
and Lauzon. Prior knowledge on geometric properties is required to run
the model, as shown below:

``` r
# Prior knowledge
priors <- data.frame(
     # constant effective width of the rectangular channel
     Be = c(
          1500, # initial guess
          "LogNormal", # distribution
          log(1500), 0.1
     ), # parameters of the distribution
     # River bed elevation relative to a reference system
     be = c(
          -14.6915,
          "Gaussian",
          -14.6915, 2
     ),
     # Leveling error
     delta = c(
          0,
          "Gaussian",
          0, 0.02
     ),
     # effective friction coefficient
     ne = c(
          0.023,
          "LogNormal",
          log(0.023), 0.15
     ),
     # Initial discharge
     Q0 = c(
          12500,
          "Gaussian",
          12500, 10000
     ),
     # Distance between two gauge stations
     dx = c(
          38000,
          "Gaussian",
          38000, 250
     )
)

# Put in object for using RBaM package
Be <- RBaM::parameter(
     name = "Be",
     init = as.numeric(priors$Be[1]),
     prior.dist = priors$Be[2],
     prior.par = as.numeric(priors$Be[c(3, 4)])
)

be <- RBaM::parameter(
     name = "be",
     init = as.numeric(priors$be[1]),
     prior.dist = priors$be[2],
     prior.par = as.numeric(priors$be[c(3, 4)])
)

delta <- RBaM::parameter(
     name = "delta",
     init = as.numeric(priors$delta[1]),
     prior.dist = priors$delta[2],
     prior.par = as.numeric(priors$delta[c(3, 4)])
)

ne <- RBaM::parameter(
     name = "ne",
     init = as.numeric(priors$ne[1]),
     prior.dist = priors$ne[2],
     prior.par = as.numeric(priors$ne[c(3, 4)])
)

Q0 <- RBaM::parameter(
     name = "Q0",
     init = as.numeric(priors$Q0[1]),
     prior.dist = priors$Q0[2],
     prior.par = as.numeric(priors$Q0[c(3, 4)])
)

dx <- RBaM::parameter(
     name = "dx",
     init = as.numeric(priors$dx[1]),
     prior.dist = priors$dx[2],
     prior.par = as.numeric(priors$dx[c(3, 4)])
)

# Chart Datum of the upstream tide gauge
d1 <- -1.379
# Chart Datum of the downstream tide gauge
d2 <- -1.958
# temporal discretization of the model in seconds
dt_model <- 60

results_calibration <- Estimation_Qmec(
     CalibrationData = Saint_Laurent_F2,
     Be_object = Be,
     be_object = be,
     delta_object = delta,
     ne_object = ne,
     Q0_object = Q0,
     dx_object = dx,
     CD1 = d1,
     CD2 = d2,
     dt_model = dt_model
)

# See MCMC results:
MCMC_exploration <- RBaM::tracePlot(results_calibration$MCMC)
gridExtra::grid.arrange(grobs = MCMC_exploration, ncol = 3)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r

# See scatterplot
psych::pairs.panels(
     results_calibration$MCMC[, -c(
          length(results_calibration$MCMC) - 1,
          length(results_calibration$MCMC)
     )],
     smooth = FALSE,
     lm = T,
     scale = T,
     method = "pearson", # correlation method
     hist.col = "#00AFBB",
     cor = T,
     ci = TRUE,
     density = TRUE, # show density plots
     ellipses = TRUE
) # show correlation ellipses
```

<img src="man/figures/README-unnamed-chunk-3-2.png" width="100%" /> In
the context of Bayesian estimation, it is necessary to check the prior
and posterior distribution to ensure that the estimate is correct or
diagnostic of the data or model.

``` r
Plot_prior_posterior(DF_prior_posterior = results_calibration$prior_vs_posterior)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

The model parameters have been estimated so far. Let’s simulate the
discharge time series.

``` r
simulation_QMEC <- Prediction_Q_Qmec(
     CalibrationData = Saint_Laurent_F2,
     Model_object = results_calibration$Model_object
)
```

Let’s plot the simulations with the associated uncertainties and the
observations:

``` r
Q_plot <- plot_Q_sim_Qmec(
     Q_observed = simulation_QMEC$Data_obs_plot,
     Q_simulated = simulation_QMEC$Qsim
)

Q_plot
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r

# Zoom:
library(ggplot2)

Q_plot +
     coord_cartesian(x = c(
          min(Saint_Laurent_F2$date[which(Saint_Laurent_F2$Q != -9999)]),
          max(Saint_Laurent_F2$date[which(Saint_Laurent_F2$Q != -9999)])
     ))
```

<img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" />

Finally, it is also possible to plot the components of the 1D shallow
water equation (pressure gradient, bottom friction and advection) to
understand tidal dynamics:

``` r
SW_plot <- plot_shallow_water(
     pressure_SW = simulation_QMEC$pressure,
     friction_SW = simulation_QMEC$friction,
     advection_SW = simulation_QMEC$advection
)
# Total uncertainty:
SW_plot$TotalU
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

``` r

# MaxPost:
SW_plot$MaxPost
```

<img src="man/figures/README-unnamed-chunk-7-2.png" width="100%" />

``` r

# combine input and output data
input_stage_plot <- ggplot(Saint_Laurent_F2, aes(x = date)) +
     geom_line(aes(y = h1, col = "Upstream")) +
     geom_line(aes(y = h2, col = "Downstream")) +
     labs(
          x = "Time",
          y = "Stage [m]",
          col = "Gauge station"
     ) +
     theme_bw()

library(patchwork)

input_stage_plot +
     Q_plot +
     SW_plot$MaxPost +
     plot_layout(
          ncol = 1,
          guides = "collect",
          axis_titles = "collect"
     )
```

<img src="man/figures/README-unnamed-chunk-7-3.png" width="100%" />

## Case Lower Seine in France

A second case study is proposed to test the tool and assess its
applicability to other tidal rivers.

The Lower Seine is located between the Poses dam and the mouth of the
Seine, where it flows into the English Channel. It lies within the
Seine-Normandy basin in the Normandy region.

It was decided to test the tool on the section extending from the gauge
station at Oissel upstream to Petite-Couronne downstream. The prior
knowledge is specified here:

``` r
# Prior knowledge
priors_LS <- data.frame(
     # constant effective width of the rectangular channel
     Be = c(
          150, # initial guess
          "LogNormal", # distribution
          log(150), 0.3
     ), # parameters of the distribution
     # River bed elevation relative to a reference system
     be = c(
          -5,
          "Gaussian",
          -5, 1
     ),
     # Leveling error
     delta = c(
          0,
          "Gaussian",
          0, 0.02
     ),
     # effective friction coefficient
     ne = c(
          0.025,
          "LogNormal",
          log(0.025), 0.15
     ),
     # Initial discharge
     Q0 = c(
          410,
          "Gaussian",
          410, 100
     ),
     # Distance between two gauge stations
     dx = c(
          23747,
          "Gaussian",
          23747, 200
     )
)

# Put in object for using RBaM package
Be_LS <- RBaM::parameter(
     name = "Be",
     init = as.numeric(priors_LS$Be[1]),
     prior.dist = priors_LS$Be[2],
     prior.par = as.numeric(priors_LS$Be[c(3, 4)])
)

be_LS <- RBaM::parameter(
     name = "be",
     init = as.numeric(priors_LS$be[1]),
     prior.dist = priors_LS$be[2],
     prior.par = as.numeric(priors_LS$be[c(3, 4)])
)

delta_LS <- RBaM::parameter(
     name = "delta",
     init = as.numeric(priors_LS$delta[1]),
     prior.dist = priors_LS$delta[2],
     prior.par = as.numeric(priors_LS$delta[c(3, 4)])
)

ne_LS <- RBaM::parameter(
     name = "ne",
     init = as.numeric(priors_LS$ne[1]),
     prior.dist = priors_LS$ne[2],
     prior.par = as.numeric(priors_LS$ne[c(3, 4)])
)

Q0_LS <- RBaM::parameter(
     name = "Q0",
     init = as.numeric(priors_LS$Q0[1]),
     prior.dist = priors_LS$Q0[2],
     prior.par = as.numeric(priors_LS$Q0[c(3, 4)])
)

dx_LS <- RBaM::parameter(
     name = "dx",
     init = as.numeric(priors_LS$dx[1]),
     prior.dist = priors_LS$dx[2],
     prior.par = as.numeric(priors_LS$dx[c(3, 4)])
)

# Chart Datum of the upstream tide gauge
d1_LS <- 0
# Chart Datum of the downstream tide gauge
d2_LS <- 0
# temporal discretization of the model in seconds
dt_model_LS <- 60

results_calibration_LS <- Estimation_Qmec(
     CalibrationData = Lower_Seine_Rouen,
     Be_object = Be_LS,
     be_object = be_LS,
     delta_object = delta_LS,
     ne_object = ne_LS,
     Q0_object = Q0_LS,
     dx_object = dx_LS,
     CD1 = d1_LS,
     CD2 = d2_LS,
     dt_model = dt_model_LS
)

# See MCMC results:
MCMC_exploration_LS <- RBaM::tracePlot(results_calibration_LS$MCMC)
gridExtra::grid.arrange(grobs = MCMC_exploration_LS, ncol = 3)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

``` r

# See scatterplot
psych::pairs.panels(
     results_calibration_LS$MCMC[, -c(
          length(results_calibration_LS$MCMC) - 1,
          length(results_calibration_LS$MCMC)
     )],
     smooth = FALSE,
     lm = T,
     scale = T,
     method = "pearson", # correlation method
     hist.col = "#00AFBB",
     cor = T,
     ci = TRUE,
     density = TRUE, # show density plots
     ellipses = TRUE
) # show correlation ellipses
```

<img src="man/figures/README-unnamed-chunk-8-2.png" width="100%" />

Here a plot indicating the prior and posterior distribution:

``` r
Plot_prior_posterior(DF_prior_posterior = results_calibration_LS$prior_vs_posterior)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

Here the discharge simulation:

``` r
simulation_QMEC_LS <- Prediction_Q_Qmec(
     CalibrationData = Lower_Seine_Rouen,
     Model_object = results_calibration_LS$Model_object
)
```

Let’s plot the simulations with the associated uncertainties and the
observations:

``` r
Q_plot_LS <- plot_Q_sim_Qmec(
     Q_observed = simulation_QMEC_LS$Data_obs_plot,
     Q_simulated = simulation_QMEC_LS$Qsim
)

Q_plot_LS
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

``` r

# Zoom:
Q_plot_LS +
     coord_cartesian(x = c(
          min(Lower_Seine_Rouen$date[which(Lower_Seine_Rouen$Q != -9999)]),
          max(Lower_Seine_Rouen$date[which(Lower_Seine_Rouen$Q != -9999)])
     ))
```

<img src="man/figures/README-unnamed-chunk-11-2.png" width="100%" />

Finally, it is also possible to plot the components of the 1D shallow
water equation:

``` r
SW_plot_LS <- plot_shallow_water(
     pressure_SW = simulation_QMEC_LS$pressure,
     friction_SW = simulation_QMEC_LS$friction,
     advection_SW = simulation_QMEC_LS$advection
)
# Total uncertainty:
SW_plot_LS$TotalU
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

``` r

# MaxPost:
SW_plot_LS$MaxPost + coord_cartesian(x = c(
     min(Lower_Seine_Rouen$date[which(Lower_Seine_Rouen$Q != -9999)]) - 10 * 3600,
     max(Lower_Seine_Rouen$date[which(Lower_Seine_Rouen$Q != -9999)]) + 7 * 3600
))
```

<img src="man/figures/README-unnamed-chunk-12-2.png" width="100%" />

``` r

# combine input and output data
input_stage_plot_LS <- ggplot(Lower_Seine_Rouen, aes(x = date)) +
     geom_line(aes(y = h1, col = "Upstream")) +
     geom_line(aes(y = h2, col = "Downstream")) +
     labs(
          x = "Time",
          y = "Stage [m]",
          col = "Gauge station"
     ) +
     theme_bw()

input_stage_plot_LS +
     Q_plot_LS +
     SW_plot_LS$MaxPost +
     plot_layout(
          ncol = 1,
          guides = "collect",
          axis_titles = "collect"
     )
```

<img src="man/figures/README-unnamed-chunk-12-3.png" width="100%" />

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-bourgaultPhysicallyBasedMethod2020" class="csl-entry">

Bourgault, Daniel, and Pascal Matte. 2020. “A Physically Based Method
for Real-Time Monitoring of Tidal River Discharges From Water Level
Observations, With an Application to the St. Lawrence River.” *Journal
of Geophysical Research: Oceans* 125 (5): e2019JC015992.
<https://doi.org/10.1029/2019JC015992>.

</div>

<div id="ref-bourgaultQmecMatlabCode2022" class="csl-entry">

———. 2022. “Qmec : A Matlab Code to Compute Tidal River Discharge from
Water Level.” Code Ocean. <https://doi.org/10.24433/CO.7299598.V2>.

</div>

<div id="ref-renardBaMBayesianModeling2017" class="csl-entry">

Renard, Benjamin. 2017. “BaM ! (Bayesian Modeling): Un code de calcul
pour l’estimation d’un modèle quelconque et son utilisation en
prédiction.” {Report}. irstea.

</div>

</div>
