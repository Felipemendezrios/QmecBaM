Qmec BaM
================
Felipe MENDEZ (INRAE, RiverLy)
Novemeber 2024

# Acknowledgement

Felipe MENDEZ, Clara CHABRILLANGEAS, Benjamin RENARD, Jérôme LE COZ
(INRAE, RiverLy and RECOVER). November 2024. Acknowledgement to Pascal
MATTE (Enviroment Canada) and Daniel BOURGAULT (UQAR, Canada) for
developing
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

<div id="refs" class="references csl-bib-body hanging-indent">

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
