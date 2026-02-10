
# R2camtrapdp

<!-- badges: start -->
<!-- badges: end -->

R2camtrapdp is an R package to convert camera trap data in R into Camtrap DP.

## Installation

You can install the development version of R2camtrapdp like so:

``` r
#install.packages("devtools")
devtools::install_github("kfukasawa37/R2camtrapdp",build_vignettes = TRUE)
```

## Vignette

R2camtrapdp have two vignettes to give details of the functions of this package. 

``` r
library(R2camtrapdp)
#For multiple camera traps
vignette("Vignette_R2camtrapdp")
#For a single camera trap
vignette("Vignette_R2camtrapdp_SingleCamera")
```

## Data

R2camtrapdp have deployments and observation data to try R2camtrapdp.

[Single camera trap data]
-Vdep is the deployment data of one camera trap, located in NIES (National Institute for Studies in Japan).
-Vobs is the video data regarding to Vdep.

[Multiple camera trap data/ This data is dummy]
-Idep is the deployment data of 10 camera trap as dummy data.
-Iobs is the image data regarding to Idep.

R2camtrapdp also has the camtrapDP data

[Single camra trap data]
-datapackageVdata is the camtrap DP data using Vobs and Vdep

[Multiple camera trap data]
-datapackageVdata02 is the camtrap DP data using Iobs and Idep

