# rPrior_syntheticStudy
This project investigates properties of regionalized priors based on a collection of synthetic spatial random fields.


## Structure of project

It is structured as follows:

* folder 0_main contains scripts main script main.R. It generates:
  * a collection of spatial random fields (based on rPriorSynthetic::generate_fields)
  * a collection of field campaigns
    * based on rPriorSynthetic::generate_meas
    * saved in 2_data/meas
  * corresponding summary statistics (bounds, moments)
    * based on values saved in 2_data/meas
    * saved in 2_data/summary_bounds and 2_data/summary_moments
  * corresponding regionalized priors
    * by calling rPrior_single.R
    * saved in 3_rPrior
* folder 1_fields contains .rds files correponding to generated random fields
* folder 2_data contains simulated field campaigns
  * meas contains measurements from simulated field sampling campaigns
  * summary_bounds contains bounds corresponding to values in meas
  * summary_moments contains moments corresponding to values in meas
* 3_rPrior contains regionalized priors derived from simulated field campaigns
* 4_calc_res calculates performance for derived regionalized priors
(in particular the KLD between regionalized priors and underlying true distributions)
* 5_post-proc creates summary plots from performances
  
## Dependencies

Codes in this project necessitate R libraries [rPrior](https://github.com/kcucchi/rPrior) 
and [rPriorSynthetic](https://github.com/kcucchi/rPriorSynthetic).

They can be installed using the following commands within R.

```R
library(devtools)
install_github("kcucchi/rPrior")
install_github("kcucchi/rPriorSynthetic")
```