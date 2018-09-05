# rPrior_syntheticStudy
This project investigates properties of regionalized priors based on a collection of synthetic spatial random fields.
Intended for section 3.1 in regionalized prior paper.


## Structure of project

The project is structured as follows:

* [0_main](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/0_main) 
contains main script `main.R`. It generates:
  * a collection of spatial random fields
    * based on `rPriorSynthetic::generate_fields`
    * saved in [1_fields](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/1_fields) 
  * a collection of field campaigns
    * based on `rPriorSynthetic::generate_meas`
    * saved in [2_data/meas](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/2_data/meas)
  * corresponding summary statistics (bounds, moments)
    * based on values saved in [2_data/meas](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/2_data/meas)
    * saved in [2_data/summary_bounds](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/2_data/summary_bounds) and [2_data/summary_moments](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/2_data/summary_moments)
  * corresponding regionalized priors
    * by calling `rPrior_single.R`
    * saved in [3_rPrior](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/3_rPrior)
* [1_fields](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/1_fields) 
contains `.rds` files correponding to generated random fields
* [2_data](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/2_data) 
contains `.rds` files correponding to simulated field campaigns
  * [meas](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/2_data/meas) contains `.rds` files correponding to measurements from simulated field sampling campaigns
  * [summary_bounds](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/2_data/summary_bounds)
  contains `.rds` files correponding to bounds corresponding to values in meas
  * [summary_moments](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/2_data/summary_moments)
  contains `.rds` files correponding to moments corresponding to values in meas
* [3_rPrior](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/3_rPrior) contains `.rds` files correponding to regionalized priors derived from simulated field campaigns
* [4_calc_res](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/4_calc_res) 
calculates performance for derived regionalized priors, saved in `.rds` file
(in particular the KLD between regionalized priors and underlying "true" distributions)
* [5_post-proc](https://github.com/kcucchi/rPrior_syntheticStudy/tree/master/5_post-proc)
creates summary plots from performances
  
## Dependencies

Codes in this project necessitate R libraries [rPrior](https://github.com/kcucchi/rPrior) 
and [rPriorSynthetic](https://github.com/kcucchi/rPriorSynthetic).

They can be installed using the following commands within R.

```R
library(devtools)
install_github("kcucchi/rPrior")
install_github("kcucchi/rPriorSynthetic")
```
