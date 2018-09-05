This folder contains files and scripts for the simulation of regionalized priors from synthetic fields for the paper

WORK IN PROGRESS

	> param_fields.csv (field_idx)
		contains defs for generating synthetic fields names_fields

	> param_meas.csv (meas_idx)
		contains defs for generation of measurements field_idx + names_meas

	> param_config
		contains defs for generation of prior distributions
		ultimately, config is combination of : 
		field idx, meas idx, data type -> save corresponding data in one dataframe to feed rPrior
