#revised code for response to reviewers
library(tidyverse)
library(readxl)

library(openxlsx)

date_code<-"20250422"
input_date_code<-"20250422"

#figures tweaked, not data
figure_date_code<-"20250422"

my_seed<-20160307

source("scripts/prep_inputs/a0_read_incidence.R")
source("scripts/prep_inputs/a1_impute_incidence.R")
source("scripts/prep_inputs/a2_compute_smoking_adjustment.R")
source("scripts/prep_inputs/a3_apply_smoking_adjustment.R")

source("scripts/prep_inputs/b0_read_confusion.R")
source("scripts/prep_inputs/b1_adjust_confusion_sex.R")
source("scripts/prep_inputs/b2_classifier_bias.R")
source("scripts/prep_inputs/b3_finalize_confusion.R")
source("scripts/prep_inputs/b4_join_cancer_to_cso.R")
#prepare for sensitivity analysis
source("scripts/prep_inputs/b5_generate_confusion_draws.R")
source("scripts/prep_inputs/b6_generate_frequency_draws.R")

source("scripts/prep_inputs/c0_isotone_sensitivity.R")
#prepare for sensitivity analysis
source("scripts/prep_inputs/c1_generate_sens_draws.R")
source("scripts/prep_inputs/c2_generate_fp_draws.R")

source("scripts/prep_inputs/d0_estimate_survival_params.R")

#now that setup is done: do the modeling

source("scripts/01_load_incidence.R")
source("scripts/02_retrieve_dwell.R")
source("scripts/03_sensitivity_table.R")
source("scripts/04_execute_parallel_interception.R")
source("scripts/05_add_CSO_to_results.R")
source("scripts/06_add_survival.R")
source("scripts/07_summarize_situations.R")
source("scripts/07a_pretty_supplement_table.R")

#helper
source("R/figure_friendly_labels.R")

#now figures
source("scripts/08_figure_one.R")
source("scripts/09_figure_two.R")
source("scripts/10_figure_three.R")
source("scripts/11_figure_four.R")
source("scripts/12_figure_five.R")
source("scripts/13_figure_six.R")

#summaries for manuscript text
source("scripts/20_summarize_figure_stats.R")


#supplemental
source("scripts/101_supplemental_flow_diagram.R")
source("scripts/102_summarize_inputs.R")

#alternate strategies
source("scripts/110_no_CSO_model.R")

#sensitivity analysis
source("scripts/sensitivity_analysis/00_coordinate_sensitivity_analysis.R")

#sensitivity analysis underlying tables driving figures
source("scripts/201_summarize_sa.R")

