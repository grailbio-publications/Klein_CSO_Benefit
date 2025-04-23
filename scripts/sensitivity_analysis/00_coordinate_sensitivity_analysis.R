#sensitivity analysis

#some important statements about entities to analyze
#forbidden combinations (computed incidentally)
#no stage, so no lives-saved as interesting quantities
#no stage shift expected - so no lives saved by default, boring
#boring cases - male breast
forbidden_sex_cso<-tribble(~Sex,~label,~tag,
                           "Male","Cervix","forbidden",
                           "Male","Ovary","forbidden",
                           "Male","Uterus","forbidden",
                           "Female","Prostate","forbidden")

no_stage_modeled<-tribble(~label,~tag,
                          "Plasma cell neoplasm","nostage",
                          "Myeloid lineage","nostage")

no_shift_expected<-tribble(~label,~tag,
                           "Thyroid gland","noshift",
                           "Melanocytic lineage","noshift")

boring_cases<-tribble(~Sex,~label,~tag,
                      "Male","Breast","low incidence")

#what limit for diagnostic tests per lives saved might be problematic
margin_limit<-240
ppv_limit<-0.07 #li-fraumeni

#overall uncertainty in average sensitivity

source("scripts/sensitivity_analysis/01_compute_uncertainty.R")
source("scripts/sensitivity_analysis/01_a_plots.R")
source("scripts/sensitivity_analysis/01_b_plots.R")

#stochastic uncertainty probably covers other sensitivity analyses for results

#OS vs CSS comparison - plots
source("scripts/sensitivity_analysis/02_plot_os_vs_css.R")


#plot sensitivity by dwell time
source("scripts/sensitivity_analysis/03_dwell_time.R")
source("scripts/sensitivity_analysis/03_a_plots.R")

#survival hazard ratio - use CSS
source("scripts/sensitivity_analysis/04_hazard_ratio_survival.R")


