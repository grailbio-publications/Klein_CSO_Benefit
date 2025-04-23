#201 summarize sensitivity analysis and others

#read in relevant files

sa_table_hr<-read_tsv(sprintf("reports/%s_hr_sensitivity.tsv",date_code))
sa_table_oss_vs_css<-read_tsv(sprintf("reports/%s_summary_limits_os_vs_css.tsv",date_code))
sa_table_stochastic_fail<-read_tsv(sprintf("reports/%s_rates_of_stochastic_failure_table.tsv",date_code))
sa_table_stochastic_quantile<-read_tsv(sprintf("generated_data/%s_summarized_stochastic_quantile_table.tsv",date_code))
sa_table_stochastic_raw<-read_tsv(sprintf("generated_data/%s_individual_iterations_stochastic_raw.tsv",date_code))
sa_table_incidence<-read_tsv(sprintf("reports/%s_extrapolated_incidence_performance_numbers.tsv",date_code))
sa_table_prevalence<-read_tsv(sprintf("reports/%s_extrapolated_prevalence_performance_numbers.tsv",date_code))
sa_table_alternate<-read_tsv(sprintf("reports/%s_summarize_alternate_strategies.tsv",date_code))

#raw data sets
sa_pretty_sheets<-list("SensA HR"=sa_table_hr,
                        "SensA OSS vs CSS"=sa_table_oss_vs_css,
                       "SensA stochastic fail"=sa_table_stochastic_fail,
                       "SensA quantile"=sa_table_stochastic_quantile,
                       "SensA stochastic raw"=sa_table_stochastic_raw,
                       "SensA incidence"=sa_table_incidence,
                       "SensA prevalence"=sa_table_prevalence,
                       "Alternate Strategies"=sa_table_alternate)

write.xlsx(sa_pretty_sheets,file=sprintf("reports/%s_sa_table.xlsx",date_code))

