#make pretty tables for supplementary
#from text files describing the full data set used for main document figures
#incidence/prevalence

supp_table_one<-read_tsv(sprintf("reports/%s_extrapolated_incidence_performance_numbers.tsv",date_code))
supp_table_two<-read_tsv(sprintf("reports/%s_extrapolated_prevalence_performance_numbers.tsv",date_code))

#fix infinite to 'not modelable'
#fix NA to text "NA" for excel
#make headers pretty
supp_table_one_pretty<-supp_table_one %>%
  mutate(diagnostic_ls_css_first=case_when(is.infinite(diagnostic_ls_css_first)~ "Not Modelable",
                                           TRUE ~ as.character(diagnostic_ls_css_first))) %>%
  mutate(across(c("overall_ppv","ppv_first","ppv_xct","diagnostic_ls_css_first","diagnostic_ls_css_xct"),
                ~ case_when(is.na(.x)~"NA",
                            TRUE ~ as.character(.x)))) %>%
  mutate(`Age band`=sprintf("%s-%s",Lower,Upper),
         Smoking=Exposure,
         `Tumor growth scenario`=case_when(scenario=="old_3" ~ "Fast",
                                           scenario=="old_4" ~ "Fast Aggressive",
                                           TRUE ~ "Other"),
         `Predicted CSO`=predicted_CSO,
         `Positives per 100,000`=total_cso_driven,
         `PPV for any cancer`=overall_ppv,
         `PPV for cancer at the CSO`=ppv_first,
         `PPV for cancers in locations other than the CSO`=ppv_xct,
         `Number of CSO-directed diagnostic tests per life saved`=diagnostic_ls_css_first,
         `Number of non-CSO directed diagnostic tests per life saved`=diagnostic_ls_css_xct) %>%
  select(Sex,
         `Age band`,Smoking,`Tumor growth scenario`,`Predicted CSO`,
         `Positives per 100,000`,`PPV for any cancer`,`PPV for cancer at the CSO`,
         `PPV for cancers in locations other than the CSO`,
         `Number of CSO-directed diagnostic tests per life saved`,
         `Number of non-CSO directed diagnostic tests per life saved`)

supp_table_two_pretty<-supp_table_two %>%
  mutate(diagnostic_ls_css_first=case_when(is.infinite(diagnostic_ls_css_first)~ "Not Modelable",
                                           TRUE ~ as.character(diagnostic_ls_css_first))) %>%
  mutate(across(c("overall_ppv","ppv_first","ppv_xct","diagnostic_ls_css_first","diagnostic_ls_css_xct"),
                ~ case_when(is.na(.x)~"NA",
                            TRUE ~ as.character(.x)))) %>%
  mutate(`Age band`=sprintf("%s-%s",Lower,Upper),
         Smoking=Exposure,
         `Tumor growth scenario`=case_when(scenario=="old_3" ~ "Fast",
                                           scenario=="old_4" ~ "Fast Aggressive",
                                           TRUE ~ "Other"),
         `Predicted CSO`=predicted_CSO,
         `Positives per 100,000`=total_cso_driven,
         `PPV for any cancer`=overall_ppv,
         `PPV for cancer at the CSO`=ppv_first,
         `PPV for cancers in locations other than the CSO`=ppv_xct,
         `Number of CSO-directed diagnostic tests per life saved`=diagnostic_ls_css_first,
         `Number of non-CSO directed diagnostic tests per life saved`=diagnostic_ls_css_xct) %>%
  select(Sex,
         `Age band`,Smoking,`Tumor growth scenario`,`Predicted CSO`,
         `Positives per 100,000`,`PPV for any cancer`,`PPV for cancer at the CSO`,
         `PPV for cancers in locations other than the CSO`,
         `Number of CSO-directed diagnostic tests per life saved`,
         `Number of non-CSO directed diagnostic tests per life saved`)

supp_pretty_sheets<-list("Supplement Table 1. Incidence"=supp_table_one_pretty,
                             "Supplement Table 2. Prevalence"=supp_table_two_pretty)

write.xlsx(supp_pretty_sheets,file=sprintf("reports/%s_supplementary_table.xlsx",date_code))

