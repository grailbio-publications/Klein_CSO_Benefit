#manufacture excel sheet with all inputs

#seer draw parameters
incidence_specs<-read_excel(path=sprintf("data/%s",seer_cso_file),
                          sheet="CSO benefit incidence specs",
                          range="A1:B212",
                          col_names=c("V1","V2"))

css_specs<-read_excel(path=sprintf("data/%s",seer_cso_file),
                            sheet="CSO benefit CSS specs",
                            range="A1:B232",
                            col_names=c("V1","V2"))

os_specs<-read_excel(path=sprintf("data/%s",seer_cso_file),
                      sheet="CSO benefit OS specs",
                      range="A1:B230",
                      col_names=c("V1","V2"))


#original files

#seer
#incidence_basic

#risk adjustment
#exposure_cg
#cigarette_risk

#sensitivity
#full_ccga_iso_sens

#confusion
#bind_rows(tp=tp_cso_complete,fp=fp_cso_complete,.id="positive_type") - confusion numbers
#cancer_to_cso_raw

#survival used (processed)
survival_used<-total_survival %>%
  filter(Months==60) %>%
  select(Sex,Lower,Upper,SEER_Draw,NET,Stage,Months,CSS,OS,NCS)

#dwell times
dwell_used<-
  dwell_all %>% filter(scenario %in% c("old_3","old_4")) %>%
  mutate(label=case_when(scenario=="old_3" ~ "Fast",
                       scenario=="old_4" ~ "Fast Aggressive",
                       TRUE ~ "NA"))

#assemble spreadsheet

reference_data_sheet=tribble(~Sheet,~Reference,~ReferenceData,
                             "seer_spec_incidence",
                             "Software:  Surveillance Research Program, National Cancer Institute SEER*Stat software (www.seer.cancer.gov/seerstat) version 8.4.3.",
                             "Data:  Surveillance, Epidemiology, and End Results (SEER) Program (www.seer.cancer.gov) SEER*Stat Database: Incidence - SEER Research Data, 17",
                             "seer_spec_css",
                             "Software:  Surveillance Research Program, National Cancer Institute SEER*Stat software (www.seer.cancer.gov/seerstat) version 8.4.3.",
                             "Data:  Surveillance, Epidemiology, and End Results (SEER) Program (www.seer.cancer.gov) SEER*Stat Database: Incidence - SEER Research Data, 17",
                             "seer_spec_os",
                             "Software:  Surveillance Research Program, National Cancer Institute SEER*Stat software (www.seer.cancer.gov/seerstat) version 8.4.3.",
                             "Data:  Surveillance, Epidemiology, and End Results (SEER) Program (www.seer.cancer.gov) SEER*Stat Database: Incidence - SEER Research Data, 17",
                             "cigarette_exposure",
                             "https://acsjournals.onlinelibrary.wiley.com/doi/10.3322/caac.21440",
                             "Supporting Table 2",
                             "cigarette_risk",
                             "https://acsjournals.onlinelibrary.wiley.com/doi/10.3322/caac.21440",
                             "Supporting Table 3",
                             "sensitivity",
                             "Klein EA, Richards D, Cohn A, Tummala M, Lapham R, Cosgrove D, Chung G, Clement J, Gao J, Hunkapiller N, Jamshidi A, Kurtzman KN, Seiden MV, Swanton C, Liu MC. Clinical validation of a targeted methylation-based multi-cancer early detection test using an independent validation set. Ann Oncol. 2021 Sep;32(9):1167-1177. doi: 10.1016/j.annonc.2021.05.806. Epub 2021 Jun 24. PMID: 34176681.",
"internal table",
"confusion_matrix",
"Klein EA, Richards D, Cohn A, Tummala M, Lapham R, Cosgrove D, Chung G, Clement J, Gao J, Hunkapiller N, Jamshidi A, Kurtzman KN, Seiden MV, Swanton C, Liu MC. Clinical validation of a targeted methylation-based multi-cancer early detection test using an independent validation set. Ann Oncol. 2021 Sep;32(9):1167-1177. doi: 10.1016/j.annonc.2021.05.806. Epub 2021 Jun 24. PMID: 34176681.",
"confusion matrix in paper",
"cancer_to_cso",
"Klein EA, Richards D, Cohn A, Tummala M, Lapham R, Cosgrove D, Chung G, Clement J, Gao J, Hunkapiller N, Jamshidi A, Kurtzman KN, Seiden MV, Swanton C, Liu MC. Clinical validation of a targeted methylation-based multi-cancer early detection test using an independent validation set. Ann Oncol. 2021 Sep;32(9):1167-1177. doi: 10.1016/j.annonc.2021.05.806. Epub 2021 Jun 24. PMID: 34176681.",
"supplemental",
"dwell_times",
"Earl Hubbell, Christina A. Clarke, Alexander M. Aravanis, Christine D. Berg; Modeled Reductions in Late-stage Cancer with a Multi-Cancer Early Detection Test. Cancer Epidemiol Biomarkers Prev 1 March 2021; 30 (3): 460–468. https://doi.org/10.1158/1055-9965.EPI-20-1134",
"supplemental"
)

input_parameter_sheets<-list("references"=reference_data_sheet,
                             "seer_spec_incidence"=incidence_specs,
                       "seer_spec_css"=css_specs,
                       "seer_spec_os"=os_specs,
                       "seer_incidence"=incidence_basic,
                       "seer_survival"=survival_used,
                       "cigarete_exposure"=exposure_cg,
                       "cigarette_risk"=cigarette_risk,
                       "sensitivity"=full_ccga3_iso_sens,
                       "confusion_matrix"=bind_rows(tp=tp_cso_complete,fp=fp_cso_complete,.id="positive_type"),
                       "cancer_to_cso"=cancer_to_cso_raw,
                       "dwell_times"=dwell_used)

write.xlsx(input_parameter_sheets,file=sprintf("reports/%s_input_parameters.xlsx",date_code))
