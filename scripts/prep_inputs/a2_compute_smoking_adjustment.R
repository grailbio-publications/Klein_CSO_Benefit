#add exposure categories for smoking

#manual adjustment because Islami et al do not have machine readable data
#Proportion and number of cancer cases and deaths attributable to potentially modifiable risk factors in the United States
#supplement
#https://acsjournals.onlinelibrary.wiley.com/doi/10.3322/caac.21440
#supporting table 2
exposure_cg<-tribble(~Exposure,~Sex,~Prevalence,
                     "Never","Men",50.8,
                     "Former","Men",30.2,
                     "Current","Men",19.0,
                     "Never","Women",63.2,
                     "Former","Women",21.8,
                     "Current","Women",15.0)
#supporting table 3
cigarette_risk<-tribble(~Cancer,~Exposure,~Risk,
                        "Head and Neck","Current",5.66,
                        "Head and Neck","Former",1.88,
                        "Head and Neck","Never",1.0,
                        "Esophagus","Current",4.25,
                        "Esophagus","Former",2.49,
                        "Esophagus","Never",1.0,
                        "Stomach","Current",1.81,
                        "Stomach","Former",1.3,
                        "Stomach","Never",1.0,
                        "Colon/Rectum","Current",1.51,
                        "Colon/Rectum","Former",1.20,
                        "Colon/Rectum","Never",1.0,
                        "Liver","Current",2.10,
                        "Liver","Former",1.38,
                        "Liver","Never",1.0,
                        "Pancreas","Current",1.74,
                        "Pancreas","Former",1.08,
                        "Pancreas","Never",1.0,
                        "Lung","Current",23.86,
                        "Lung","Former",6.80,
                        "Lung","Never",1.0,
                        "Cervix","Current",1.9,
                        "Cervix","Former",1.5,
                        "Cervix","Never",1.0,
                        "Kidney","Current",1.55,
                        "Kidney","Former",1.38,
                        "Kidney","Never",1.0,
                        "Bladder","Current",3.90,
                        "Bladder","Former",2.37,
                        "Bladder","Never",1.0,
                        "Myeloid Neoplasm","Current",1.57,
                        "Myeloid Neoplasm","Former",1.30,
                        "Myeloid Neoplasm","Never",1.0)

exposure_fix<-exposure_cg %>%
  left_join(cigarette_risk,multiple="all") %>%
  arrange(Sex,Cancer) %>%
  group_by(Sex,Cancer) %>%
  mutate(per_unit=sum(Prevalence)/sum(Prevalence*Risk),
         fraction_of_risk=per_unit*Prevalence*Risk,
         adjust_seer=fraction_of_risk/Prevalence) %>%
  ungroup()  %>%
  select(Cancer,Exposure,Prevalence,Sex,Risk,adjust_seer)

exposure_both<-exposure_cg %>% 
  group_by(Exposure) %>%
  summarize(Prevalence=sum(Prevalence)) %>%
  ungroup() %>%
  mutate(Prevalence=100*Prevalence/sum(Prevalence),
         Sex="Men and Women") %>%
  left_join(cigarette_risk,multiple="all") %>%
  arrange(Cancer,Sex) %>%
  group_by(Cancer,Sex) %>%
  mutate(per_unit=sum(Prevalence)/sum(Prevalence*Risk),
         fraction_of_risk=per_unit*Prevalence*Risk,
         adjust_seer=fraction_of_risk/Prevalence) %>%
  ungroup() %>%
  select(Cancer,Exposure,Prevalence,Sex,Risk,adjust_seer)

exposure_smoking<-bind_rows(exposure_fix,exposure_both)

write_tsv(exposure_smoking,sprintf("generated_data/%s_multiplier_for_smoking.tsv",date_code))
