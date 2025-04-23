#apply smoking adjustment

smoking_risk_adjustment<-exposure_smoking

smoking_risk_fixed_a<-smoking_risk_adjustment %>% 
  rename(SEER_Draw=Cancer) %>%
  mutate(Sex=case_when(Sex=="Men" ~ "Male",
                       Sex=="Women" ~ "Female",
                       TRUE ~ "Male and female")) %>%
  mutate(SEER_Draw=case_when(SEER_Draw=="Liver" ~ "Liver/Bile-duct",
                             TRUE ~ SEER_Draw)) #fix up description

smoking_risk_fixed<-smoking_risk_fixed_a %>%
  bind_rows(smoking_risk_fixed_a %>% 
              mutate(Exposure="Any",
                     Prevalence=100.0,
                     Risk=1.0,
                     adjust_seer=1.0) %>%
              distinct()) %>%
  arrange(SEER_Draw,Exposure,Sex)

#set up multipliers for all the cancers not known to be affected
update_bkg<-seer_imputed_all %>%
  select(SEER_Draw,Sex) %>%
  distinct() %>%
  left_join(smoking_risk_fixed %>%
              select(Exposure,Prevalence,Sex) %>% 
              distinct(),multiple="all") %>%
  mutate(Risk=1.0,adjust_seer=1.0) %>%
  anti_join(smoking_risk_fixed %>% select(SEER_Draw) %>% distinct()) %>%
  select(SEER_Draw,Exposure,Prevalence,Sex,Risk,adjust_seer)

smoking_risk_final<-bind_rows(smoking_risk_fixed,update_bkg) %>%
  arrange(SEER_Draw,Sex,Exposure)

#apply to seer
smoking_seer_sex_year<-seer_imputed_all %>% 
  left_join(smoking_risk_final,multiple="all") %>%
  mutate(Rate=IR*adjust_seer) %>%
  select(Sex,SEER_Draw,NET,Stage,Lower,Upper,Rate,Exposure,Prevalence,Risk,adjust_seer)

#now we have the "official" estimate for incidence rates
#modified for smoking exposure levels
seer_sex_year_smoking_df<-smoking_seer_sex_year %>%
  select(Sex,Exposure,Lower,Upper,SEER_Draw,NET,Stage,Rate)

write_tsv(seer_sex_year_smoking_df,sprintf("generated_data/%s_imputed_incidence_by_age_sex_smoking.tsv",date_code))

