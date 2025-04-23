#get prepared seer incidence


seer_raw_all<-read_tsv(sprintf("generated_data/%s_imputed_incidence_by_age_sex_smoking.tsv",input_date_code))
#supply indexes for groups within which stage shifting is computed
#i.e. population, within cancer, and within NET type (if applicable)
index_cancer<-c("Sex","Exposure","Lower","Upper","SEER_Draw","NET")

seer_raw_all<-seer_raw_all %>% rename(IR=Rate)
