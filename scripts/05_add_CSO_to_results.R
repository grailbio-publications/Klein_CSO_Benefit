#add CSO to intercepted
#model basic test report received
#

local_confusion_sex_matched<-read_tsv(sprintf("generated_data/%s_confusion_sex_matched.tsv",input_date_code))


#join to get expected CSO
#with net
#and then join the confusion matrix to get the output divided into predicted CSO
interception_plus_CSO<-interception_stage_shift %>%
  mutate(found_using=gsub("soc","uc",found_using)) %>%
  left_join(local_confusion_sex_matched %>% 
              rename(found_using=mode_found, SEER_Draw=Cancer, expected_fraction=final_confusion),multiple="all") %>%
  mutate(caught_CSO=caught*expected_fraction,
         prevalence_caught_CSO=prevalence_caught*expected_fraction) %>%
  rename(expected_CSO=`Expected CSO`,predicted_CSO=`Predicted CSO`)
