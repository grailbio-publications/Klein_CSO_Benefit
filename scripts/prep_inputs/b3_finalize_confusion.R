#apply classifier bias as a reference prior to the various columns

total_energy_prior<-3 #original scale

#combine with prior to fill in missing values
#note prior needs adjusting if we halved the estimate when we divided by sex
#finally normalize to 1.0 per column
#note: forbidden columns are non-zero here to avoid NaN issues
sex_cso_tp_confusion_final<-sex_tp_cso_raw %>% 
  select(`Expected CSO`,`Predicted CSO`,Sex,expected_tag,N_prime) %>%
  left_join(sex_classifier_bias %>% select(`Predicted CSO`,Sex, final_frac)) %>%
  mutate(prior_energy=case_when(expected_tag=="ok" ~ total_energy_prior/2,
                                expected_tag=="required" ~ total_energy_prior,
                                TRUE ~ total_energy_prior)) %>%
  mutate(combined_prime=N_prime+final_frac*prior_energy) %>%
  group_by(Sex,`Expected CSO`) %>%
  mutate(final_confusion=combined_prime/sum(combined_prime)) %>%
  ungroup()

#now same exercise for fp
#but because so little fp total use larger prior influence
total_energy_nc_prior<-25

sex_cso_fp_confusion_final<-sex_fp_cso_raw %>% 
  select(`Expected CSO`,`Predicted CSO`,Sex,expected_tag,N_prime) %>%
  left_join(sex_classifier_bias %>% select(`Predicted CSO`,Sex, final_frac)) %>%
  mutate(prior_energy=case_when(expected_tag=="ok" ~ total_energy_nc_prior/2,
                                expected_tag=="required" ~ total_energy_nc_prior,
                                TRUE ~ total_energy_nc_prior)) %>%
  mutate(combined_prime=N_prime+final_frac*prior_energy) %>%
  group_by(Sex,`Expected CSO`) %>%
  mutate(final_confusion=combined_prime/sum(combined_prime)) %>%
  ungroup()

#final exercise (null) for other cancers with NoneExpected CSO
#where we just use the prior

sex_cso_null_confusion_final<-sex_cso_fp_confusion_final %>%
  mutate(N_prime=0) %>%
  mutate(prior_energy=case_when(expected_tag=="ok" ~ total_energy_nc_prior/2,
                                                     expected_tag=="required" ~ total_energy_nc_prior,
                                                     TRUE ~ total_energy_nc_prior)) %>%
  mutate(combined_prime=N_prime+final_frac*prior_energy) %>%
  group_by(Sex,`Expected CSO`) %>%
  mutate(final_confusion=combined_prime/sum(combined_prime)) %>%
  ungroup() %>%
  mutate(`Expected CSO`="None assigned")

all_tp_matrix<-bind_rows(sex_cso_tp_confusion_final,sex_cso_null_confusion_final) %>%
  select(Sex,`Expected CSO`,`Predicted CSO`,final_confusion)

all_fp_matrix<-sex_cso_fp_confusion_final %>%
  select(Sex,`Expected CSO`,`Predicted CSO`,final_confusion)

#potential plotting
#all_tp_matrix %>% ggplot(aes(x=`Expected CSO`,y=`Predicted CSO`))+geom_tile(aes(fill=final_confusion),color="black")+facet_wrap(~Sex)