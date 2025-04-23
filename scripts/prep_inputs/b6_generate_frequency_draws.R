#using draws, regenerate estimates of confusion matrix

#multinom_draw_cso

#b1
sex_multinom_draw_cso<-multinom_draw_cso %>%
  cross_join(tribble(~Sex,"Male","Female")) %>% 
  left_join(required_sex_cso %>% rename(Sex=sex,`Expected CSO`=label,expected_tag=tag)) %>%
  left_join(required_sex_cso %>% rename(Sex=sex,`Predicted CSO`=label,predicted_tag=tag)) %>%
  mutate(expected_tag=replace_na(expected_tag,replace="ok")) %>%
  mutate(predicted_tag=replace_na(predicted_tag,replace="ok")) %>%
  mutate(N_prime=case_when(predicted_tag=="forbidden" ~ 0, #none of this CSO happens in this sex
                           predicted_tag=="required" ~ N, #all of this CSO must be in this sex
                           expected_tag=="required" ~ N, #all of this CSO must be in this sex
                           expected_tag=="forbidden" ~ 0, #never should happen
                           TRUE ~ N*0.5)) #training data about 50-50(* technically not true for breast)

#add null cso here
sex_multinom_null_cso<-sex_multinom_draw_cso %>%
  filter(positive_type=="fp") %>%
  mutate(`Expected CSO`="None assigned",
         positive_type="tp",
         N=0,
         N_prime=0)

sex_multinom_draw_cso<-bind_rows(sex_multinom_draw_cso,sex_multinom_null_cso)

#b2
multinom_approximate_classifier_bias<-multinom_draw_cso %>%
  filter(positive_type=="tp") %>%
  group_by(`Predicted CSO`,iter) %>%
  summarize(total=sum(N)+1) %>%
  ungroup() %>%
  mutate(ref_frac=total/sum(total)) #latent frequency estimate

#compute sex-specific prior given forbidden outcomes
sex_multinom_classifier_bias<-multinom_approximate_classifier_bias %>%
  cross_join(tribble(~Sex,"Male","Female")) %>% 
  left_join(required_sex_cso %>% rename(Sex=sex,`Predicted CSO`=label,predicted_tag=tag)) %>%
  mutate(predicted_tag=replace_na(predicted_tag,replace="ok")) %>% 
  mutate(updated_frac=case_when(predicted_tag=="forbidden" ~ 0.0,
                                TRUE ~ ref_frac)) %>%
  group_by(Sex,iter) %>%
  mutate(final_frac=updated_frac/sum(updated_frac)) %>%
  ungroup() #prior always sums to 1.0 across all valid entries

#b3
positive_prior_energy<-tribble(~positive_type,~energy,
                               "tp",3.0,
                               "fp",25.0)

sex_cso_multinom_confusion_final<-sex_multinom_draw_cso %>% 
  select(positive_type, iter, `Expected CSO`,`Predicted CSO`,Sex,expected_tag,N_prime) %>%
  left_join(positive_prior_energy) %>%
  left_join(sex_multinom_classifier_bias %>% select(iter, `Predicted CSO`,Sex, final_frac)) %>%
  mutate(prior_energy=case_when(expected_tag=="ok" ~ energy/2,
                                expected_tag=="required" ~ energy,
                                TRUE ~ energy)) %>%
  mutate(combined_prime=N_prime+final_frac*prior_energy) %>%
  group_by(Sex,`Expected CSO`,iter, positive_type) %>%
  mutate(final_confusion=combined_prime/sum(combined_prime)) %>%
  ungroup()

sex_cso_multinom_confusion_matrix<-sex_cso_multinom_confusion_final %>%
  select(positive_type,iter,Sex,`Expected CSO`,`Predicted CSO`,final_confusion)


#b4
all_to_cso_raw<-cancer_to_cso_raw %>%
  mutate(positive_type="tp") %>%
  bind_rows(tribble(~positive_type,~Cancer,~NET,~`Expected CSO`,
          "fp","Non-cancer","NotExpected","NoneExpected")) 

multinom_cancer_setup<-all_to_cso_raw %>%
  left_join(sex_cso_multinom_confusion_matrix,multiple="all")

#add some more fun
multinom_uc_setup<-uc_cancer_setup %>% 
  cross_join(multinom_cancer_setup %>% select(iter) %>% distinct()) 

all_confusion_iter<-multinom_cancer_setup %>% 
  mutate(mode_found="cfdna") %>%
  bind_rows(multinom_uc_setup %>% mutate(mode_found="uc", positive_type="uc"))

write_tsv(all_confusion_iter,sprintf("generated_data/%s_confusion_iter.tsv",date_code))
