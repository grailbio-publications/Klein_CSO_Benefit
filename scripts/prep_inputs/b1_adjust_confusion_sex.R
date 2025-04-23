#adjust confusion matrix for sex

#sex-specific CSO only are predicted in appropriate sexes
#sex-specific CSO columns only occur in appropriate sexes

required_sex_cso<-tribble(~sex,~label,~tag,
                       "Female","Cervix","required",
                       "Female","Ovary","required",
                       "Female","Uterus","required",
                       "Male","Prostate","required",
                       "Male","Cervix","forbidden",
                       "Male","Ovary","forbidden",
                       "Male","Uterus","forbidden",
                       "Female","Prostate","forbidden")


#start process of separating matrix
sex_tp_cso_raw<-tp_cso_complete %>%
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
  
#check is that sum of two 'sex-approximate' confusion matrices equals the original

#now do the same operation on fp
sex_fp_cso_raw<-fp_cso_complete %>%
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
