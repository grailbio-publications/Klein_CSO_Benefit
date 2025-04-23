#now adjust for prior classifier biases to fill in zero entries

#classifier has been optimized
#produces predictions that should be in approximate proportion to positive set
#add positive bias as usual to allow for unobserved outcomes
approximate_classifier_bias<-tp_cso_complete %>%
  group_by(`Predicted CSO`) %>%
  summarize(total=sum(N)+1) %>%
  ungroup() %>%
  mutate(ref_frac=total/sum(total)) #latent frequency estimate

#compute sex-specific prior given forbidden outcomes
sex_classifier_bias<-approximate_classifier_bias %>%
  cross_join(tribble(~Sex,"Male","Female")) %>% 
  left_join(required_sex_cso %>% rename(Sex=sex,`Predicted CSO`=label,predicted_tag=tag)) %>%
  mutate(predicted_tag=replace_na(predicted_tag,replace="ok")) %>% 
  mutate(updated_frac=case_when(predicted_tag=="forbidden" ~ 0.0,
                                TRUE ~ ref_frac)) %>%
  group_by(Sex) %>%
  mutate(final_frac=updated_frac/sum(updated_frac)) %>%
  ungroup() #prior always sums to 1.0 across all valid entries
  
