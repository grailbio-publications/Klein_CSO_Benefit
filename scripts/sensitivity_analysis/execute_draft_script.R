
#inputs needed:
#draft_seer_all<-seer_raw_all
#draft_sens_table<-final_sens_table
#draft_fp_rate<-0.005
#draft_dwell_slip_rate<-multiple_dwell_slip_rate
#draft_confusion_sex_matched<-local_confusion_sex_matched
#draft_survival_filtered_five<-survival_filtered_five




#doing sensitivity analysis
#basic stage shift component - 04



draft_interception_stage_shift<-compute_parallel_interception_stage_shift(draft_seer_all,
                                                                    draft_sens_table,
                                                                    draft_dwell_slip_rate,
                                                                    index_cancer,
                                                                    index_scenario)



draft_interception_fp<-run_parallel_fp_model(draft_seer_all,
                                       draft_fp_rate, #fp rate corresponds to sensitivity
                                       draft_dwell_slip_rate,
                                       index_cancer,
                                       index_scenario)

#fix up the no-screening scenario
#fix up the NET for non-cancers to have only one case
#which is "NotExpected"
draft_interception_fp<-draft_interception_fp %>%
  mutate(caught=case_when(scenario=="NO" ~ 0.0,
                          TRUE ~ caught),
         prevalence_caught=case_when(scenario=="NO" ~ 0.0,
                                     TRUE ~ caught)) %>%
  filter(NET=="NotExpected")

draft_interception_stage_shift<-draft_interception_stage_shift %>% 
  bind_rows(draft_interception_fp) %>%
  filter(scenario!="MIS") #take out the theoretical max scenario


#add CSO - 05 


#join to get expected CSO
#with net
#and then join the confusion matrix to get the output divided into predicted CSO
draft_interception_plus_CSO<-draft_interception_stage_shift %>%
  mutate(found_using=gsub("soc","uc",found_using)) %>%
  left_join(draft_confusion_sex_matched %>% 
              rename(found_using=mode_found, SEER_Draw=Cancer, expected_fraction=final_confusion),multiple="all") %>%
  mutate(caught_CSO=caught*expected_fraction,
         prevalence_caught_CSO=prevalence_caught*expected_fraction) %>%
  rename(expected_CSO=`Expected CSO`,predicted_CSO=`Predicted CSO`)


#add survival - 06

#add original/shifted survival
#note using usual model of 'from original diagnosis'
#individuals survive to cancer diagnosis if minimally not overdiagnosed
#lead time can be computed if we care
draft_interception_plus_CSO_survival<-draft_interception_plus_CSO %>%
  left_join(draft_survival_filtered_five %>% rename(clinical=number_stage,original_CSS=CSS, original_OS=OS,original_NCS=NCS)) %>%
  left_join(draft_survival_filtered_five %>% rename(prequel=number_stage,shifted_CSS=CSS, shifted_OS=OS,shifted_NCS=NCS)) %>% 
  mutate(original_CSS=replace_na(original_CSS,replace=1.0),
         original_OS=replace_na(original_OS,replace=1.0),
         original_NCS=replace_na(original_NCS,replace=1.0),
         shifted_CSS=replace_na(shifted_CSS,replace=1.0),
         shifted_OS=replace_na(shifted_OS,replace=1.0),
         shifted_NCS=replace_na(shifted_NCS,replace=1.0)) %>% #mostly non-cancer, who of course survive cancer - ignoring delta for false positives
  mutate(delta_css=pmax(0,shifted_CSS-original_CSS),
         delta_os=pmax(0,shifted_OS-original_OS)) %>%
  mutate(lives_CSS_saved_CSO=caught_CSO*delta_css,
         prevalence_CSS_lives_saved_CSO=prevalence_caught_CSO*delta_css,
         lives_OS_saved_CSO=caught_CSO*delta_os,
         prevalence_OS_lives_saved_CSO=prevalence_caught_CSO*delta_os) 


#summarize down to metrics of interest
#Now we sort them into buckets by CSO and aggregate numbers found and effects
draft_ppv_situation_df<-draft_interception_plus_CSO_survival %>% 
  filter(scenario %in% c("old_3","old_4"),
         found_using=="cfdna") %>% 
  group_by(Lower,Upper,Sex,Exposure,scenario,predicted_CSO,found_using) %>% 
  summarize(total_cso_driven=sum(caught_CSO),
            ct=length(caught_CSO),
            ls_css_one=sum(lives_CSS_saved_CSO[expected_CSO==predicted_CSO]),
            ls_css_two=sum(lives_CSS_saved_CSO[expected_CSO!=predicted_CSO]),
            ls_os_one=sum(lives_OS_saved_CSO[expected_CSO==predicted_CSO]),
            ls_os_two=sum(lives_OS_saved_CSO[expected_CSO!=predicted_CSO]),
            fp=sum(caught_CSO[SEER_Draw=="Non-cancer"]),
            true_CSO=sum(caught_CSO[expected_CSO==predicted_CSO]),
            false_CSO=sum(caught_CSO[expected_CSO!=predicted_CSO & SEER_Draw!="Non-cancer"])) %>% 
  mutate(total_ncso_driven=(false_CSO+fp),
         overall_ppv=1-fp/total_cso_driven,
         ppv_first=true_CSO/total_cso_driven,
         ppv_cancer=true_CSO/(total_cso_driven-fp),
         ppv_xct=false_CSO/total_ncso_driven,
         diagnostic_ls_css_first=total_cso_driven/ls_css_one,
         diagnostic_ls_css_xct=total_ncso_driven/ls_css_two,
         diagnostic_ls_os_first=total_cso_driven/ls_os_one,
         diagnostic_ls_os_xct=total_ncso_driven/ls_os_two) %>%
  ungroup()


#prevalence round (of minimal interest)
#move variables into the slots needed instead of revisiting summarization code

draft_ppv_prevalence_situation_df<-draft_interception_plus_CSO_survival %>% 
  filter(scenario %in% c("old_3","old_4"),
         found_using=="cfdna") %>% 
  mutate(caught_CSO=prevalence_caught_CSO,  #just move variables into the slots being used
         lives_CSS_saved_CSO=prevalence_CSS_lives_saved_CSO,
         lives_OS_saved_CSO=prevalence_OS_lives_saved_CSO) %>% 
  group_by(Lower,Upper,Sex,Exposure,scenario,predicted_CSO,found_using) %>% 
  summarize(total_cso_driven=sum(caught_CSO),
            ct=length(caught_CSO),
            ls_css_one=sum(lives_CSS_saved_CSO[expected_CSO==predicted_CSO]),
            ls_css_two=sum(lives_CSS_saved_CSO[expected_CSO!=predicted_CSO]),
            ls_os_one=sum(lives_OS_saved_CSO[expected_CSO==predicted_CSO]),
            ls_os_two=sum(lives_OS_saved_CSO[expected_CSO!=predicted_CSO]),
            fp=sum(caught_CSO[SEER_Draw=="Non-cancer"]),
            true_CSO=sum(caught_CSO[expected_CSO==predicted_CSO]),
            false_CSO=sum(caught_CSO[expected_CSO!=predicted_CSO & SEER_Draw!="Non-cancer"])) %>% 
  mutate(total_ncso_driven=(false_CSO+fp),
         overall_ppv=1-fp/total_cso_driven,
         ppv_first=true_CSO/total_cso_driven,
         ppv_cancer=true_CSO/(total_cso_driven-fp),
         ppv_xct=false_CSO/total_ncso_driven,
         diagnostic_ls_css_first=total_cso_driven/ls_css_one,
         diagnostic_ls_css_xct=total_ncso_driven/ls_css_two,
         diagnostic_ls_os_first=total_cso_driven/ls_os_one,
         diagnostic_ls_os_xct=total_ncso_driven/ls_os_two) %>%
  ungroup()


#outputs: all intermediate results helpfully available because not function
#rename/copy for sensitivity analyses individually


