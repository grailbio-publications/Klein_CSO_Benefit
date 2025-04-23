#table and graphs
#show specific constrasts
#reduce the large table to overall effect by CSO

#Now we sort them into buckets by CSO and aggregate numbers found and effects
ppv_situation_df<-interception_plus_CSO_survival %>% 
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

#write out the big table of relevant numbers
#from which we can take any particular slice
ppv_situation_df %>% 
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,
         total_cso_driven, total_ncso_driven,
         overall_ppv,ppv_first,ppv_xct,
         diagnostic_ls_css_first,diagnostic_ls_css_xct,
         diagnostic_ls_os_first,diagnostic_ls_os_xct) %>%
  mutate(across(contains("ppv"),function(x){round(x*100)})) %>% 
  mutate(across(contains("total"),round)) %>% 
  mutate(across(contains("diagnostic"),function(x){round(x,digits=1)})) %>%
  write_tsv(sprintf("reports/%s_extrapolated_incidence_performance_numbers.tsv",date_code))

#prevalence round (of minimal interest)
#move variables into the slots needed instead of revisiting summarization code

ppv_prevalence_situation_df<-interception_plus_CSO_survival %>% 
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

ppv_prevalence_situation_df %>% 
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,
         total_cso_driven, total_ncso_driven,
         overall_ppv,ppv_first,ppv_xct,
         diagnostic_ls_css_first,diagnostic_ls_css_xct,
         diagnostic_ls_os_first,diagnostic_ls_os_xct) %>%
  mutate(across(contains("ppv"),function(x){round(x*100)})) %>% 
  mutate(across(contains("total"),round)) %>% 
  mutate(across(contains("diagnostic"),function(x){round(x,digits=1)})) %>%
  write_tsv(sprintf("reports/%s_extrapolated_prevalence_performance_numbers.tsv",date_code))

