#run through replicates to get quantiles
#focus on main figures
#female 65 Any (current,former, never)
#female/male 65
#female 55, 65,75


#inputs needed: focus on figures
draft_seer_all<-seer_raw_all %>%
  mutate(twoflag=( Lower==65 & Exposure=="Any"),
         threeflag=(Lower %in% c(55,65,75) & Exposure=="Any" & Sex=="Female"),
         fourflag=(Lower==65 & Sex=="Female")) %>%
  filter(twoflag+threeflag+fourflag>0) %>%
  select(Sex,Exposure,Lower,Upper,SEER_Draw,NET,Stage,IR)

#raw draw rates here
fp_rate_draws<-fp_draws %>%
  mutate(fp_rate=fp_draw/Total) 

         

draft_dwell_slip_rate<-multiple_dwell_slip_rate %>%
  filter(scenario=="old_3")

draft_survival_filtered_five<-survival_filtered_five

#run through iterations to get uncertainty range
#and then collapse to IQR/95% interval (mean/median) for each numerical stat
my_list<-vector("list",100)

for (i_iter in 1:500){
  print(i_iter)
  
  #draft_sens_table<-final_sens_table
  #draft_confusion_sex_matched<-local_confusion_sex_matched
  draft_sens_table<-seer_draw_basic %>% 
    left_join(draw_data_iso_sens %>% 
                rename(SEER_Draw=Cancer) %>%
                filter(iter==i_iter)) %>%
    select(SEER_Draw,Stage,c,n,sens) %>%
    mutate(sens=replace_na(sens,replace=0.0),
           n=replace_na(n,replace=0),
           c=replace_na(c,replace=0)) %>%
    select(SEER_Draw,Stage,c,n,sens)
  
  draft_fp_rate<-fp_rate_draws$fp_rate[i_iter]  #draw fp count as well
  
  draft_confusion_sex_matched<-all_confusion_iter %>%
    filter(iter==i_iter) %>%
    select(Cancer,NET,`Expected CSO`,Sex,`Predicted CSO`,final_confusion,mode_found)
  
  source("scripts/sensitivity_analysis/execute_draft_script.R")
  
  #retain only the incidence round
  my_list[[i_iter]]<-draft_ppv_situation_df
  
}

#bind all the iterations
draft_ppv_iter<-bind_rows(my_list,.id="id") 

write_tsv(draft_ppv_iter,sprintf("generated_data/%s_individual_iterations_stochastic_raw.tsv",date_code))


#reduce to useful quantities

helper_q_summary<-function(x){
  p_prob<-c(0.025,0.25,0.5,0.75,0.975)
  tibble(val=quantile(x,probs=p_prob,na.rm=TRUE),
         quant=p_prob)
}

quantile_ppv_iter<-draft_ppv_iter %>%
  group_by(Lower,Upper,Sex,Exposure,scenario,predicted_CSO,found_using) %>%
  reframe(across(where(is.numeric),helper_q_summary))

#unpack everything at once
fixed_quantile_ppv_iter <-quantile_ppv_iter %>%
  unnest(cols=c(total_cso_driven,ct,ls_css_one,ls_css_two,ls_os_one,ls_os_two,
           fp,true_CSO,false_CSO,
           total_ncso_driven,overall_ppv,ppv_first,ppv_cancer,ppv_xct,
           diagnostic_ls_css_first,diagnostic_ls_css_xct,
           diagnostic_ls_os_first,diagnostic_ls_os_xct),names_sep=":") %>%
  mutate(master_quant=`ct:quant`) %>%
  select(!contains(":quant")) %>% 
  rename_with(~ gsub(":val","",.x,fixed=TRUE)) %>%
  rename(quant=master_quant)

write_tsv(fixed_quantile_ppv_iter,sprintf("generated_data/%s_summarized_stochastic_quantile_table.tsv",date_code))



#try summary values for "success"
#how to describe well?
rates_of_stochastic_failure_limits<-draft_ppv_iter %>%
  filter(Lower>=50,Lower<=75) %>%
  anti_join(forbidden_sex_cso %>% rename(predicted_CSO=label)) %>%
  anti_join(no_stage_modeled %>% rename(predicted_CSO=label)) %>%
  anti_join(no_shift_expected %>% rename(predicted_CSO=label)) %>%
  anti_join(boring_cases %>% rename(predicted_CSO=label)) %>% 
  mutate(first_violate_ls=1*(diagnostic_ls_css_first>margin_limit),
         xct_violate_ls=1*(diagnostic_ls_css_xct>margin_limit),
         first_violate_ppv=1*(ppv_first<ppv_limit),
         xct_violate_ppv=1*(ppv_xct<ppv_limit)) %>% 
  group_by(Sex,Lower,Exposure,predicted_CSO) %>%
  summarize(first_ls_fraction=mean(first_violate_ls),
            xct_ls_fraction=mean(xct_violate_ls),
            xct_ppv_fraction=mean(xct_violate_ppv),
            first_ppv_fraction=mean(first_violate_ppv)) %>%
  ungroup() %>%
  select(Sex,Lower,Exposure,predicted_CSO,
         ppv_cso_driven_fraction=first_ppv_fraction,
         ppv_post_cso_driven_fraction=xct_ppv_fraction,
         ls_cso_driven_fraction=first_ls_fraction,
         ls_post_cso_driven_fraction=xct_ls_fraction)

write_tsv(rates_of_stochastic_failure_limits,sprintf("reports/%s_rates_of_stochastic_failure_table.tsv",date_code))
  

