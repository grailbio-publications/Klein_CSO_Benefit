#compute hazard ratio and apply
#note this only involves basic hazards and doesn't affect proportion found
#only the proportion of deaths affected
#compute for CSS
#just use 2.0 as an estimate (see latest)
#note this hazard ratio model is an approximation and is not likely "true"

#hidden assumption: cfdna positive at detection->would be cfdna positive at clinical diagnosis

#final_sens_table
#survival_filtered_five
#interception_plus_CSO (to add)

source("R/compute_split_survival.R")

#this now includes a computed CSS split by whether the cancer would be cfdna detectable at clinical diagnosis
#and a postulated hazard ratio between groups
#so that when survival is linked to stage shift
#we wind up with sweeping more deaths into detected
#which mitigates the effect of lowered survival at early stages
modified_survival_five<-survival_data %>%
  filter(Months==60) %>%
  select(Sex,Lower,SEER_Draw,NET,Stage,CSS) %>%
  left_join(final_sens_table %>% select(SEER_Draw,Stage,sens)) %>%
  filter(!is.na(sens)) %>%
  mutate(hr=2.0) %>%
  rowwise() %>% 
  mutate(cfdna_negative=find_cfdna_status_survival(CSS,hr,sens)) %>%
  ungroup() %>%
  mutate(cfdna_positive=cfdna_negative^hr) %>% 
  select(Sex,Lower,SEER_Draw,NET,Stage,cfdna_positive,cfdna_negative) %>%
  pivot_longer(cols=c("cfdna_negative","cfdna_positive"),names_to="status",values_to="CSS") %>%
  mutate(cfdna_detectable=case_when(status=="cfdna_positive" ~ "yes",
                                    TRUE ~ "no")) %>%
  mutate(number_stage=match(Stage,c("I","II","III","IV","NotStaged"))) %>%
  select(Sex,Lower,SEER_Draw,NET,cfdna_detectable,number_stage,CSS)

#now once again we replicate linking to survival
#this time using cfdna_detectable as a linking variable
#but just look at CSS
modified_interception_plus_CSO_survival<-interception_plus_CSO %>%
  left_join(modified_survival_five %>% rename(clinical=number_stage,original_CSS=CSS)) %>%
  left_join(modified_survival_five %>% rename(prequel=number_stage,shifted_CSS=CSS)) %>% 
  mutate(original_CSS=replace_na(original_CSS,replace=1.0),
         shifted_CSS=replace_na(shifted_CSS,replace=1.0)) %>% #mostly non-cancer, who of course survive cancer - ignoring delta for false positives
  mutate(delta_css=pmax(0,shifted_CSS-original_CSS)) %>%
  mutate(lives_CSS_saved_CSO=caught_CSO*delta_css,
         prevalence_CSS_lives_saved_CSO=prevalence_caught_CSO*delta_css) 

#replicate summarization
#note there are no changes to ppv due to survival alterations
modified_ppv_situation_df<-modified_interception_plus_CSO_survival %>% 
  filter(scenario %in% c("old_3","old_4"),
         found_using=="cfdna") %>% 
  group_by(Lower,Upper,Sex,Exposure,scenario,predicted_CSO,found_using) %>% 
  summarize(total_cso_driven=sum(caught_CSO),
            ct=length(caught_CSO),
            ls_css_one=sum(lives_CSS_saved_CSO[expected_CSO==predicted_CSO]),
            ls_css_two=sum(lives_CSS_saved_CSO[expected_CSO!=predicted_CSO]),
            fp=sum(caught_CSO[SEER_Draw=="Non-cancer"]),
            true_CSO=sum(caught_CSO[expected_CSO==predicted_CSO]),
            false_CSO=sum(caught_CSO[expected_CSO!=predicted_CSO & SEER_Draw!="Non-cancer"])) %>% 
  mutate(total_ncso_driven=(false_CSO+fp),
         overall_ppv=1-fp/total_cso_driven,
         ppv_first=true_CSO/total_cso_driven,
         ppv_cancer=true_CSO/(total_cso_driven-fp),
         ppv_xct=false_CSO/total_ncso_driven,
         diagnostic_ls_css_first=total_cso_driven/ls_css_one,
         diagnostic_ls_css_xct=total_ncso_driven/ls_css_two) %>%
  ungroup()

#now join up to allow for comparison
compare_ppv_situation_df<-modified_ppv_situation_df %>%
  left_join(ppv_situation_df %>% 
              select(Lower,Sex,Exposure,scenario,predicted_CSO,
                     o_ls_css_one=ls_css_one,o_ls_css_two=ls_css_two,
                     o_diagnostic_ls_css_first=diagnostic_ls_css_first,o_diagnostic_ls_css_xct=diagnostic_ls_css_xct))

#write out to save
write_tsv(compare_ppv_situation_df,sprintf("reports/%s_hr_sensitivity.tsv",date_code))
#get some summary values
#take out the unmodelable set
#take out a) outside standard age range
#take out b) boring cases known already (male breast)
#count anything that changes
#global summary of what changes
compare_hr_ratio_ls_summary<-compare_ppv_situation_df %>%
  filter(Lower>=50,Lower<=75) %>%
  anti_join(forbidden_sex_cso %>% rename(predicted_CSO=label)) %>%
  anti_join(no_stage_modeled %>% rename(predicted_CSO=label)) %>%
  anti_join(no_shift_expected %>% rename(predicted_CSO=label)) %>%
  anti_join(boring_cases %>% rename(predicted_CSO=label)) %>%
  mutate(first_ratio=diagnostic_ls_css_first/o_diagnostic_ls_css_first,
         xct_ratio=diagnostic_ls_css_xct/o_diagnostic_ls_css_xct) %>%
  select(Sex,Lower,Exposure,predicted_CSO,first_ratio,xct_ratio) %>%
#  group_by(Sex) %>%
  reframe(across(where(is.numeric),helper_q_summary)) %>%
#  ungroup() %>%
  unnest(cols=c(first_ratio,xct_ratio),names_sep=":") %>%
  mutate(master_quant=`first_ratio:quant`) %>%
  select(!contains(":quant")) %>% 
  rename_with(~ gsub(":val","",.x,fixed=TRUE)) %>%
  rename(quant=master_quant) 

margin_limit<-240

#count any violators
compare_ppv_situation_df %>%
  filter(Lower>=50,Lower<=75,scenario=="old_3") %>%
  anti_join(forbidden_sex_cso %>% rename(predicted_CSO=label)) %>%
  anti_join(no_stage_modeled %>% rename(predicted_CSO=label)) %>%
  anti_join(no_shift_expected %>% rename(predicted_CSO=label)) %>%
  anti_join(boring_cases %>% rename(predicted_CSO=label)) %>%
  mutate(first_violate_ls=1*(diagnostic_ls_css_first>margin_limit),
         xct_violate_ls=1*(diagnostic_ls_css_xct>margin_limit),
         o_first_violate_ls=1*(o_diagnostic_ls_css_first>margin_limit),
         o_xct_violate_ls=1*(o_diagnostic_ls_css_xct>margin_limit)) %>%
  select(first_violate_ls,xct_violate_ls,o_first_violate_ls,o_xct_violate_ls) %>%
  summarize(across(where(is.numeric),mean))
  
#find examples
#again only 50-year old never smokers,xct lives saved
#because of diagnostic tests ratio

compare_ppv_situation_df %>%
  filter(Lower>=50,Lower<=75,scenario=="old_3") %>%
  anti_join(forbidden_sex_cso %>% rename(predicted_CSO=label)) %>%
  anti_join(no_stage_modeled %>% rename(predicted_CSO=label)) %>%
  anti_join(no_shift_expected %>% rename(predicted_CSO=label)) %>%
  anti_join(boring_cases %>% rename(predicted_CSO=label)) %>% 
  select(Lower,Sex,Exposure,predicted_CSO,diagnostic_ls_css_first,diagnostic_ls_css_xct,
         o_diagnostic_ls_css_first,o_diagnostic_ls_css_xct) %>%
  mutate(diagnostic_ls_css_first_cap=pmin(diagnostic_ls_css_first,245),
         o_diagnostic_ls_css_first_cap=pmin(o_diagnostic_ls_css_first,245)) %>%
  mutate(Lower_Age=factor(Lower)) %>%
  ggplot(aes(x=o_diagnostic_ls_css_first_cap,y=diagnostic_ls_css_first_cap))+
  geom_point(aes(color=Sex,shape=Lower_Age))+
  geom_abline(slope=1.0,intercept=0.0, lty="dashed")+
  geom_vline(xintercept=240,lty="dashed")+
  geom_hline(yintercept=240,lty="dashed")+
  coord_cartesian(x=c(0,250),y=c(0,250))+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="DT/LS Standard Survival",y="DT/LS Increased HR")+
  ggtitle("Contrast HR: CSO-Directed Tests")

ggsave(filename=sprintf("figs/%s_supplement_hazard_ratio_CSO_driven.pdf",figure_date_code),
       width=11,
       height=13)


compare_ppv_situation_df %>%
  filter(Lower>=50,Lower<=75,scenario=="old_3") %>%
  anti_join(forbidden_sex_cso %>% rename(predicted_CSO=label)) %>%
  anti_join(no_stage_modeled %>% rename(predicted_CSO=label)) %>%
  anti_join(no_shift_expected %>% rename(predicted_CSO=label)) %>%
  anti_join(boring_cases %>% rename(predicted_CSO=label)) %>% 
  select(Lower,Sex,Exposure,predicted_CSO,diagnostic_ls_css_first,diagnostic_ls_css_xct,
         o_diagnostic_ls_css_first,o_diagnostic_ls_css_xct) %>%
  mutate(diagnostic_ls_css_xct_cap=pmin(diagnostic_ls_css_xct,245),
         o_diagnostic_ls_css_xct_cap=pmin(o_diagnostic_ls_css_xct,245)) %>%
  mutate(Lower_Age=factor(Lower)) %>%
  ggplot(aes(x=o_diagnostic_ls_css_xct_cap,y=diagnostic_ls_css_xct_cap))+
  geom_point(aes(color=Sex,shape=Lower_Age))+
  geom_abline(slope=1.0,intercept=0.0, lty="dashed")+
  geom_vline(xintercept=240,lty="dashed")+
  geom_hline(yintercept=240,lty="dashed")+
  coord_cartesian(x=c(0,250),y=c(0,250))+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="DT/LS Standard Survival",y="DT/LS Increased HR")+
  ggtitle("Contrast HR: post-CSO-Directed Tests")

  
ggsave(filename=sprintf("figs/%s_supplement_hazard_ratio_post_CSO_driven.pdf",figure_date_code),
       width=11,
       height=13)

  