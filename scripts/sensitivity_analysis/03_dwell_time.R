#dwell time sensitivity analysis
#ppv
#LS

  
#summary ppv
ppv_situation_df %>% 
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,total_cso_driven,
         overall_ppv,ppv_first,ppv_xct,diagnostic_ls_css_first,diagnostic_ls_css_xct) %>%
  mutate(across(contains("ppv"),function(x){round(x*100)})) %>% 
  mutate(across(contains("total"),round)) %>% 
  mutate(across(contains("diagnostic"),function(x){round(x,digits=1)})) %>%
  mutate(Age=paste(Lower,Upper,sep="-")) %>%
  filter(Sex=="Female",Exposure=="Any",Lower==65,scenario %in% c("old_3","old_4")) %>%
  left_join(pretty_cso_labels %>% rename(predicted_CSO=internal_CSO)) %>%
  ggplot(aes(x=pretty_CSO))+
  geom_hline(yintercept=7,lty="dashed")+
  # geom_line(aes(y=overall_ppv,group=1),lty="dashed")+
  geom_col(aes(y=overall_ppv,fill=scenario,group=scenario),color="black",width=0.35,
           position=position_dodge(width=0.75))+
  geom_label(aes(y=overall_ppv,label=overall_ppv,group=scenario),
             position=position_dodge(width=0.75))+
  expand_limits(y=c(0,100))+
  scale_fill_manual(values=c("darkgray","darkblue"),labels=c("old_3"="Fast","old_4"="Fast Aggressive"))+
  coord_flip()+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Predicted CSO",y="PPV for Any Cancer")+
  ggtitle("Overall PPV by Dwell Time:\nFemale, Any Smoking Exposure, 65-69")

ggsave(filename=sprintf("figs/%s_supplement_dwell_overall_ppv.pdf",figure_date_code),
       width=11,
       height=13)

#summary lives_saved
ppv_situation_df %>% 
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,total_cso_driven,
         overall_ppv,ppv_first,ppv_xct,diagnostic_ls_css_first,diagnostic_ls_css_xct) %>%
  mutate(across(contains("ppv"),function(x){round(x*100)})) %>% 
  mutate(across(contains("total"),round)) %>% 
  mutate(across(contains("diagnostic"),function(x){round(x,digits=1)})) %>%
  mutate(Age=paste(Lower,Upper,sep="-")) %>%
  filter(Sex=="Female",Exposure=="Any",Lower==65,scenario %in% c("old_3","old_4")) %>%
  left_join(pretty_cso_labels %>% rename(predicted_CSO=internal_CSO)) %>%
  mutate(diagnostic_ls_css_first=case_when(is.infinite(diagnostic_ls_css_first) ~ NA,
                      TRUE ~ diagnostic_ls_css_first)) %>%
  ggplot(aes(x=pretty_CSO))+
  geom_hline(yintercept=240,lty="dashed")+
  # geom_line(aes(y=overall_ppv,group=1),lty="dashed")+
  geom_col(aes(y=diagnostic_ls_css_first,fill=scenario,group=scenario),color="black",width=0.35,
           position=position_dodge(width=0.75))+
  geom_label(aes(y=diagnostic_ls_css_first,label=round(diagnostic_ls_css_first,digits=0),group=scenario),
             position=position_dodge(width=0.75))+
  expand_limits(y=c(0,100))+
  scale_fill_manual(values=c("darkgray","darkblue"),labels=c("old_3"="Fast","old_4"="Fast Aggressive"))+
  coord_flip(ylim=c(0,240))+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Predicted CSO",y="DT/LS for CSO-Directed")+
  ggtitle("DT/LS for CSO-Directed by Dwell Time:\nFemale, Any Smoking Exposure, 65-69")

#dwell time affects both PPV and LS/diagnostic
#because of alterations to episode sensitivity due to changes in interval cancers

ggsave(filename=sprintf("figs/%s_supplement_dwell_ls_cso_driven.pdf",figure_date_code),
       width=11,
       height=13)
