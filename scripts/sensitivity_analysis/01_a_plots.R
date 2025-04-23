#do some plots with uncertainty added

#figure 2

#sex contrast: female vs male
quantile_figure_two_data<-fixed_quantile_ppv_iter %>% 
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,
         overall_ppv,quant) %>%
  mutate(across(contains("ppv"),function(x){round(x*100)})) %>% 
  mutate(across(contains("total"),round)) %>% 
  mutate(across(contains("diagnostic"),function(x){round(x,digits=1)})) %>%
  filter(Lower==65,Exposure=="Any",scenario=="old_3") %>%
  left_join(pretty_cso_labels %>% rename(predicted_CSO=internal_CSO)) %>%
  pivot_wider(values_from=overall_ppv,names_prefix="q_",names_from=quant)

quantile_figure_two_data %>%
  ggplot(aes(x=pretty_CSO))+
  geom_hline(yintercept=7,lty="dashed")+
  # geom_line(aes(y=overall_ppv,group=1),lty="dashed")+
  geom_col(aes(y=q_0.5,fill=Sex,group=Sex),color="black",width=0.35,
           position=position_dodge(width=0.75))+
  geom_errorbar(aes(ymin=q_0.025,ymax=q_0.975, group=Sex),width=0.15,
                position=position_dodge(width=0.75))+
  geom_label(aes(y=q_0.5,label=q_0.5,group=Sex),
             position=position_dodge(width=0.75))+
  expand_limits(y=c(0,100))+
  #  facet_wrap(~Sex,ncol=2)+
  coord_flip()+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Predicted CSO",y="PPV for Any Cancer")+
  ggtitle("Overall PPV by Sex:\nAge 65\u00ad69 years, Any Smoking")

ggsave(filename=sprintf("figs/%s_supplement_quantile_figure_two.pdf",figure_date_code),
       width=11,
       height=13)

#data for age contrast
quantile_figure_three_data<-fixed_quantile_ppv_iter %>% 
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,
         overall_ppv,quant) %>%
  mutate(across(contains("ppv"),function(x){round(x*100)})) %>% 
  mutate(across(contains("total"),round)) %>% 
  mutate(across(contains("diagnostic"),function(x){round(x,digits=1)})) %>%
  mutate(Age=sprintf("%s\u00ad%s years",Lower,Upper)) %>%
  filter(Sex=="Female",Exposure=="Any",Lower %in% c(55,65,75),scenario=="old_3") %>%
  left_join(pretty_cso_labels %>% rename(predicted_CSO=internal_CSO)) %>%
  pivot_wider(values_from=overall_ppv,names_prefix="q_",names_from=quant)


quantile_figure_three_data %>%
  ggplot(aes(x=pretty_CSO))+
  geom_hline(yintercept=7,lty="dashed")+
  # geom_line(aes(y=overall_ppv,group=1),lty="dashed")+
  geom_col(aes(y=q_0.5,fill=Age,group=Age),color="black",width=0.35,
           position=position_dodge(width=0.85))+
  geom_errorbar(aes(ymin=q_0.025,ymax=q_0.975, group=Age),width=0.15,
                position=position_dodge(width=0.85))+
  geom_label(aes(y=q_0.5,label=q_0.5,group=Age),
             position=position_dodge(width=0.85))+
  expand_limits(y=c(0,100))+
  scale_fill_manual(values=c("darkgray","darkblue","blue"))+
  coord_flip()+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Predicted CSO",y="PPV for Any Cancer")+
  ggtitle("Overall PPV by Age:\nFemale, Any Smoking Exposure")


ggsave(filename=sprintf("figs/%s_supplement_quantile_figure_three.pdf",figure_date_code),
       width=11,
       height=13)


#smoking
quantile_figure_four_data<-fixed_quantile_ppv_iter %>% 
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,
         overall_ppv,quant) %>%
  mutate(across(contains("ppv"),function(x){round(x*100)})) %>% 
  mutate(across(contains("total"),round)) %>% 
  mutate(across(contains("diagnostic"),function(x){round(x,digits=1)})) %>%
  filter(Sex=="Female",Lower==65,Exposure!="Any",scenario=="old_3") %>%
  left_join(pretty_cso_labels %>% rename(predicted_CSO=internal_CSO))  %>%
  pivot_wider(values_from=overall_ppv,names_prefix="q_",names_from=quant)

quantile_figure_four_data %>%
  ggplot(aes(x=pretty_CSO))+
  geom_hline(yintercept=7,lty="dashed")+
  # geom_line(aes(y=overall_ppv,group=1),lty="dashed")+
  geom_col(aes(y=q_0.5,fill=Exposure,group=Exposure),color="black",width=0.35,
           position=position_dodge(width=0.85))+
  geom_errorbar(aes(ymin=q_0.025,ymax=q_0.975, group=Exposure),width=0.15,
                position=position_dodge(width=0.85))+
  geom_label(aes(y=q_0.5,label=q_0.5,group=Exposure),
             position=position_dodge(width=0.85))+
  expand_limits(y=c(0,100))+
  scale_fill_manual(name="Smoking History",values=rev(c("darkgray","darkblue","blue")))+
  #facet_wrap(~Exposure,ncol=3)+
  coord_flip()+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Predicted CSO",y="PPV for Any Cancer")+
  ggtitle("Overall PPV by Smoking:\nFemale, Age 65\u00ad69 years")


ggsave(filename=sprintf("figs/%s_supplement_quantile_figure_four.pdf",figure_date_code),
       width=11,
       height=13)

quantile_figure_five_data<-fixed_quantile_ppv_iter  %>% #how many go to non-site-specific or other testing
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,
         overall_ppv,ppv_first,ppv_xct,quant) %>%
  mutate(across(contains("ppv"),function(x){round(x*100)})) %>% 
  mutate(across(contains("total"),round)) %>% 
  mutate(across(contains("diagnostic"),function(x){round(x,digits=1)})) %>%
  filter(Sex=="Female",Lower==65,Exposure=="Any",scenario=="old_3") %>%
  pivot_longer(contains("ppv"),names_to="vtype",values_to="PPV") %>%
  left_join(pretty_cso_labels %>% rename(predicted_CSO=internal_CSO)) %>%
  pivot_wider(values_from=PPV,names_prefix="q_",names_from=quant)

quantile_figure_five_data %>%
  ggplot(aes(x=pretty_CSO))+
  geom_hline(yintercept=7,lty="dashed")+
  # geom_line(aes(y=PPV,group=1),lty="dashed")+
  geom_col(aes(y=q_0.5,fill=vtype,group=vtype),width=0.35,color="black",
           position=position_dodge(width=0.75))+
  geom_errorbar(aes(ymin=q_0.025,ymax=q_0.975, group=vtype),width=0.15,
                position=position_dodge(width=0.85))+
  geom_label(aes(y=q_0.5,label=q_0.5,group=vtype),
             position=position_dodge(width=0.75))+
  expand_limits(y=c(0,100))+
  scale_fill_manual(name="Post-Step",values=(c("darkgray","darkblue","blue")),labels=diagnostic_labels)+
  #facet_wrap(~type,ncol=3,labeller=as_labeller(diagnostic_labels))+
  coord_flip()+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Predicted CSO",y="PPV")+
  ggtitle("PPV by Diagnostic Step:\nFemale, Age 65\u00ad69 years, Any Smoking")


ggsave(filename=sprintf("figs/%s_supplement_quantile_figure_five.pdf",figure_date_code),
       width=11,
       height=13)


#lives saved using css
quantile_figure_six_data<-fixed_quantile_ppv_iter %>% #how many go to non-site-specific or other testing
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,diagnostic_ls_css_first,diagnostic_ls_css_xct,quant) %>%
  mutate(across(contains("ppv"),function(x){round(x*100)})) %>% 
  mutate(across(contains("total"),round)) %>% 
  mutate(across(contains("diagnostic"),function(x){round(x,digits=0)})) %>%
  filter(Sex=="Female",Lower==65,Exposure=="Any",scenario=="old_3") %>%
  pivot_longer(contains("diagnostic"),names_to="vtype",values_to="LS") %>%
  mutate(LS=case_when(is.infinite(LS) ~ NA,
                      TRUE ~ LS)) %>%
  left_join(pretty_cso_labels %>% rename(predicted_CSO=internal_CSO)) %>%
  pivot_wider(values_from=LS,names_prefix="q_",names_from=quant)

quantile_figure_six_data %>%
  ggplot(aes(x=pretty_CSO))+
  geom_hline(yintercept=240,lty="dashed",color="red")+
  #geom_line(aes(y=LS,group=1),lty="dashed")+
  geom_col(aes(y=q_0.5,group=vtype,fill=vtype),color="black",width=0.35,
           position=position_dodge(width=0.75))+
  geom_errorbar(aes(ymin=q_0.025,ymax=q_0.975, group=vtype),width=0.15,
                position=position_dodge(width=0.85))+
  geom_label(aes(y=q_0.5,label=q_0.5,group=vtype),
             position=position_dodge(width=0.75))+
  expand_limits(y=c(0,100))+
  scale_fill_manual(name="Test Type",values=(c("darkgray","darkblue","blue")),labels=ls_labels)+
  #  facet_wrap(~type,ncol=3,labeller=as_labeller(ls_labels))+
  coord_flip(ylim=c(0,240))+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Predicted CSO",y="Diagnostic Tests Per Life Saved")+
  ggtitle("Diagnostic Tests Per Life Saved:\nFemale, Age 65\u00ad69, Any Smoking")


ggsave(filename=sprintf("figs/%s_supplement_quantile_figure_six.pdf",figure_date_code),
       width=11,
       height=13)

