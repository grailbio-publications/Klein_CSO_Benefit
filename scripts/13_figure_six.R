#lives saved using css
figure_six_data<-ppv_situation_df %>% #how many go to non-site-specific or other testing
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,total_cso_driven,total_ncso_driven,
         overall_ppv,ppv_first,ppv_xct,diagnostic_ls_css_first,diagnostic_ls_css_xct) %>%
  mutate(across(contains("ppv"),function(x){round(x*100)})) %>% 
  mutate(across(contains("total"),round)) %>% 
  mutate(across(contains("diagnostic"),function(x){round(x,digits=0)})) %>%
  filter(Sex=="Female",Lower==65,Exposure=="Any",scenario=="old_3") %>%
  select(Lower,Upper,Sex,Exposure,predicted_CSO,total_cso_driven, total_ncso_driven, diagnostic_ls_css_first,diagnostic_ls_css_xct) %>%
  pivot_longer(contains("diagnostic"),names_to="type",values_to="LS") %>%
  mutate(LS=case_when(is.infinite(LS) ~ NA,
                      TRUE ~ LS)) %>%
  left_join(pretty_cso_labels %>% rename(predicted_CSO=internal_CSO)) 

figure_six_data %>%
  ggplot(aes(x=pretty_CSO))+
  geom_hline(yintercept=240,lty="dashed",color="red")+
  #geom_line(aes(y=LS,group=1),lty="dashed")+
  geom_col(aes(y=LS,group=type,fill=type),color="black",width=0.35,
           position=position_dodge(width=0.75))+
  geom_label(aes(y=LS,label=LS,group=type),
             position=position_dodge(width=0.75))+
  expand_limits(y=c(0,100))+
  scale_fill_manual(values=(c("darkgray","darkblue","blue")),labels=ls_labels)+
  #  facet_wrap(~type,ncol=3,labeller=as_labeller(ls_labels))+
  coord_flip(ylim=c(0,240))+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top",
        plot.tag.location="plot")+
  labs(x="Predicted CSO",y="Diagnostic Tests Per Life Saved",tag="Figure 6")+
  ggtitle("Diagnostic Tests Per Life Saved:\nFemale, Age 65\u00ad69, Any Smoking")

ggsave(filename=sprintf("figs/%s_figure_six.pdf",figure_date_code),
       width=13,
       height=13)

ggsave(filename=sprintf("figs/%s_figure_six.tiff",figure_date_code),
       width=13,
       height=13)

figure_six_summary_stats<-figure_six_data %>% 
  filter(!is.nan(LS)) %>% #remove forbidden events
  filter(pretty_CSO!="Myeloid Neoplasm",pretty_CSO!="Thyroid") %>% #remove not useful because no sensitivity
  filter(pretty_CSO!="Plasma Cell Neoplasm",pretty_CSO!="Melanoma") %>% #not useful because no modelable shift
  mutate(total_match=case_when(type=="diagnostic_ls_css_xct" ~ total_ncso_driven,
                               TRUE ~ total_cso_driven)) %>% 
  mutate(Age=sprintf("%s-%s",Lower,Upper)) %>%
  group_by(type) %>% 
  summarize(Sex=Sex[1],
            Age=Age[1],
            median_TN=median(LS),
            weighted_mean_needed=sum(total_match*LS)/sum(total_match),
            TN_range=sprintf("%s-%s",min(LS),max(LS)),
            TN_CSO=sprintf("(%s,%s)",pretty_CSO[which.min(LS)],pretty_CSO[which.max(LS)]),
            num_threshold_max=sprintf("%s/%s",sum(LS<240),length(LS)),
            num_threshold_20=sprintf("%s/%s",sum(LS<20),length(LS)),
            num_threshold_60=sprintf("%s/%s",sum(LS<60),length(LS))) %>%
  ungroup() %>%
  mutate(Exclusions="Myeloid,Thyroid,Plasma,Melanoma")