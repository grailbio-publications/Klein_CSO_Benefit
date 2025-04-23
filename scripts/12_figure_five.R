
figure_five_data<-ppv_situation_df  %>% #how many go to non-site-specific or other testing
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,total_cso_driven,total_ncso_driven,
         overall_ppv,ppv_first,ppv_xct,diagnostic_ls_css_first,diagnostic_ls_css_xct) %>%
  mutate(across(contains("ppv"),function(x){round(x*100)})) %>% 
  mutate(across(contains("total"),round)) %>% 
  mutate(across(contains("diagnostic"),function(x){round(x,digits=1)})) %>%
  filter(Sex=="Female",Lower==65,Exposure=="Any",scenario=="old_3") %>%
  select(Lower,Upper,Sex,Exposure, predicted_CSO,total_cso_driven, total_ncso_driven,overall_ppv,ppv_first,ppv_xct) %>%
  pivot_longer(contains("ppv"),names_to="type",values_to="PPV") %>%
  left_join(pretty_cso_labels %>% rename(predicted_CSO=internal_CSO)) 

figure_five_data %>%
  ggplot(aes(x=pretty_CSO))+
  geom_hline(yintercept=7,lty="dashed")+
  # geom_line(aes(y=PPV,group=1),lty="dashed")+
  geom_col(aes(y=PPV,fill=type,group=type),width=0.35,color="black",
           position=position_dodge(width=0.75))+
  geom_label(aes(y=PPV,label=PPV,group=type),
             position=position_dodge(width=0.75))+
  expand_limits(y=c(0,100))+
  scale_fill_manual(name="Post-Step",values=(c("darkgray","darkblue","blue")),labels=diagnostic_labels)+
  #facet_wrap(~type,ncol=3,labeller=as_labeller(diagnostic_labels))+
  coord_flip()+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top",
        plot.tag.location="plot")+
  labs(x="Predicted CSO",y="PPV",tag="Figure 5")+
  ggtitle("PPV by Diagnostic Step:\nFemale, Age 65\u00ad69 years, Any Smoking")

ggsave(filename=sprintf("figs/%s_figure_five.pdf",figure_date_code),
       width=11,
       height=13)

ggsave(filename=sprintf("figs/%s_figure_five.tiff",figure_date_code),
       width=11,
       height=13)

figure_five_summary_stats<-figure_five_data %>% 
  mutate(total_match=case_when(type=="ppv_xct" ~ total_ncso_driven,
                               TRUE ~ total_cso_driven)) %>% 
  filter(!is.nan(PPV)) %>% #remove forbidden events
  filter(pretty_CSO!="Myeloid Neoplasm",pretty_CSO!="Thyroid") %>% #remove not useful
  mutate(Age=sprintf("%s-%s",Lower,Upper)) %>%
  group_by(type) %>% 
  summarize(Sex=Sex[1],
            Age=Age[1],
            median_PPV=median(PPV),
            weighted_mean_PPV=sum(total_match*PPV)/sum(total_match),
            ppv_range=sprintf("%s-%s",min(PPV),max(PPV)),
            ppv_CSO=sprintf("(%s,%s)",pretty_CSO[which.min(PPV)],pretty_CSO[which.max(PPV)]),
            num_threshold=sprintf("%s/%s",sum(PPV>7),length(PPV))) %>%
  ungroup() %>%
  mutate(Exclusions="Myeloid,Thyroid")