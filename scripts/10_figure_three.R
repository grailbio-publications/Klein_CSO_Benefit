#figure 3

#data for age contrast
figure_three_data<-ppv_situation_df %>% 
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,total_cso_driven,
         overall_ppv,ppv_first,ppv_xct,diagnostic_ls_css_first,diagnostic_ls_css_xct) %>%
  mutate(across(contains("ppv"),function(x){round(x*100)})) %>% 
  mutate(across(contains("total"),round)) %>% 
  mutate(across(contains("diagnostic"),function(x){round(x,digits=1)})) %>%
  mutate(Age=sprintf("%s\u00ad%s years",Lower,Upper)) %>%
  filter(Sex=="Female",Exposure=="Any",Lower %in% c(55,65,75),scenario=="old_3") %>%
  left_join(pretty_cso_labels %>% rename(predicted_CSO=internal_CSO)) 

figure_three_data %>%
  ggplot(aes(x=pretty_CSO))+
  geom_hline(yintercept=7,lty="dashed")+
  # geom_line(aes(y=overall_ppv,group=1),lty="dashed")+
  geom_col(aes(y=overall_ppv,fill=Age,group=Age),color="black",width=0.35,
           position=position_dodge(width=0.85))+
  geom_label(aes(y=overall_ppv,label=overall_ppv,group=Age),
             position=position_dodge(width=0.85))+
  expand_limits(y=c(0,100))+
  scale_fill_manual(values=c("darkgray","darkblue","blue"))+
  coord_flip()+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top",
        plot.tag.location="plot")+
  labs(x="Predicted CSO",y="PPV for Any Cancer",tag="Figure 3")+
  ggtitle("Overall PPV by Age:\nFemale, Any Smoking Exposure")

ggsave(filename=sprintf("figs/%s_figure_three.pdf",figure_date_code),
       width=11,
       height=13)

ggsave(filename=sprintf("figs/%s_figure_three.tiff",figure_date_code),
       width=11,
       height=13)

figure_three_summary_stats<-figure_three_data %>% 
  filter(!is.nan(overall_ppv)) %>% #remove forbidden events
  mutate(Age=sprintf("%s-%s",Lower,Upper)) %>%
  group_by(Age) %>% 
  summarize(Sex=Sex[1],
            median_PPV=median(overall_ppv),
            weighted_mean_PPV=sum(total_cso_driven*overall_ppv)/sum(total_cso_driven),
            ppv_range=sprintf("%s-%s",min(overall_ppv),max(overall_ppv)),
            ppv_CSO=sprintf("(%s,%s)",pretty_CSO[which.min(overall_ppv)],pretty_CSO[which.max(overall_ppv)]),
            num_threshold=sprintf("%s/%s",sum(overall_ppv>7),length(overall_ppv))) %>%
  ungroup()