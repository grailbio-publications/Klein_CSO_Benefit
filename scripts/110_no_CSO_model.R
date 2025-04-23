

#total tests
#measure expensive tests
#measure how far can you get with usual organ-directed tests
summarize_alternate_strategies_df<-ppv_situation_df %>% 
  group_by(Lower,Upper,Sex,Exposure,scenario) %>% 
  summarize(total_cso_driven=sum(total_cso_driven),
            total_lss_cso_driven=sum(ls_css_one,na.rm=TRUE),
            total_ncso_driven=sum(total_ncso_driven),
            remaining_cancer_risk=sum(false_CSO)/total_ncso_driven,
            total_lss_ncso_driven=sum(ls_css_two,na.rm=TRUE),
            total_no_CSO_driven=sum(true_CSO+false_CSO+fp)) %>%
  ungroup() %>%
  mutate(total_lss_no_CSO_driven=total_lss_cso_driven+total_lss_ncso_driven) %>%
  mutate(first_cso_ratio=total_lss_cso_driven/(total_lss_no_CSO_driven)) %>%
  mutate(breakeven_ratio=(total_no_CSO_driven-total_ncso_driven)/total_cso_driven)

write_tsv(summarize_alternate_strategies_df,sprintf("reports/%s_summarize_alternate_strategies.tsv",date_code))

#1 -failed strategy - just follow the CSO to minimize expensive testing
#gets 85% of lives-saved
#remaining risk of cancer unacceptably high
#varies by age because of FP rate
#need note limitation

#ratio of life-years saved by only doing one workup on everyone
summarize_alternate_strategies_df %>% 
  filter(scenario=="old_3") %>%
  ggplot(aes(x=Lower,y=first_cso_ratio))+
  geom_point(aes(shape=Exposure),size=3)+
  expand_limits(y=c(0,1))+
  facet_wrap(~Sex) +
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Lower Age",y="Fraction of Lives Saved With Only CSO-Directed Workups")+
  ggtitle("Only Using CSO-Directed Workups")

ggsave(sprintf("figs/%s_supplemental_fraction_lives_only_cso.pdf",figure_date_code),
       width=11,height=11)



#residual risk plot
summarize_alternate_strategies_df %>% 
  filter(scenario=="old_3") %>%
  ggplot(aes(x=Lower,y=remaining_cancer_risk))+
  geom_point(aes(shape=Exposure),size=3)+
  geom_hline(yintercept=0.07,lty="dashed")+
  facet_wrap(~Sex) +
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Lower Age",y="Residual Risk of Cancer After CSO-Directed Workup")+
  ggtitle("Only Using CSO-Directed Workups")

ggsave(sprintf("figs/%s_supplemental_residual_risk_only_cso.pdf",figure_date_code),
       width=11,height=11)


#2 - failed strategy - only do expensive testing 'to save workups'
# gets all the lives saved
#but if cso-driven tests are on average 40% as expensive - time, cost, harms then we win
#again varies by age...

summarize_alternate_strategies_df %>%
  filter(scenario=="old_3") %>%
  mutate(total_strategy_driven=total_cso_driven+total_ncso_driven) %>% 
  pivot_longer(c("total_cso_driven","total_ncso_driven")) %>%
  mutate(total_no_CSO_driven=case_when(name=="total_cso_driven" ~ total_no_CSO_driven,
                                       TRUE ~ 0.0)) %>%
  ggplot(aes(x=Lower))+
  geom_col(aes(y=value,fill=name),position="stack",color="black")+
  geom_col(aes(y=total_no_CSO_driven,fill="No CSO"),width=2,color="black")+
  scale_fill_manual(name="Type of Test",values=list("No CSO"="red",
                                                    "total_cso_driven"="green",
                                                    "total_ncso_driven"="blue"),
                    labels=c("No CSO"="No CSO Strategy",
                             "total_cso_driven"="CSO-Directed",
                             "total_ncso_driven"="NCSO-Directed"))+
  facet_grid(Exposure~Sex)+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Lower Age",y="Tests Under Different Strategies")+
  ggtitle("Strategies Depend on Test Tradeoffs\nNot Total Tests")


ggsave(sprintf("figs/%s_supplemental_alternate_strategies_number_tests.pdf",figure_date_code),
       width=11,height=11)


summarize_alternate_strategies_df %>% 
  filter(scenario=="old_3") %>%
  ggplot(aes(x=Lower,y=breakeven_ratio))+
  geom_point(aes(shape=Exposure),size=3)+
  expand_limits(y=c(0,1))+
  facet_wrap(~Sex) +
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Lower Age",y="Average Breakeven Ratio To Favor CSO-Directed")+
  ggtitle("Testing Ratios Determine Tradeoffs")

ggsave(sprintf("figs/%s_supplemental_alternate_strategies_breakeven_ratio.pdf",figure_date_code),
       width=11,height=11)

