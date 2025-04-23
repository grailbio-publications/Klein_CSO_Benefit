#try differently
ppv_situation_df %>% 
  select(Lower,Upper,Sex, Exposure,scenario,predicted_CSO,total_cso_driven,
         overall_ppv,ppv_first,ppv_xct,diagnostic_ls_css_first,diagnostic_ls_css_xct) %>%
  filter(Lower>=50,Lower<=75) %>%
  anti_join(forbidden_sex_cso %>% rename(predicted_CSO=label)) %>%
  anti_join(no_stage_modeled %>% rename(predicted_CSO=label)) %>%
  anti_join(no_shift_expected %>% rename(predicted_CSO=label)) %>%
  anti_join(boring_cases %>% rename(predicted_CSO=label)) %>% 
  mutate(first_margin_ppv=ppv_first>ppv_limit,
         xct_margin_ppv=ppv_xct>ppv_limit,
         first_css_margin=diagnostic_ls_css_first<margin_limit,
         xct_css_margin=diagnostic_ls_css_xct<margin_limit) %>% 
  filter(!xct_margin_ppv) %>%
  count(Lower,Upper,Exposure)

#verba; description
#basically only young never-smokers fail first PPV under this model (Lung, NET)
#many fails of xct_ppv at 7% but 'boring'
#no fails of first_ls - even if fail ppv still LS (may mean issues with PPV as measure)
#some fails of xct_ls in 50-year olds

ppv_situation_df %>%
  filter(Lower>=50,Lower<=75) %>%
  anti_join(forbidden_sex_cso %>% rename(predicted_CSO=label)) %>%
  anti_join(no_stage_modeled %>% rename(predicted_CSO=label)) %>%
  anti_join(no_shift_expected %>% rename(predicted_CSO=label)) %>%
  anti_join(boring_cases %>% rename(predicted_CSO=label)) %>% 
  mutate(diagnostic_ls_css_first_cap=pmin(diagnostic_ls_css_first,245)) %>%
  mutate(Lower_Age=factor(Lower)) %>%
  mutate(Dwell=case_when(scenario=="old_3" ~ "Fast",
                         scenario=="old_4" ~ "Fast Aggressive",
                         TRUE ~ "Other")) %>%
  ggplot(aes(x=ppv_first,y=diagnostic_ls_css_first_cap))+
  geom_point(aes(color=Sex,shape=Lower_Age))+
  geom_vline(xintercept=0.07,lty="dashed")+
  geom_hline(yintercept=240,lty="dashed")+
  facet_wrap(facets=c("Dwell"),ncol=4)+
  coord_cartesian(x=c(0,1),y=c(0,250))+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="PPV CSO-Directed",y="Diagnostic Tests Per Life Saved: CSO-Directed")+
  ggtitle("Dwell Time: CSO-Directed Tests")

ggsave(filename=sprintf("figs/%s_supplement_dwell_CSO_driven.pdf",figure_date_code),
       width=11,
       height=13)

ppv_situation_df %>%
  filter(Lower>=50,Lower<=75) %>%
  anti_join(forbidden_sex_cso %>% rename(predicted_CSO=label)) %>%
  anti_join(no_stage_modeled %>% rename(predicted_CSO=label)) %>%
  anti_join(no_shift_expected %>% rename(predicted_CSO=label)) %>%
  anti_join(boring_cases %>% rename(predicted_CSO=label)) %>% 
  mutate(diagnostic_ls_css_xct_cap=pmin(diagnostic_ls_css_xct,245)) %>%
  mutate(Lower_Age=factor(Lower)) %>%
  mutate(Dwell=case_when(scenario=="old_3" ~ "Fast",
                         scenario=="old_4" ~ "Fast Aggressive",
                         TRUE ~ "Other")) %>%
  ggplot(aes(x=ppv_xct,y=diagnostic_ls_css_xct_cap))+
  geom_point(aes(color=Sex,shape=Lower_Age))+
  geom_vline(xintercept=0.07,lty="dashed")+
  geom_hline(yintercept=240,lty="dashed")+
  facet_wrap(facets=c("Dwell"),ncol=4)+
  coord_cartesian(x=c(0,1),y=c(0,250))+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="PPV post-CSO-Directed",y="Diagnostic Tests Per Life Saved: post-CSO-Directed")+
  ggtitle("Dwell Time: post-CSO-Directed Tests")

ggsave(filename=sprintf("figs/%s_supplement_dwell_post_CSO_driven.pdf",figure_date_code),
       width=11,
       height=13)

