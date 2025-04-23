#extra plots for sensitivity analysis

#raw data in all its glory
draft_ppv_iter %>%
       filter(Lower>=50,Lower<=75) %>%
       anti_join(forbidden_sex_cso %>% rename(predicted_CSO=label)) %>%
       anti_join(no_stage_modeled %>% rename(predicted_CSO=label)) %>%
       anti_join(no_shift_expected %>% rename(predicted_CSO=label)) %>%
       anti_join(boring_cases %>% rename(predicted_CSO=label)) %>% 
  mutate(diagnostic_ls_css_first_cap=pmin(diagnostic_ls_css_first,245)) %>%
  ggplot(aes(x=ppv_first,y=diagnostic_ls_css_first_cap))+
  geom_point(aes(color=Sex))+
  geom_vline(xintercept=0.07,lty="dashed")+
  geom_hline(yintercept=240,lty="dashed")+
  facet_wrap(facets=c("predicted_CSO"),ncol=4)+
  coord_cartesian(x=c(0,1),y=c(0,250))+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="PPV CSO-Directed",y="Diagnostic Tests Per Life Saved: CSO-Directed")+
  ggtitle("Stochastic uncertainty: CSO-Directed Tests")

ggsave(filename=sprintf("figs/%s_supplement_stochastic_CSO_driven.pdf",figure_date_code),
       width=11,
       height=13)

#raw data in all its glory
draft_ppv_iter %>%
  filter(Lower>=50,Lower<=75) %>%
  anti_join(forbidden_sex_cso %>% rename(predicted_CSO=label)) %>%
  anti_join(no_stage_modeled %>% rename(predicted_CSO=label)) %>%
  anti_join(no_shift_expected %>% rename(predicted_CSO=label)) %>%
  anti_join(boring_cases %>% rename(predicted_CSO=label)) %>% 
  mutate(diagnostic_ls_css_xct_cap=pmin(diagnostic_ls_css_xct,245)) %>%
  ggplot(aes(x=ppv_xct,y=diagnostic_ls_css_xct_cap))+
  geom_point(aes(color=Sex))+
  geom_vline(xintercept=0.07,lty="dashed")+
  geom_hline(yintercept=240,lty="dashed")+
  facet_wrap(facets=c("predicted_CSO"),ncol=4)+
  coord_cartesian(x=c(0,1),y=c(0,250))+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="PPV post-CSO-Directed",y="Diagnostic Tests Per Life Saved: post-CSO-Directed")+
  ggtitle("Stochastic uncertainty: post-CSO-Directed Tests")

ggsave(filename=sprintf("figs/%s_supplement_stochastic_post_CSO_driven.pdf",figure_date_code),
       width=11,
       height=13)



