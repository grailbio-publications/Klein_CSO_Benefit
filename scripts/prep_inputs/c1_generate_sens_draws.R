# generate multiple draws for posterior uncertainty
# 

initial_draw_sens<-read_tsv(sprintf("generated_data/%s_ccga3_iso_sens.tsv",input_date_code))

nsample<-500 #enough to get some CI
set.seed(seed=my_seed) #make sure we can reproduce this

#raw redrawn data: how observations could have gone
#jeffrey prior on beta-binomial
draw_data_sens<-initial_draw_sens %>%
  cross_join(tibble(iter=1:nsample)) %>%
  mutate(redraw_sens=rbeta(length(c),c+0.5,n-c+0.5)) %>%  
  mutate(c_star=redraw_sens*n) %>%
  select(Cancer,Stage,c=c_star,n,iter) #don't sample because doing an additional binomial draw causes discrete artifacts

draw_data_stageable<-draw_data_sens %>%
  filter(!Cancer %in% special_set) %>%
  filter(Stage!="NotStaged") 

draw_data_notstaged<-draw_data_sens %>%
  filter(Cancer %in% special_set | (Cancer=="[OTHER]" & Stage=="NotStaged")) %>%
  group_by(Cancer,iter) %>%
  summarize(Stage="NotStaged", c=sum(c),n=sum(n)) %>%
  ungroup() 

#do the isotone regression to generate non-decreasing sensitivity estimates
draw_iso_stageable<-draw_data_stageable %>%
  mutate(sensitivity=case_when(n>0 ~ c/n,
                               TRUE ~ 0.0)) %>%
  group_by(Cancer,iter) %>%
  mutate(original_sens = sensitivity,
         sens=isotone_fix(sensitivity,Stage,n)) %>%
  ungroup() 

draw_iso_notstaged<-draw_data_notstaged %>%
  mutate(sensitivity=case_when(n>0 ~ c/n,
                               TRUE ~ 0.0)) %>%
  group_by(Cancer,iter) %>%
  mutate(original_sens = sensitivity,
         sens=sensitivity) %>%
  ungroup() 

draw_data_iso_sens<-bind_rows(draw_iso_stageable,draw_iso_notstaged) %>%
  select(Cancer,Stage,c,n,original_sens,sens,iter)

#store for later use in sensitivity analysis
write_tsv(draw_data_iso_sens,sprintf("generated_data/%s_draw_data_iso_sens.tsv",date_code))
