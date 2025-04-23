library(diagram)

make_unrolled_flow_diagram<-function(){
  basic_stage<-(c(1,2,4,6,8)-0.5)/10  #NC,I,II,III,IV
  center_level<-4.5/9
  #predetect - diamond
  predetect_offset<-basic_stage+0.05
  predetect_level<-center_level+0.15
  #intercepted - rect
  intercept_x_pos=predetect_offset+0.05
  intercept_y_level=predetect_level+0.15
  
  #preclinical - circle
  preclinical_level<-center_level-0.05
  preclinical_x_pos<-basic_stage
  #clinical - rect
  clinical_level<-preclinical_level-0.2
  clinical_x_pos<-preclinical_x_pos+0.05
  
  diagram_map_one<-tibble(x=basic_stage,stage=c("NC","I","II","III","IV"),status="basic")
  diagram_map_box<-diagram_map_one %>%
    left_join(tibble(level=rev(1:5),y=seq(2.5/9,6.5/9,length=5)),by=character()) %>%
    mutate(zeta=match(stage,c("NC","I","II","III","IV"))-1) %>%
    mutate(status=case_when(stage=="NC" ~ "basic",
                            level>zeta ~ "preclinical",
                            TRUE ~ "predetect")) %>%
    filter(stage!="NC" | level==3) %>%
    bind_rows(diagram_map_one %>% mutate(x=x+0.10,y=2.5/9-0.12,status="clinical")) %>%
    bind_rows(diagram_map_one %>% mutate(x=x+0.10,y=6.5/9+0.12,status="intercept")) %>%
    left_join(tribble(~status,~box, ~box_col,
                      "basic","circle", "white",
                      "predetect","diamond", "white",
                      "preclinical","circle", "white",
                      "intercept","rect", "plum",
                      "clinical","rect", "gray")) 
  
  
  
  
  diagram_map_box<-diagram_map_box %>%
    filter(!(status=="basic" & stage!="NC"),
           !(status!="basic" & stage=="NC"))
  
  diagram_map_box<-diagram_map_box %>%
    mutate(box_size=0.035) %>%
    mutate(box_size = box_size*(1+0.2*(box=="diamond")))
  
  predecessor_df<-tribble(~stage,~prequel,
                          "NC","X",
                          "I","NC",
                          "I","I",
                          "II","I",
                          "II","II",
                          "III","II",
                          "III","III",
                          "IV","III",
                          "IV","IV")
  
  prestatus_df<-tribble(~status,~prestatus,
                        "basic","X",
                        "predetect","preclinical",
                        "predetect","basic",
                        "predetect","predetect",
                        "preclinical","preclinical",
                        "preclinical","basic",
                        "intercept","predetect",
                        "clinical","preclinical",
                        "clinical","predetect")
  
  
  #make arrows
  diagram_map_arrow <- diagram_map_box %>%
    left_join(predecessor_df) %>%
    left_join(prestatus_df) %>%
    filter(!(prequel=="NC" & prestatus!="basic")) %>%
    filter(!(prequel==stage & prestatus==status)) %>%
    filter(!(prequel!=stage & status %in% c("clinical","intercept"))) %>%
    filter(!(prequel!="NC" & prestatus=="basic")) %>%
    filter(!(prequel==stage & status %in% c("preclinical","predetect"))) %>%
    filter(prequel!="X",prestatus!="X") %>%
    left_join(diagram_map_box %>%
                select(x_end=x,prequel=stage,y_end=y,prestatus=status)) %>% 
    mutate(contradiction=case_when(prestatus %in% c("preclinical","predetect") &
                                     status %in% c("preclinical","predetect") &
                                     y_end!=y ~ "violation",
                                   TRUE ~ "good")) %>%
    filter(contradiction=="good")
  
  
  #box_size=0.05
  openplotmat(main = "")
  
  
  for (ii in 1:length(diagram_map_arrow$x)){
    local_lty=2-(diagram_map_arrow$status[ii] %in% c("clinical","intercept"))
    straightarrow(from=c(diagram_map_arrow$x_end[ii],diagram_map_arrow$y_end[ii]),
                  to=c(diagram_map_arrow$x[ii],diagram_map_arrow$y[ii]),
                  lty=3-local_lty)
  }
  
  for (ii in 1:length(diagram_map_box$x)){
    shadowbox(box.type=diagram_map_box$box[ii],
              mid=c(diagram_map_box$x[ii],diagram_map_box$y[ii]),
              radx=diagram_map_box$box_size[ii],
              shadow.size=0.0,
              box.col=diagram_map_box$box_col[ii],
              lwd=2)
  }
  
  for (ii in 1:length(diagram_map_box$x)){
    textplain(
      mid=c(diagram_map_box$x[ii],diagram_map_box$y[ii]),
      height=diagram_map_box$box_size[ii],
      lab=diagram_map_box$stage[ii],
      cex=1.5)
  }
  
  #infer some decor
  diagram_map_top<-diagram_map_box %>% 
    group_by(stage) %>% 
    summarize(xmin=min(x-box_size),
              xmax=max(x+box_size),
              ymax=max(y+box_size),
              xmean=mean(x)) %>% 
    ungroup() %>%
    filter(grepl("I",stage))
  
  for (ii in 1:length(diagram_map_top$xmax)){
    segments(diagram_map_top$xmin[ii],diagram_map_top$ymax[ii]+0.01,
             diagram_map_top$xmax[ii],diagram_map_top$ymax[ii]+0.01,lwd=2)
    text(diagram_map_top$xmean,diagram_map_top$ymax+0.035,diagram_map_top$stage,cex=1.5)
  }
  
  #decor
  diagram_map_side<-diagram_map_box %>%
    mutate(cfdna=case_when(status=="intercept" ~ "cfdna",
                           status=="predetect" ~ "cfdna",
                           status=="clinical" ~ "soc",
                           status=="preclinical" ~ "soc",
                           TRUE ~ "basic")) %>%
    filter(cfdna!="basic",!grepl("pre",status)) %>%
    group_by(cfdna) %>%
    summarize(ymean=mean(y),
              xmean=mean(x)) %>%
    ungroup()
  
  shadowbox(box.type="diamond",mid=c(0.04,diagram_map_side$ymean[1]),radx=0.02,shadow.size=0.0)
  text(0.12,diagram_map_side$ymean[1],"detectable\nby MCED")
  shadowbox(box.type="circle",mid=c(0.04,diagram_map_side$ymean[2]-0.02),radx=0.02,shadow.size=0.0)
  text(0.12,diagram_map_side$ymean[2]-0.02,"not yet\ndetectable\nby MCED")
  
  text(diagram_map_side$xmean[1],0.95,"Intercepted by MCED",cex=1.5)
  text(diagram_map_side$xmean[1],0.05,"Found by usual care",cex=1.5)
  
  box()
  
}

make_compact_flow_diagram<-function(){
  basic_stage<-(c(1,2,4,6,8)-0.5)/9  #NC,I,II,III,IV
  center_level<-4.5/9
  #predetect - diamond
  predetect_offset<-basic_stage+0.05
  predetect_level<-center_level+0.15
  #intercepted - rect
  intercept_x_pos=predetect_offset+0.05
  intercept_y_level=predetect_level+0.15
  
  #preclinical - circle
  preclinical_level<-center_level-0.05
  preclinical_x_pos<-basic_stage
  #clinical - rect
  clinical_level<-preclinical_level-0.2
  clinical_x_pos<-preclinical_x_pos+0.05
  
  diagram_map_one<-tibble(x=basic_stage,stage=c("NC","I","II","III","IV"),y=rep(center_level,5),status="basic")
  diagram_map_box<-diagram_map_one %>%
    bind_rows(diagram_map_one %>% mutate(x=x+0.05,y=y+0.10,status="predetect")) %>%
    bind_rows(diagram_map_one %>% mutate(x=x+0.1,y=y+0.3,status="intercept")) %>%
    bind_rows(diagram_map_one %>% mutate(x=x,y=y-0.10,status="preclinical")) %>%
    bind_rows(diagram_map_one %>% mutate(x=x+0.1,y=y-0.3,status="clinical")) %>%
    left_join(tribble(~status,~box, ~box_col,
                      "basic","circle", "white",
                      "predetect","diamond", "white",
                      "preclinical","circle", "white",
                      "intercept","rect", "plum",
                      "clinical","rect", "gray"))
  
  diagram_map_box<-diagram_map_box %>%
    filter(!(status=="basic" & stage!="NC"),
           !(status!="basic" & stage=="NC"))
  
  diagram_map_box<-diagram_map_box %>%
    mutate(box_size=0.045) %>%
    mutate(box_size = box_size*(1+0.2*(box=="diamond")))
  
  predecessor_df<-tribble(~stage,~prequel,
                          "NC","X",
                          "I","NC",
                          "I","I",
                          "II","I",
                          "II","II",
                          "III","II",
                          "III","III",
                          "IV","III",
                          "IV","IV")
  
  prestatus_df<-tribble(~status,~prestatus,
                        "basic","X",
                        "predetect","preclinical",
                        "predetect","basic",
                        "predetect","predetect",
                        "preclinical","preclinical",
                        "preclinical","basic",
                        "intercept","predetect",
                        "clinical","preclinical",
                        "clinical","predetect")
  
  
  #make arrows
  diagram_map_arrow <- diagram_map_box %>%
    left_join(predecessor_df) %>%
    left_join(prestatus_df) %>%
    filter(!(prequel=="NC" & prestatus!="basic")) %>%
    filter(!(prequel==stage & prestatus==status)) %>%
    filter(!(prequel!=stage & status %in% c("clinical","intercept"))) %>%
    filter(!(prequel!="NC" & prestatus=="basic")) %>%
    filter(!(prequel==stage & status %in% c("preclinical","predetect"))) %>%
    filter(prequel!="X",prestatus!="X") %>%
    left_join(diagram_map_box %>%
                select(x_end=x,prequel=stage,y_end=y,prestatus=status))
  
  
  #box_size=0.05
  openplotmat(main = "")
  
  #solid lines are directly observed, dashed are inferred
  for (ii in 1:length(diagram_map_arrow$x)){
    local_lty=2-(diagram_map_arrow$status[ii] %in% c("clinical","intercept"))
    straightarrow(from=c(diagram_map_arrow$x_end[ii],diagram_map_arrow$y_end[ii]),
                  to=c(diagram_map_arrow$x[ii],diagram_map_arrow$y[ii]),
                  lty=3-local_lty)
  }
  
  for (ii in 1:length(diagram_map_box$x)){
    shadowbox(box.type=diagram_map_box$box[ii],
              mid=c(diagram_map_box$x[ii],diagram_map_box$y[ii]),
              radx=diagram_map_box$box_size[ii],
              shadow.size=0.0,
              box.col=diagram_map_box$box_col[ii],
              lwd=2)
  }
  
  for (ii in 1:length(diagram_map_box$x)){
    textplain(
      mid=c(diagram_map_box$x[ii],diagram_map_box$y[ii]),
      height=diagram_map_box$box_size[ii],
      lab=diagram_map_box$stage[ii],
      cex=1.5)
  }
  
  #infer some decor
  diagram_map_top<-diagram_map_box %>% 
    group_by(stage) %>% 
    summarize(xmin=min(x-box_size),
              xmax=max(x+box_size),
              ymax=max(y+box_size),
              xmean=mean(x)) %>% 
    ungroup() %>%
    filter(grepl("I",stage))
  
  for (ii in 1:length(diagram_map_top$xmax)){
    segments(diagram_map_top$xmin[ii],diagram_map_top$ymax[ii]+0.01,
             diagram_map_top$xmax[ii],diagram_map_top$ymax[ii]+0.01,lwd=2)
    text(diagram_map_top$xmean,diagram_map_top$ymax+0.035,diagram_map_top$stage,cex=1.5)
  }
  
  #decor
  diagram_map_side<-diagram_map_box %>%
    mutate(cfdna=case_when(status=="intercept" ~ "cfdna",
                           status=="predetect" ~ "cfdna",
                           status=="clinical" ~ "soc",
                           status=="preclinical" ~ "soc",
                           TRUE ~ "basic")) %>%
    filter(cfdna!="basic") %>%
    group_by(cfdna) %>%
    summarize(ymean=mean(y),
              xmean=mean(x)) %>%
    ungroup()
  
  shadowbox(box.type="diamond",mid=c(0.04,diagram_map_side$ymean[1]),radx=0.02,shadow.size=0.0)
  text(0.12,diagram_map_side$ymean[1],"detectable\nby MCED")
  shadowbox(box.type="circle",mid=c(0.04,diagram_map_side$ymean[2]-0.02),radx=0.02,shadow.size=0.0)
  text(0.12,diagram_map_side$ymean[2]-0.02,"not yet\ndetectable\nby MCED")
  
  text(diagram_map_side$xmean[1],0.95,"Intercepted by MCED",cex=2)
  text(diagram_map_side$xmean[1],0.1,"Found by usual care",cex=2)
  
  box()
  
}
