#explanatory diagrams

#PPV remaining
#figure 1A
plot_conditional_diagram<-function(){
  
  xstart<-0.1
  ystart<-0.85
  delta_x<-0.09
  delta_y<-0.11
  
  #Population examples
  population_examples<-c("Male\n50\u00ad55 years\nNever Smoker","Female\n65\u00ad69 years\nAny Smoking","Female\n75\u00ad79 years\nCurrent Smoker")
  
  openplotmat(main="Conditional calculation of CSO Benefits",cex.main=3)
  
  textplain(mid=c(xstart,ystart+0.7*delta_y),lab="Source\nPopulation",cex=1.5)
  
  #put some dots down to imply extra boxes not shown
  for (ii in 1:14) {
    textplain(mid=c(xstart,ystart-ii*0.5*delta_y),lab=".",cex=2.5)
  }
  #plot shown examples
  textrect(mid=c(xstart,ystart),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab=population_examples[1])
  textrect(mid=c(xstart,ystart-3*delta_y),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab=population_examples[2])
  textrect(mid=c(xstart,ystart-7*delta_y),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab=population_examples[3])
  
  #second column
  cso_examples<-c("Anus","Lung","Uterus")
  
  cso_x<-xstart+2*delta_x
  
  textplain(mid=c(cso_x,ystart+0.7*delta_y),lab="Predicted CSO",cex=1.5)
  
  #throw some arrows
  for (ii in 0:7){
    straightarrow(from=c(xstart+delta_x*0.4,ystart-3*delta_y),
                  to=c(cso_x-delta_x*0.45,ystart-ii*delta_y),
                  arr.pos=1.0,
                  arr.type="triangle")
  }
  
  #put some dots down to imply extra boxes not shown
  for (ii in 1:14) {
    textplain(mid=c(cso_x,ystart-ii*0.5*delta_y),lab=".",cex=2.5)
  }
  #plot shown examples
  textrect(mid=c(cso_x,ystart),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab=cso_examples[1])
  textrect(mid=c(cso_x,ystart-3*delta_y),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab=cso_examples[2])
  textrect(mid=c(cso_x,ystart-7*delta_y),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab=cso_examples[3])
  
  #PPV for all status
  ppv_all_x<-xstart+4*delta_x
  textplain(mid=c(ppv_all_x,ystart+0.7*delta_y),lab="Initial\nPositives",cex=1.5)
  
  #throw some arrows
  for (ii in c(0,3,7)){
    straightarrow(from=c(cso_x+delta_x*0.4,ystart-ii*delta_y),
                  to=c(ppv_all_x-delta_x*0.45,ystart-ii*delta_y),
                  arr.pos=1.0,
                  arr.type="triangle")
  }
  
  #put some dots down to imply extra boxes not shown
  for (ii in 1:14) {
    textplain(mid=c(ppv_all_x,ystart-ii*0.5*delta_y),lab=".",cex=2.5)
  }
  #plot shown examples
  textellipse(mid=c(ppv_all_x,ystart),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab="PPV All")
  textellipse(mid=c(ppv_all_x,ystart-3*delta_y),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab="PPV All")
  textellipse(mid=c(ppv_all_x,ystart-7*delta_y),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab="PPV All")
  
  #"PPV" for cso-driven diagnostics
  ppv_first_x<-xstart+6*delta_x
  textplain(mid=c(ppv_first_x,ystart+0.7*delta_y),lab="Yield of\nCSO\u00adDirected Diagnostics",cex=1.5)
  
  #throw some arrows
  for (ii in c(0,3,7)){
    straightarrow(from=c(ppv_all_x+delta_x*0.4,ystart-ii*delta_y),
                  to=c(ppv_first_x-delta_x*0.45,ystart-ii*delta_y),
                  arr.pos=1.0,
                  arr.type="triangle")
  }
  
  #put some dots down to imply extra boxes not shown
  for (ii in 1:14) {
    textplain(mid=c(ppv_first_x,ystart-ii*0.5*delta_y),lab=".",cex=2.5)
  }
  #plot shown examples
  textellipse(mid=c(ppv_first_x,ystart),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab="\"PPV\" First")
  textellipse(mid=c(ppv_first_x,ystart-3*delta_y),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab="\"PPV\" First")
  textellipse(mid=c(ppv_first_x,ystart-7*delta_y),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab="\"PPV\" First")
  
  #"PPV" for remaining non-cso-driven diagnostics
  ppv_remain_x<-xstart+8*delta_x
  textplain(mid=c(ppv_remain_x,ystart+0.7*delta_y),lab="Yield of\nGeneral Diagnostics",cex=1.5)
  
  #throw some arrows
  for (ii in c(0,3,7)){
    straightarrow(from=c(ppv_first_x+delta_x*0.4,ystart-ii*delta_y),
                  to=c(ppv_remain_x-delta_x*0.45,ystart-ii*delta_y),
                  arr.pos=1.0,
                  arr.type="triangle")
  }
  
  #put some dots down to imply extra boxes not shown
  for (ii in 1:14) {
    textplain(mid=c(ppv_remain_x,ystart-ii*0.5*delta_y),lab=".",cex=2.5)
  }
  #plot shown examples
  textellipse(mid=c(ppv_remain_x,ystart),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab="PPV Remain")
  textellipse(mid=c(ppv_remain_x,ystart-3*delta_y),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab="PPV Remain")
  textellipse(mid=c(ppv_remain_x,ystart-7*delta_y),radx=delta_x*0.4,rady=delta_y*0.3, shadow.size=0.0,lwd=2,lab="PPV Remain")
  
  box()
}


#diagram of methodology

#
#figure 1B

plot_CSO_prediction_diagram<-function(){
  openplotmat(main="Modeling CSO",cex.main=3)
  
  
  #cso
  st_mid=c(0.15,0.85)
  rscale<-0.05
  delta=c(3*rscale,0.0)
  
  textplain(mid=st_mid+1.5*delta+2.5*c(0,rscale),lab="state transition model (per cancer)",cex=1.5)
  
  shadowbox(mid=st_mid+1.5*delta+0.02*c(1,1),radx=1.5*delta[1]+2*rscale, rady=1.5*rscale,shadow.size=0.0)
  shadowbox(mid=st_mid+1.5*delta+0.01*c(1,1),radx=1.5*delta[1]+2*rscale, rady=1.5*rscale,shadow.size=0.0)
  
  shadowbox(mid=st_mid+1.5*delta,radx=1.5*delta[1]+2*rscale, rady=1.5*rscale,shadow.size=0.0)
  
  for (ii in 1:3){
    straightarrow(from=st_mid+(ii-1)*delta+c(rscale,0),
                  to=st_mid+ii*delta-c(rscale,0),
                  arr.pos=0.5,
                  arr.type="triangle")
  }
  
  textdiamond(mid=st_mid,radx=rscale,lab="I")
  textdiamond(mid=st_mid+delta,radx=rscale,lab="II")
  textdiamond(mid=st_mid+2*delta,radx=rscale,lab="III")
  textdiamond(mid=st_mid+3*delta,radx=rscale,lab="IV")
  
  #run arrow to imply
  straightarrow(from=st_mid+2*delta-c(0,1.5*rscale),
                to=st_mid+2*delta-c(0,3.5*rscale),
                lwd=3,arr.pos=1,arr.type="triangle")
  
  num_col<-8
  ncolor<-num_col+1
  test_col<-rev(gray.colors(ncolor))
  
  #cancer types
  st_lower=st_mid-c(0,5*rscale)+1.5*delta-2*c(rscale,0)
  textplain(mid=st_lower+c(4.5*rscale,rscale),lab="cancers found by MCED + FP")
  #shadowbox(mid=st_lower+1.5*delta, radx=1.5*delta[1],rady=0.5*rscale,shadow.size=0.0)
  for (ii in 1:ncolor){
    my.col=ifelse(ii<9, test_col[10-ii],"white")
    my.text=ifelse(ii<9,"","FP")
    textrect(mid=st_lower+ii*c(rscale,0),radx=0.5*rscale,shadow.size=0.0,lab=my.text,box.col=my.col)
    if(ii==9)
      textrect(mid=st_lower+ii*c(rscale,0),radx=0.4*rscale,shadow.size=0.0,lab=my.text,box.col=my.col,lwd=2)
    
  }
  
  textplain(mid=st_lower-c(-4.5*rscale,1.0*rscale),lab="+",cex=3)
  
  
  #prior
  st_prior=st_lower-2*c(rscale,0)-2*c(0,rscale)
  
  textplain(mid=st_prior+c(0,0.25*rscale),lab="prior")
  textplain(mid=st_prior+c(-rscale,-4.5*rscale),lab="predicted CSO",srt=90)
  
  for (ii in 1:num_col){
    textrect(mid=st_prior-ii*c(0,rscale),radx=0.5*rscale,shadow.size=0.0,box.col=test_col[8-floor(ii/3)])
  }
  
  #confusion
  st_obs<-c(st_lower[1],st_prior[2])
  
  textplain(mid=st_obs+c(4.5*rscale,0.25*rscale),lab="observed confusion matrix")
  
  #below
  textplain(mid=st_obs+c(4.5*rscale,-(num_col+1.25)*rscale),lab="expected clinical CSO")
  textplain(mid=st_obs+c(0,-4.5*rscale),lab="observed CSO",srt=90)
  
  
  for (ii in 1:num_col){
    for(jj in 1:ncolor){
      my.col=ifelse(ii==jj, test_col[num_col],"white")
      my.col=ifelse(jj<ncolor,my.col,test_col[3])
      textrect(mid=st_obs+c(jj*rscale,-ii*rscale),radx=0.5*rscale,shadow.size=0.0,box.col=my.col)
    }
  }
  
  textplain(mid=st_obs-c(0.75*rscale,4.5*rscale),lab="+",cex=3)
  
  
  
  #predict
  st_predict=st_obs+12*c(rscale,0)
  textplain(mid=st_predict+c(0,0.25*rscale),lab="predicted CSO")
  for (ii in 1:num_col){
    my.col=test_col[ncolor-ii]
    textrect(mid=st_predict-ii*c(0,rscale),radx=0.5*rscale,shadow.size=0.0,box.col=test_col[9-ii])
  }
  
  textplain(mid=st_predict-c(1.5*rscale,4.5*rscale),lab="=",cex=3)
  
  box()
  
}
