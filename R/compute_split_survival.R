#utility function: given that we are splitting survival into cfdna+ and cfdna-
# so that weighted average (detection rate) is the original survival
# and that we have a given hazard rate between the two curves

# find the survival estimate given cfdna status
# returns the survival estimate for cfdna negative
# survival estimate for cfdna positive is  then that estimate^ hazard ratio
find_cfdna_status_survival<-function(o_surv,haz_ratio,det_rate){ 
  tmp_fcn<-function(t_surv){
    (t_surv^haz_ratio*det_rate + t_surv*(1-det_rate)-o_surv)^2
  }
  par=optimize(tmp_fcn,interval=c(0,1))
  par$minimum
}