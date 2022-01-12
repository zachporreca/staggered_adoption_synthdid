staggered_synth_DID_jack_se <- 
  function(data, initial_treat_var, untreated, outcome_var,   unit, treatment_var, time_var){
    attach(data)
#jacknifing leaving out observations one at a time
jackknife_vec=matrix(nrow=length(unique(data[,unit])), ncol=4)
jackknife_vec[,4]=unique(data[,unit])
timeNow <- Sys.time()
for (j in 1:nrow(jackknife_vec)){
  data_j=data[-c(which(data[,unit]==jackknife_vec[j,4])),]
  data_j[,treatment_var]=ifelse(data_j[,treatment_var]==untreated, 0, data_j[,treatment_var])
  untreated=0
  treated_periods=unique(data_j[which(data_j[,initial_treat_var]>0), initial_treat_var])
  treated_periods=treated_periods[which(treated_periods>min(data_j[,time_var])+1)]
  result_matrix=matrix(ncol=6, nrow=length(treated_periods))
  for (i in 1:length(treated_periods)){
    subbed=data_j[which(data_j[,initial_treat_var]==treated_periods[i]|data_j[,initial_treat_var]==untreated),]
    result_matrix[i,1]=treated_periods[i]
    subbed[,ncol(subbed)+1]=0
    colnames(subbed)[ncol(subbed)]="post_treat"
    subbed$post_treat=ifelse(subbed[,time_var]>=subbed[,initial_treat_var] & subbed[,initial_treat_var]!=0, 1, 0)
    setup=panel.matrices(subbed, unit=unit, time=time_var, 
                         outcome = outcome_var, treatment = "post_treat")
    tau_hat=synthdid_estimate(setup$Y, setup$N0, setup$T0)
    result_matrix[i,2]=as.numeric(tau_hat)
    result_matrix[i,3]=sqrt(vcov(tau_hat, method = "jackknife"))
    result_matrix[i,4]=result_matrix[i,2]-(1.96*result_matrix[i,3])
    result_matrix[i,5]=result_matrix[i,2]+(1.96*result_matrix[i,3])
    result_matrix[i,6]=nrow(subbed[which(subbed$post_treat==1),])
  }
  result_matrix=as.data.frame(result_matrix)
  colnames(result_matrix)[1]="treatment_period"
  colnames(result_matrix)[2]="tau_hat"
  colnames(result_matrix)[3]="standard_error"
  colnames(result_matrix)[4]="lower_95_CI"
  colnames(result_matrix)[5]="upper_95_CI"
  result_matrix$weight=result_matrix[,6]/sum(result_matrix[,6])
  result_matrix[,6]=NULL
  overall_estimator=sum(result_matrix[,6]*result_matrix[,2])
  jackknife_vec[j,1]=overall_estimator
  cat("\r", round(j*100/nrow(jackknife_vec), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
#getting main estimator from whole sample
data[,treatment_var]=ifelse(data[,treatment_var]==untreated, 0, data[,treatment_var])
untreated=0
treated_periods=unique(data[which(data[,initial_treat_var]>0), initial_treat_var])
treated_periods=treated_periods[which(treated_periods>min(data[,time_var])+1)]
result_matrix=matrix(ncol=6, nrow=length(treated_periods))
timeNow <- Sys.time()
for (i in 1:length(treated_periods)){
  subbed=data[which(data[,initial_treat_var]==treated_periods[i]|data[,initial_treat_var]==untreated),]
  result_matrix[i,1]=treated_periods[i]
  subbed[,ncol(subbed)+1]=0
  colnames(subbed)[ncol(subbed)]="post_treat"
  subbed$post_treat=ifelse(subbed[,time_var]>=subbed[,initial_treat_var] & subbed[,initial_treat_var]!=0, 1, 0)
  setup=panel.matrices(subbed, unit=unit, time=time_var, 
                       outcome = outcome_var, treatment = "post_treat")
  tau_hat=synthdid_estimate(setup$Y, setup$N0, setup$T0)
  result_matrix[i,2]=as.numeric(tau_hat)
  result_matrix[i,3]=sqrt(vcov(tau_hat, method = "jackknife"))
  result_matrix[i,4]=result_matrix[i,2]-(1.96*result_matrix[i,3])
  result_matrix[i,5]=result_matrix[i,2]+(1.96*result_matrix[i,3])
  result_matrix[i,6]=nrow(subbed[which(subbed$post_treat==1),])
  cat("\r", round(i*100/nrow(data), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
result_matrix=as.data.frame(result_matrix)
colnames(result_matrix)[1]="treatment_period"
colnames(result_matrix)[2]="tau_hat"
colnames(result_matrix)[3]="standard_error"
colnames(result_matrix)[4]="lower_95_CI"
colnames(result_matrix)[5]="upper_95_CI"
result_matrix$weight=result_matrix[,6]/sum(result_matrix[,6])
result_matrix[,6]=NULL
#computing jacknife se
overall_estimator=sum(result_matrix[,6]*result_matrix[,2])
jackknife_vec[,2]=jackknife_vec[,1]-overall_estimator
jackknife_vec[,3]=jackknife_vec[,2]^2
overall_se=sqrt((sum(jackknife_vec[,3]))*((nrow(data)-1)/nrow(data)))
overall_p=2*pt(q=sqrt(((overall_estimator/overall_se)^2)), df=(nrow(data)-length(unique(data[,time_var]))-length(unique(data[,unit]))), lower.tail=FALSE)
lower_95_CI=overall_estimator-(1.96*overall_se)
upper_95_CI=overall_estimator+(1.96*overall_se)
print(paste0("ATT= ",overall_estimator, ifelse(overall_p < .05 & overall_p > .01,"**",
                                               ifelse(overall_p < .1 & overall_p > .05,"*",
                                                      ifelse(overall_p < .01,"***","")))))
print(paste0("SE= ", overall_se))
print(paste0("p= ", overall_p))
print(paste0("95% CI: (",lower_95_CI,", ",upper_95_CI,")"))
print("???p<0.1; ??????p<0.05; ?????????p<0.01")
return(result_matrix)
}