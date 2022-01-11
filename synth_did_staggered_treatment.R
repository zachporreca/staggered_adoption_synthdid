staggered_synth_DID <- 
  function(data, initial_treat_var, untreated, outcome_var,   unit, treatment_var, time_var){
    attach(data)
    treated_periods=unique(data[which(data[,initial_treat_var]>0), initial_treat_var])
    treated_periods=treated_periods[which(treated_periods>min(data[,time_var])+1)]
    result_matrix=matrix(ncol=6, nrow=length(treated_periods))
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
    }
    result_matrix=as.data.frame(result_matrix)
    colnames(result_matrix)[1]="treatment_period"
    colnames(result_matrix)[2]="tau_hat"
    colnames(result_matrix)[3]="standard_error"
    colnames(result_matrix)[4]="lower_95_CI"
    colnames(result_matrix)[5]="upper_95_CI"
    result_matrix$weight=result_matrix[,6]/sum(result_matrix[,6])
    result_matrix[,6]=NULL
    
    
    #FOR NOW SE WILL BE CALCULATED THROUGH SIMILIAR WEIGHTING- LITERATURE NEEDS TO RESOLVE THIS. MTHOD HERE LIKELY WRONG
    
    overall_estimator=sum(result_matrix[,6]*result_matrix[,2])
    overall_se=sum(result_matrix[,6]*result_matrix[,3])
    lower_95_CI=overall_estimator-(1.96*overall_se)
    upper_95_CI=overall_estimator+(1.96*overall_se)
    print(paste0("ATT= ",overall_estimator))
    print(paste0("SE= ", overall_se))
    print(paste0("95% CI: (",lower_95_CI,", ",upper_95_CI,")"))
  }