#' Staggered Synthetic Difference-in-Differences
#' @description Following the methods described in the appendix 
#'   of Arkhangelsky et al. (2021) and Porreca (2022)
#' 
#' At least two pre-periods where no units are treated are required.
#' @param data Data.frame
#' @param untreated Numeric. Value assigned in treatment_var column to untreated units
#' @param outcome_var Numeric. Column number for y variable
#' @param unit Numeric. Column number for variable that identifies each unit
#' @param treatment_var Numeric. Column number for 0/1 variable. = 1 if unit is 
#'   treated in that period
#' @param time_var Numeric. Column number for variable indicating time period. 
#' 
#' @return Data.frame containing estimates for each treatment_period. Containing
#'   point estimates, standard error, and confidence intervals.
#' 



staggered_synth_DID <- 
  function(data, untreated, outcome_var, unit, treatment_var, time_var){
    attach(data)
    
    #Creating initial treatment time variable
    state_initial_treat=matrix(nrow=length(unique(data[,unit])), ncol=2)
    state_initial_treat[,1]=unique(data[,unit])
    for (i in 1:nrow(state_initial_treat)){
      state_initial_treat[i,2]=min(data[which(data[,unit]==state_initial_treat[i,1] & data[,treatment_var]==1),time_var])
      state_initial_treat[i,2]=ifelse(state_initial_treat[i,2]=="Inf", 0, state_initial_treat[i,2])
    }
    state_initial_treat=as.data.frame(state_initial_treat)
    colnames(state_initial_treat)[1]="unit"
    colnames(state_initial_treat)[2]="initial_treat_period"
    colnames(data)[unit]="unit"
    col_name_vec=colnames(data)
    data=merge(data, state_initial_treat, by="unit")
    unit=1
    time_var=as.numeric(which(colnames(data)==col_name_vec[time_var]))
    outcome_var=as.numeric(which(colnames(data)==col_name_vec[outcome_var]))
    treatment_var=as.numeric(which(colnames(data)==col_name_vec[treatment_var]))
    data$initial_treat_var=as.numeric(data$initial_treat_period)
    initial_treat_var=as.numeric(which(colnames(data)=="initial_treat_var"))
    
    
    data[,treatment_var]=ifelse(data[,treatment_var]==untreated, 0, data[,treatment_var])
    untreated=0
    treated_periods=unique(data[which(data[,initial_treat_var]>0), initial_treat_var])
    treated_periods=treated_periods[which(treated_periods>min(data[,time_var])+1)]
    result_matrix=matrix(ncol=6, nrow=length(treated_periods))
    for (i in 1:length(treated_periods)){
      subbed=data[which(data[,initial_treat_var]==treated_periods[i]|data[,initial_treat_var]==untreated),]
      result_matrix[i,1]=treated_periods[i]
      subbed[,ncol(subbed)+1]=0
      colnames(subbed)[ncol(subbed)]="post_treat"
      subbed$post_treat=ifelse(subbed[,time_var]>=subbed[,initial_treat_var] & subbed[,initial_treat_var]!=0, 1, 0)
      setup=synthdid::panel.matrices(subbed, unit=unit, time=time_var, 
                           outcome = outcome_var, treatment = "post_treat")
      tau_hat=synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
      result_matrix[i,2]=as.numeric(tau_hat)
      result_matrix[i,3]=sqrt(vcov(tau_hat, method = "placebo"))
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
    #VARIANCE/ SE CALCULATION
    B=result_matrix[,2]
    x=data[,3]
    y=data[,4]
    x=as.matrix(x)
    y=as.matrix(y)
    B=as.matrix(B)
    influence=matrix(nrow=nrow(x), ncol=nrow(B))
    for(r in 1:nrow(B)){
      for(q in 1:nrow(x)){
        influence[q,r]=((x[q]-mean(x))/var(x))*((y[q]-mean(y))*B[r]*(x[q]-mean(x)))
      }
    }
    V=rnorm(nrow(data), 0, 1)
    variance=(1/((nrow(x)^2)))*(t(influence) %*% influence)
    weights=result_matrix$weight
    variance=t(weights) %*% weights
    overall_se=sqrt(sqrt(variance^2))       
    overall_t=(overall_estimator/(overall_se/sqrt(nrow(data))))
    overall_p=2*pt(q=sqrt(((overall_estimator/overall_se)^2)), df=(nrow(data)-length(unique(data[,time_var]))-length(unique(data[,unit]))), lower.tail=FALSE)
    lower_95_CI=overall_estimator-(1.96*overall_se)
    upper_95_CI=overall_estimator+(1.96*overall_se)
    print(paste0("ATT= ",overall_estimator, ifelse(overall_p < .05 & overall_p > .01,"**",
                                                   ifelse(overall_p < .1 & overall_p > .05,"*",
                                                          ifelse(overall_p < .01,"***","")))))
    print(paste0("SE= ", overall_se))
    print(paste0("p= ", overall_p))
    print(paste0("95% CI: (",lower_95_CI,", ",upper_95_CI,")"))
    print("*p<0.1; **p<0.5, ***p<0.01")
    
  }
