#' Staggered Synthetic Difference-in-Differences
#' @description Following the methods described in the appendix 
#'   of Arkhangelsky et al. (2021), Porreca (2022), and Clarke et al. (2023)
#' 
#' At least two pre-periods where no units are treated are required.
#' @param data Data.frame
#' @param untreated Numeric. Value assigned in treatment_var column to untreated units
#' @param outcome_var Numeric. Column number for y variable
#' @param unit Numeric. Column number for variable that identifies each unit
#' @param treatment_var Numeric. Column number for 0/1 variable. = 1 if unit is 
#'   treated in that period
#' @param time_var Numeric. Column number for variable indicating time period.
#' @param variance_type Character. Character string equal to either "jacknife" or "placebo" for standard error calculation
#' @param iterations Numeric. Number of placebo iterations to employ in calculating standard errors
#' 
#' @return Data.frame containing estimates for each treatment_period. Containing
#'   point estimates, standard error, and confidence intervals.
#' 


staggered_synth_DID <- 
  function(data, untreated, outcome_var,   unit, treatment_var, time_var, variance_type, iterations){
    attach(data)
    
    #Creating initial treatment time variable
    state_initial_treat=matrix(nrow=length(unique(data[,unit])), ncol=2)
    state_initial_treat[,1]=unique(data[,unit])
    for (i in 1:nrow(state_initial_treat)){
      state_initial_treat[i,2]=min(data[which(data[,unit]==state_initial_treat[i,1] & data[,treatment_var]==1),1])
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
    
    
    attach(data)
    timeNow <- Sys.time()
    data[,treatment_var]=ifelse(data[,treatment_var]==untreated, 0, data[,treatment_var])
    untreated=0
    treated_periods=unique(data[which(data[,initial_treat_var]>0), initial_treat_var])
    treated_periods=sort(treated_periods[which(treated_periods>min(data[,time_var])+1)])
    result_matrix=matrix(ncol=6, nrow=length(treated_periods))
    omega=list()
    lambda=list()
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
      result_matrix[i,3]=sqrt(vcov(tau_hat, method = "placebo"))
      result_matrix[i,4]=result_matrix[i,2]-(1.96*result_matrix[i,3])
      result_matrix[i,5]=result_matrix[i,2]+(1.96*result_matrix[i,3])
      result_matrix[i,6]=nrow(subbed[which(subbed$post_treat==1),])
      omega[[i]]=as.vector(attr(tau_hat, which="weights")$omega)
      lambda[[i]]=attr(tau_hat, which="weights")$lambda
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
    
    
    if (variance_type=="placebo"){
      B=matrix(ncol=3, nrow=iterations)
      co_sample=sample=data[which(data$initial_treat_period==0),]
      co_sample_units=unique(as.numeric(co_sample[,unit]))
      for (j in 1:iterations){
        placebo_treat_units=matrix(nrow=length(co_sample_units), ncol=2)
        placebo_treat_units[,1]=co_sample_units
        #number_to_treat=sample(seq(1:(length(co_sample_units)-1)), 1)
        number_to_treat=length(unique(data[which(data$initial_treat_period !=0),1]))
        treat_obs=sample(1:length(co_sample_units), number_to_treat, replace=FALSE)
        time_periods_sample=sort(unique(co_sample[,time_var]))
        placebo_treat_units[c(treat_obs),2]=sample(time_periods_sample[c(3:length(time_periods_sample))], length(treat_obs), replace=TRUE)
        placebo_treat_units[-c(treat_obs),2]=0
        
        data2=co_sample
        data2$initial_treat_period=placebo_treat_units[match(as.numeric(data2[,unit]), placebo_treat_units[,1]),2]
        data2[,treatment_var]=ifelse((data2[,unit] %in% placebo_treat_units) & data2[,time_var] >= data2$initial_treat_period &
                                       data2$initial_treat_period!=0, 1, 0)
        data2[,treatment_var]=ifelse(data2[,treatment_var]==untreated, 0, data2[,treatment_var])
        untreated=0
        treated_periods=unique(data2[which(data2[,initial_treat_var]>0), initial_treat_var])
        treated_periods=treated_periods[which(treated_periods>min(data2[,time_var])+1)]
        result_matrix2=matrix(ncol=6, nrow=length(treated_periods))
        for (i in 1:length(treated_periods)){
          subbed=data2[which(data2[,initial_treat_var]==treated_periods[i]|data2[,initial_treat_var]==untreated),]
          result_matrix2[i,1]=treated_periods[i]
          subbed[,ncol(subbed)+1]=0
          colnames(subbed)[ncol(subbed)]="post_treat"
          subbed$post_treat=ifelse(subbed[,time_var]>=subbed[,initial_treat_var] & subbed[,initial_treat_var]!=0, 1, 0)
          setup=panel.matrices(subbed, unit=unit, time=time_var, 
                               outcome = outcome_var, treatment = "post_treat")
          tau_hat=synthdid_estimate(setup$Y, setup$N0, setup$T0)
          result_matrix2[i,2]=as.numeric(tau_hat)
          result_matrix2[i,3]=NA
          result_matrix2[i,4]=NA
          result_matrix2[i,5]=NA
          result_matrix2[i,6]=nrow(subbed[which(subbed$post_treat==1),])
        }
        result_matrix2=as.data.frame(result_matrix2)
        result_matrix2$weight=result_matrix2[,6]/sum(result_matrix2[,6])
        result_matrix2[,6]=NULL
        B[j,1]=sum(result_matrix2[,6]*result_matrix2[,2])
        cat("\r", round(j*100/(nrow(data)+1), 2), "% done in ", Sys.time() - timeNow, " ... ")
        
      }
      B[,2]=mean(B[,1])
      B[,3]=(B[,1]-B[,2])^2
      variance=mean(B[,3])
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
    if (variance_type=="jackknife"){
    B=vector(mode="numeric", length=length(unique(data[,unit])))
    lambda_length=vector(mode="numeric", length=length(lambda))
    for (l in 1:length(lambda)){
      lambda_length[l]=length(lambda[[l]])
    }
    untreated_units=unique(data[which(data$initial_treat_period==0),unit])
    units=as.numeric(unique(data[,unit]))
    timeNow <- Sys.time()
    for (j in 1:length(units)){
      data2=data[-c(which(data[,unit]==units[j])),]
      data2[,treatment_var]=ifelse(data2[,treatment_var]==untreated, 0, data2[,treatment_var])
      untreated=0
      treated_periods2=unique(data2[which(data2[,initial_treat_var]>0), initial_treat_var])
      treated_periods2=sort(treated_periods2[which(treated_periods2>min(data2[,time_var])+1)])
      result_matrix2=matrix(ncol=6, nrow=length(treated_periods2))
      tmp_dat=data[which(data[,unit]==units[j]),]
      for (i in 1:length(treated_periods2)){
        if(tmp_dat[1,which(colnames(tmp_dat)=="initial_treat_period")]==0) {used_omega=as.vector(omega[[i]][-c(which(untreated_units==units[j]))])} else{used_omega=as.vector(omega[[i]])}
        subbed=data2[which(data2[,initial_treat_var]==treated_periods[i]|data2[,initial_treat_var]==untreated),]
        result_matrix2[i,1]=treated_periods2[i]
        subbed[,ncol(subbed)+1]=0
        colnames(subbed)[ncol(subbed)]="post_treat"
        subbed$post_treat=ifelse(subbed[,time_var]>=subbed[,initial_treat_var] & subbed[,initial_treat_var]!=0, 1, 0)
        if (sum(subbed$post_treat)==0){tau_hat=NA
        result_matrix2[i,2]=as.numeric(tau_hat)
        result_matrix2[i,3]=NA
        result_matrix2[i,4]=NA
        result_matrix2[i,5]=NA
        result_matrix2[i,6]=NA} else{
          setup=panel.matrices(subbed, unit=unit, time=time_var, 
                               outcome = outcome_var, treatment = "post_treat")
          tau_hat=synthdid_estimate(setup$Y, setup$N0, setup$T0, weights=list(omega = used_omega, 
                                                                              lambda=lambda[[which(lambda_length==setup$T0)]]))
          
          result_matrix2[i,2]=as.numeric(tau_hat)
          result_matrix2[i,3]=NA
          result_matrix2[i,4]=NA
          result_matrix2[i,5]=NA
          result_matrix2[i,6]=nrow(subbed[which(subbed$post_treat==1),])
        }
      }
      result_matrix2=as.data.frame(result_matrix2)
      result_matrix2=result_matrix2[which(complete.cases(result_matrix2)==TRUE),]
      result_matrix2$weight=result_matrix2[,6]/sum(result_matrix2[,6])
      result_matrix2[,6]=NULL
      B[j]=sum(result_matrix2[,6]*result_matrix2[,2])
      cat("\r", round(j*100/(length(units)+1), 2), "% done in ", Sys.time() - timeNow, " ... ")
    }
    variance=var(B)
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
    if(variance_type!="jackknife" & variance_type!="placebo"){print("variance type argument must be set to jackknife or placebo")}
    
  }
