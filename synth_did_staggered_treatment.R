#' Staggered Synthetic Difference-in-Differences
#' @description Following the methods described in the appendix 
#'   of Arkhangelsky et al. (2021)
#'   
#' @param data Data.frame
#' @param initial_treat_var Numeric. Column number containing initial treatment 
#'   time period.
#' @param untreated Numeric. Column number for untreated 0/1 variable. = 1 if 
#'   the unit is untreated
#' @param outcome_var Numeric. Column number for y variable
#' @param unit Numeric. Column number for variable that identifies each unit
#' @param treatment_var Numeric. Column number for 0/1 variable. = 1 if unit is 
#'   treated in that period
#' @param time_var Numeric. Column number for variable indicating time period. 
#' 
#' @return Data.frame containing estimates for each treatment_period. Containing
#'   point estimates, standard error, and confidence intervals.
#' 
staggered_synth_DID = function(data, initial_treat_var, untreated, 
                               outcome_var, unit, treatment_var, time_var) {
    
    treated_periods <- unique(data[which(data[, initial_treat_var] > 0), initial_treat_var])
    treated_periods <- treated_periods[which(treated_periods > min(data[, time_var]) + 1)]
    
    result_matrix <- matrix(ncol = 6, nrow = length(treated_periods))
    
    for (i in 1:length(treated_periods)) {
      subbed <- data[which(data[, initial_treat_var] == treated_periods[i] | data[, initial_treat_var] == untreated), ]
      subbed[, ncol(subbed) + 1] <- 0
      colnames(subbed)[ncol(subbed)] <- "post_treat"
      
      subbed$post_treat <- ifelse(subbed[, time_var] >= subbed[, initial_treat_var] & subbed[, initial_treat_var] != 0, 1, 0)
      
      setup <- panel.matrices(subbed,
        unit = unit, time = time_var,
        outcome = outcome_var, treatment = "post_treat"
      )
      
      tau_hat <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
      
      result_matrix[i, 1] <- treated_periods[i]
      result_matrix[i, 2] <- as.numeric(tau_hat)
      result_matrix[i, 3] <- sqrt(vcov(tau_hat, method = "jackknife"))
      result_matrix[i, 4] <- result_matrix[i, 2] - (1.96 * result_matrix[i, 3])
      result_matrix[i, 5] <- result_matrix[i, 2] + (1.96 * result_matrix[i, 3])
      result_matrix[i, 6] <- nrow(subbed[which(subbed$post_treat == 1), ])
    }
    
    result_matrix <- as.data.frame(result_matrix)
    colnames(result_matrix) <- c("treatment_period", "tau_hat", "standard_error", "lower_95_CI", "upper_95_CI", "")
    result_matrix$weight <- result_matrix[, 6] / sum(result_matrix[, 6])
    result_matrix[, 6] <- NULL

    # FOR NOW SE WILL BE CALCULATED THROUGH SIMILIAR WEIGHTING- LITERATURE NEEDS TO RESOLVE THIS. METHOD HERE LIKELY WRONG
    overall_estimator <- sum(result_matrix[, 6] * result_matrix[, 2])
    overall_se <- sum(result_matrix[, 6] * result_matrix[, 3])
    lower_95_CI <- overall_estimator - (1.96 * overall_se)
    upper_95_CI <- overall_estimator + (1.96 * overall_se)
    
    print(paste0("ATT= ", overall_estimator))
    print(paste0("SE= ", overall_se))
    print(paste0("95% CI: (", lower_95_CI, ", ", upper_95_CI, ")"))
    
    return(result_matrix)
  }
