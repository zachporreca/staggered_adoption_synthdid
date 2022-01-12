## staggered_adoption_synthdid

Original 1/11/22

Most recent update 1/12/22

Code to incorporate staggered treatment adoption (based on appendix from Arkhangelsky et al. 2021) into synthdid package

This is meant solely for applying synthetic difference-in-difference methods to staggered treatment timing settings


### This code is still in an early Beta version and has the following issues/ concerns (Help addressing them would be greatly appreciated):

1) I have not yet incorporated or looked into the xsynthdid methods for covariates
2) Per the comment from the synthdid package author (David Hishberg, also one of the paper's authors) at least two time periods before the initial treatment are needed to use the synthdid method. Because of this, in the case where two periods do not exist before the first treatment period, the program will treat those initially treated/period one treated/period two treated as untreated controls. This staggered treatment timing synthdid method is not appropriate for all settings. 
3) Standard errors of the overall ATT estimator. This is an issue seemingly not yet reoslved in the literature (Ben-Michael, Feller, and Rothstein 2019). For the individual component estimators, I have used the jackknife method as suggested. For now, the overall ATT SE has been computed through applying the same weighting method to the component SE's. I am near certain that this method is incorrect (or at best less than ideal). I have proceeded from this simply to allow for completion of the code's output. This is a priority area to resolve. 
4) I need to write up a vignette demonstration still. I'm a bit busy, but will get to this soon
5) TESTING, TESTING, and MORE TESTING (also benchmarking) needs to be done. 


### Use
Inputs (aside from data and untreated) are column <b>numbers</b>. The input is of the following format:

staggered_synth_DID(data, initial_treat_var, untreated, outcome_var,   unit, treatment_var, time_var)

Once again, data refers to the data source's name, untreated refers to the value assigned in the treatment column to untreated units, and the remainder of variables refer to the variable column number. 

AUthor: Zachary Porreca @zachporreca
Contributions by Kyle Butts @kylefbutts
