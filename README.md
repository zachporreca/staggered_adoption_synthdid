## staggered_adoption_synthdid

Original 1/11/22

Most recent update 12/13/22   

Code to incorporate staggered treatment adoption (based on appendix from Arkhangelsky et al. 2021) into synthdid package

This is meant solely for applying synthetic difference-in-difference methods to staggered treatment timing settings

Package can be installed as follows:

```
devtools::install_github("zachporreca/staggered_adoption_synthdid")
```

And if you have not already installed the synthdid package:
```
devtools::install_github("synth-inference/synthdid")
```

After installation the package can be called as follows:
```
library(staggeredSynthDid)
```


### This code is still in an early Beta version and has the following issues/ concerns (Help addressing them would be greatly appreciated):

1) I have not yet incorporated or looked into the xsynthdid methods for covariates
2) Per the comment from the synthdid package author (David Hishberg, also one of the paper's authors) at least two time periods before the initial treatment are needed to use the synthdid method. Because of this, in the case where two periods do not exist before the first treatment period, the program will treat those initially treated/period one treated/period two treated as untreated controls. This staggered treatment timing synthdid method is not appropriate for all settings. 
4) I need to write up a vignette demonstration still. I'm a bit busy, but will get to this soon (still haven't gotten to it, but my [working paper](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4015931) may help fill in some of the blanks. Once my life's a bit freer, I will upload a real vignette
5) TESTING, TESTING, and MORE TESTING (also benchmarking) needs to be done. 


### Use
Inputs (aside from data and untreated) are column <b>numbers</b>. The input is of the following format:

staggered_synth_DID(data, untreated, outcome_var,   unit, treatment_var, time_var)

Once again, data refers to the data source's name, untreated refers to the value assigned in the treatment column to untreated units, and the remainder of variables refer to the variable column number. 

Note: Use data frames or matrices, no support for tibbles


##UPDATE
- Code has been updated with a more correct computation of standard errors based on an influence functions for each observation and cohort. The overall summary parameter's variance, V_theta, is calculated as follows: V_theta=w'Vw where V is calculated as described in [Kahn (2015)](https://j-kahn.com/files/influencefunctions.pdf)
- An initital treatment period variable is no longer needed as an input (Thanks to [Alex Marsella](https://alexmarsella.github.io/) for the suggestion)
- Updated to fix a small bug, involving creation of initial treatment variable (updated 12/13/2022)

Author: Zachary Porreca @zachporreca

Contributions by Kyle Butts @kylefbutts
