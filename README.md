## staggered_adoption_synthdid

Original 1/11/22

Most recent update 04/05/23   

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

staggered_synth_DID(data, untreated, outcome_var,   unit, treatment_var, time_var, iterations)

Data refers to the data source's name, untreated refers to the value assigned in the treatment column to untreated units, iterations is the number of bootstrap iterations to employ in calculating the standard errors, and the remainder of variables refer to the variable column number. 

Note: Use data frames or matrices, no support for tibbles


##UPDATE
- Standard error calculation has been updated to the bootstrap algorithm suggested by [Clarke et al. 2023](https://arxiv.org/pdf/2301.11859.pdf). Thanks to [Agoston Reguly](https://github.com/regulyagoston) for finding a bug in the variance calculation code which pushed me to finally adopt this method. The bootstrap algorithm comes with a new user input needed: a number of iterations to employ. 
- An initital treatment period variable is no longer needed as an input (Thanks to [Alex Marsella](https://alexmarsella.github.io/) for the suggestion)
- Updated to fix a small bug, involving creation of initial treatment variable (updated 12/13/2022)

Author: Zachary Porreca @zachporreca

Contributions by Kyle Butts @kylefbutts
