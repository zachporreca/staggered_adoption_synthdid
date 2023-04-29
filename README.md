## staggered_adoption_synthdid

***IMPORTANT:***

It seems other folks have made headway on this same project, so my work here is no longer needed. I'm a little strapped for time with a new job, and had only initially written this because I needed the functionality at the time. As such, I will no longer be maintaining this code. I'll leave it to people who are likely much better coders than I. 

- [Here is a link](https://github.com/TJhon/ssynthdid) to a new R package with more functionality written by [Jhon Flores](https://github.com/TJhon)

- [Here is a link](https://github.com/Daniel-Pailanir/sdid) to a Stata package with more functionality that has the Athey and Imbens stamp of approval, so really is the gold standard. It's written by (https://github.com/Daniel-Pailanir/sdid)

Use those packages in your research as what I have here will no longer be maintained. It's been fun
******

Original 1/11/22

Most recent update 04/06/23   

PLEASE KEEP AND EYE OUT FOR BUGS WITH NEW UPDATE

Code to incorporate staggered treatment adoption (based on appendix from Arkhangelsky et al. 2021) into synthdid package

This is meant solely for applying synthetic difference-in-difference methods to staggered treatment timing settings
  
Package can be installed as follows: *THIS INSTALLATION METHOD IS NOT WORKING AT THE MOMENT, FOR NOW JUST LOAD THE CODE MANUALLY*

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
Inputs (aside from data, iterations, variance type,, and untreated) are column <b>numbers</b>. The input is of the following format:

staggered_synth_DID(data, untreated, outcome_var,   unit, treatment_var, time_var, variance type, iterations)

Data refers to the data source's name, untreated refers to the value assigned in the treatment column to untreated units, variance type is a character string that can be equal to either "jacknife" or "placebo", iterations is the number of placebo inference iterations to employ in calculating the standard errors (this parameter will be unused if "jackknife" standard errors are employe, but a value must still be supplied), and the remainder of variables refer to the variable column number. 

Note: Use data frames or matrices, no support for tibbles


##UPDATE
- Standard error calculation has been updated to either the placebo algorithm suggested by [Clarke et al. 2023](https://arxiv.org/pdf/2301.11859.pdf) or the jackknife procedure described in the same. Thanks to [Agoston Reguly](https://github.com/regulyagoston) for finding a bug in the variance calculation code which pushed me to finally adopt this method. The bootstrap algorithm comes with a new user input needed: a number of iterations to employ. This parameter must still be supplied even if a jackknife procedure will be utilized (although the iterations value will then not be used). 
- As described in [Clarke et al. 2023](https://arxiv.org/pdf/2301.11859.pdf) the placebo method for calculating standard errors can only be used if the number of control units is strictly larger than the number of treated units.
- An initital treatment period variable is no longer needed as an input (Thanks to [Alex Marsella](https://alexmarsella.github.io/) for the suggestion)
- Updated to fix a small bug, involving creation of initial treatment variable (updated 12/13/2022)

Author: Zachary Porreca @zachporreca

Contributions by Kyle Butts @kylefbutts
