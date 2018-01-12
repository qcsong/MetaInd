# MetaInd 

R Package for Conducting Meta-Analysis with Individual Correction of Reliability and Range Restriction <br /> 
Developer: Chelsea Song <br /> 
Contact: qianqisong@gmail.com <br /> 
Last Update: 01/24/2017 <br /> 
*Current version is beta version, please forward any questions or suggestions to qianqisong@gmail.com. Thanks!*

## Objective ##

The current R packages provides meta-analytic results using Hunter and Schmidt method (Schmidt & Hunter, 2014). Artifact correction are done for each individual effect size. Users can specify meta-analytic corrections for 
1. Reliability of independent variable <br />
2. Reliability of dependent variable <br />
3. Direct and indirect range restriction (Hunter, Schmidty, & Le, 2006) <br /> 

## Instructions ##

1. Open an R console or RStudio window. (R can be downloaded for free from https://cran.r-project.org; RStudio can be downloaded for free from https://www.rstudio.com/)
2. Install R package "ParetoR" through Github by pasting and running the following commands in R console or RStudio:
   install.packages("devtools") <br />
   library("devtools") <br />
   install_github("qcsong/MetaInd") <br />
   library("MetaInd") <br />
3. Import meta-analytic data. See dataset "ABHt32" for example: <br />
   data("ABHt32") <br />
4. Obtain meta-analytic results in a summary table using function "MetaSummary". Example below: <br />
   MetaSummary(ABHt32) <br />
   You may specify artifact correction through MetaSummary function options. Type following code for more details.
   help("MetaSummary") 
 
## Output Description ##

Summary table of meta-analytic results, consisting of:
* *n* - sample size  
* *k* - number of studies
* *rbar* - sample size-weighted average effect size
* *Var.rbar* - variance of sample size-weighted average effect size estimate
* *VarSE.rbar* - variance due to sampling error of average effect size estimate
* *PerVarExp.rbar* - percent variance explained by sampling error for average effect size estimate
* *LCL95.rbar* - lower boundary of 95% confidence interval of average effect size estimate
* *UCL95.rbar* - upper boundary of 95% confidence interval of average effect size estimate
* *rho* - corrected meta-analytic effect size estimate
* *Var.rho* - variance of corrected meta-analytic effect size estimate
* *VarSE.rho* - variance due to sampling error of corrected effect size estimate
* *PerVarExp.rho* - percent variance expained by sampling error for corrected effect size estimate
* *LCL95.rho* - lower boundary of 95% confidence interval of corrected effect size estimate
* *UCL95.rho* - upper boundary of 95% confidence interval of average effect size  
* *LCV80* - lower boundary of 80% credibility interval 
* *UCV80* - upper boundary of 80% credibility interval

## Citation ##

If you use the package for publication, please refer to it using the following citation:
Song, Q. C. (2017). MetaInd: A R Package for Conducting Meta-Analysis with Individual Correction of Reliability and Range Restriction. URL: https://github.com/qcsong/MetaInd

### Note ###

Package dependency: psychometric package

### References ###

1. Schmidt, F. L., & Hunter, J. E. (2014). Methods of meta-analysis: Correcting error and bias in research findings. Sage publications.
2. Hunter, J. E., Schmidt, F. L., & Le, H. (2006). Implications of direct and indirect range restriction for meta-analysis methods and findings. Journal of Applied Psychology, 91(3), 594.
