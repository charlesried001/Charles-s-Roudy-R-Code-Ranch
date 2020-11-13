Lab 6
================
Charles Reid

<p style="color:rgb(182,18,27);font-family:corbel">

Econ B2000, MA Econometrics

</p>

<p style="color:rgb(182,18,27);font-family:corbel">

Colin Powell School at the City College of New York, CUNY

</p>

<p style="color:rgb(182,18,27);font-family:corbel">

Fall 2020

</p>

``` r
#Load Required packages
library(stargazer)
library(kableExtra)
library(knitr)
library(tidyverse)
library(jtools)
library(sandwich)
library(np)
```

# Lab 6 - Probit and Logit Models

Probit and Logit are models are suited for when the dependent y variable
takes values of just 0 or 1.

### Men Over 25 in the Labor Force - Setting Up the Data

I am interested in looking at men over the age of 25. This project will
be looking at whether someone is in the labor force or not in the labor
force based on their marital status and other factors.

``` r
load("acs2017_ny_data.RData")
attach(acs2017_ny)

#select all men over the age of 25
use_varb <- (AGE >= 25) & (female == 0)

#subset this selection of of the acs2017_ny data set (the entire data set is kept, but what is kept filtered based on age)
men_over_25 <- subset(acs2017_ny,use_varb) 
detach()

#attach this newly created subset from the dataset
attach(men_over_25)

#Establish categorical levels for each different type of employment situations.
men_over_25$LABFORCE <- as.factor(men_over_25$LABFORCE)
levels(men_over_25$LABFORCE) <- c("Not in LF","in LF", "N/A")

#Establish categorical levels for each different type of marital situations.
men_over_25$MARST <- as.factor(men_over_25$MARST)
levels(men_over_25$MARST) <- c("married spouse present","married spouse absent","separated","divorced","widowed","never married")

#Create Age Ranges which will show how employment situation progresses with age
men_over_25$age_bands <- cut(men_over_25$AGE,breaks=c(0,25,35,45,55,65,100))

#Put the data in a table with these Age Ranges 
kabletable <- table(men_over_25$age_bands,men_over_25$LABFORCE)

plot(kabletable, main="Labor Force Participation Based on Age Range",
xlab="Range of Ages",
ylab="Labor Force Participation", 
col = c("red", "blue"))
```

![](Homework-6_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Men Over 25 in the Labor Force - Analyzinng the Data

The above graph shows that men reach their peak labor force
participation at around age 35 - 45, the participation in the labor
force steadily drops off from that point, suggesting that men leave the
labor force mostly around age 65. In the U.S., full retirement age is
currently 66 years, this may explain the large exodus as many retirement
benefits given that 401k disbursements and other investments and
retirement vehicles mature around this time in a workers lifespan.

### What is the difference between “NA” as label and Not in the Labor Force?

NA’s only occur under the age of 16 because these individuals, as
defined by the US Department of labor are not considered when
calculating the labor force (there is no upper age limit). A survey of
the original data-set, sorting by age proves this fact:

``` r
attach(acs2017_ny)
```

    ## The following objects are masked from men_over_25:
    ## 
    ##     AfAm, AGE, Amindian, ANCESTR1, ANCESTR1D, ANCESTR2, ANCESTR2D,
    ##     Asian, below_150poverty, below_200poverty, below_povertyline, BPL,
    ##     BPLD, BUILTYR2, CITIZEN, CLASSWKR, CLASSWKRD, Commute_bus,
    ##     Commute_car, Commute_other, Commute_rail, Commute_subway, COSTELEC,
    ##     COSTFUEL, COSTGAS, COSTWATR, DEGFIELD, DEGFIELD2, DEGFIELD2D,
    ##     DEGFIELDD, DEPARTS, EDUC, educ_advdeg, educ_college, educ_hs,
    ##     educ_nohs, educ_somecoll, EDUCD, EMPSTAT, EMPSTATD, FAMSIZE,
    ##     female, foodstamps, FOODSTMP, FTOTINC, FUELHEAT, GQ,
    ##     has_AnyHealthIns, has_PvtHealthIns, HCOVANY, HCOVPRIV, HHINCOME,
    ##     Hisp_Cuban, Hisp_DomR, Hisp_Mex, Hisp_PR, HISPAN, HISPAND,
    ##     Hispanic, in_Bronx, in_Brooklyn, in_Manhattan, in_Nassau, in_NYC,
    ##     in_Queens, in_StatenI, in_Westchester, INCTOT, INCWAGE, IND,
    ##     LABFORCE, LINGISOL, MARST, MIGCOUNTY1, MIGPLAC1, MIGPUMA1,
    ##     MIGRATE1, MIGRATE1D, MORTGAGE, NCHILD, NCHLT5, OCC, OWNCOST,
    ##     OWNERSHP, OWNERSHPD, POVERTY, PUMA, PWPUMA00, RACE, race_oth,
    ##     RACED, RELATE, RELATED, RENT, ROOMS, SCHOOL, SEX, SSMC, TRANTIME,
    ##     TRANWORK, UHRSWORK, UNITSSTR, unmarried, veteran, VETSTAT,
    ##     VETSTATD, white, WKSWORK2, YRSUSA1

``` r
select(acs2017_ny,"AGE","LABFORCE")
```

    ## # A tibble: 196,585 x 2
    ##      AGE                       LABFORCE
    ##    <dbl>                      <int+lbl>
    ##  1    72 2 [Yes, in the labor force]   
    ##  2    72 2 [Yes, in the labor force]   
    ##  3    31 2 [Yes, in the labor force]   
    ##  4    28 2 [Yes, in the labor force]   
    ##  5    54 2 [Yes, in the labor force]   
    ##  6    45 1 [No, not in the labor force]
    ##  7    84 1 [No, not in the labor force]
    ##  8    71 2 [Yes, in the labor force]   
    ##  9    68 1 [No, not in the labor force]
    ## 10    37 2 [Yes, in the labor force]   
    ## # ... with 196,575 more rows

``` r
detach(acs2017_ny)
```

### What fraction of people, say, 55-65, are in the labor force?

``` r
table_summary <- summary(kabletable)
table_summary
```

    ## Number of cases in table: 66625 
    ## Number of factors: 2 
    ## Test for independence of all factors:
    ##  Chisq = NaN, df = 10, p-value = NA
    ##  Chi-squared approximation may be incorrect

``` r
kabletable %>% 
  kbl() %>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Not in LF

</th>

<th style="text-align:right;">

in LF

</th>

<th style="text-align:right;">

N/A

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

(0,25\]

</td>

<td style="text-align:right;">

243

</td>

<td style="text-align:right;">

981

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:left;">

(25,35\]

</td>

<td style="text-align:right;">

1815

</td>

<td style="text-align:right;">

10453

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:left;">

(35,45\]

</td>

<td style="text-align:right;">

1365

</td>

<td style="text-align:right;">

9756

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:left;">

(45,55\]

</td>

<td style="text-align:right;">

2161

</td>

<td style="text-align:right;">

11008

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:left;">

(55,65\]

</td>

<td style="text-align:right;">

4516

</td>

<td style="text-align:right;">

9126

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:left;">

(65,100\]

</td>

<td style="text-align:right;">

11950

</td>

<td style="text-align:right;">

3251

</td>

<td style="text-align:right;">

0

</td>

</tr>

</tbody>

</table>

Of the 66,625 respondents, 9,126, who are between ages 55 and 65 are in
the labor force while 4,516 of that same age group were not. We can, in
the table above, see approximately 66% are in the laborforce.

``` r
(9126/13642)
```

    ## [1] 0.6689635

### What about other age ranges? What would you guess are other important predictors?

Factors in the data set that may affect someone’s willingness to work or
look for work:

  - NCHILD - Number of Children
  - EDUC - Education
  - DEGFIELD - Degree Field
  - MORTGAGE - Home ownership
  - COSTELEC x COSTFUEL x COSTGAS - Utilities
  - RACE - Ethnicity

# Men Over 25 in the Labor Force - Developing a Model

Developing a model is an ordeal of trial an error, here, multiple models
will be compiled and compared to select the best one.

## Original Model - Model 1

The original model conceived.

``` r
model_1 <- glm(as.factor(LABFORCE) ~ AGE + MARST,
                          family = binomial, data = men_over_25)
```

## Age Constrained Model - Model 2

A model based on a new subset with finer age restrictions (a cap of 55
cap).

``` r
attach(acs2017_ny)
```

    ## The following objects are masked from men_over_25:
    ## 
    ##     AfAm, AGE, Amindian, ANCESTR1, ANCESTR1D, ANCESTR2, ANCESTR2D,
    ##     Asian, below_150poverty, below_200poverty, below_povertyline, BPL,
    ##     BPLD, BUILTYR2, CITIZEN, CLASSWKR, CLASSWKRD, Commute_bus,
    ##     Commute_car, Commute_other, Commute_rail, Commute_subway, COSTELEC,
    ##     COSTFUEL, COSTGAS, COSTWATR, DEGFIELD, DEGFIELD2, DEGFIELD2D,
    ##     DEGFIELDD, DEPARTS, EDUC, educ_advdeg, educ_college, educ_hs,
    ##     educ_nohs, educ_somecoll, EDUCD, EMPSTAT, EMPSTATD, FAMSIZE,
    ##     female, foodstamps, FOODSTMP, FTOTINC, FUELHEAT, GQ,
    ##     has_AnyHealthIns, has_PvtHealthIns, HCOVANY, HCOVPRIV, HHINCOME,
    ##     Hisp_Cuban, Hisp_DomR, Hisp_Mex, Hisp_PR, HISPAN, HISPAND,
    ##     Hispanic, in_Bronx, in_Brooklyn, in_Manhattan, in_Nassau, in_NYC,
    ##     in_Queens, in_StatenI, in_Westchester, INCTOT, INCWAGE, IND,
    ##     LABFORCE, LINGISOL, MARST, MIGCOUNTY1, MIGPLAC1, MIGPUMA1,
    ##     MIGRATE1, MIGRATE1D, MORTGAGE, NCHILD, NCHLT5, OCC, OWNCOST,
    ##     OWNERSHP, OWNERSHPD, POVERTY, PUMA, PWPUMA00, RACE, race_oth,
    ##     RACED, RELATE, RELATED, RENT, ROOMS, SCHOOL, SEX, SSMC, TRANTIME,
    ##     TRANWORK, UHRSWORK, UNITSSTR, unmarried, veteran, VETSTAT,
    ##     VETSTATD, white, WKSWORK2, YRSUSA1

``` r
use_varb_25_55 <- (acs2017_ny$AGE >25) & (acs2017_ny$AGE <= 55)
men_between_25_55 <- subset(acs2017_ny, use_varb_25_55)

#Establish categorical levels for each different type of employment situations.
men_between_25_55$LABFORCE <- as.factor(men_between_25_55$LABFORCE)
levels(men_between_25_55$LABFORCE) <- c("Not in LF","in LF", "N/A")

#Establish categorical levels for each different type of marital situations.
men_between_25_55$MARST <- as.factor(men_between_25_55$MARST)
levels(men_between_25_55$MARST) <- c("married spouse present","married spouse absent","separated","divorced","widowed","never married")

model_2 <- glm(as.factor(LABFORCE) ~ AGE + MARST,
                          family = binomial, data = men_between_25_55)
```

## Age Constrained Model - Model 3

``` r
use_varb_25_45 <- (acs2017_ny$AGE >25) & (acs2017_ny$AGE <= 45)
men_between_25_45 <- subset(acs2017_ny, use_varb_25_45)

#Establish categorical levels for each different type of employment situations.
men_between_25_45$LABFORCE <- as.factor(men_between_25_45$LABFORCE)
levels(men_between_25_45$LABFORCE) <- c("Not in LF","in LF", "N/A")

#Establish categorical levels for each different type of marital situations.
men_between_25_45$MARST <- as.factor(men_between_25_45$MARST)
levels(men_between_25_45$MARST) <- c("married spouse present","married spouse absent","separated","divorced","widowed","never married")


model_3 <- glm(as.factor(LABFORCE) ~ AGE + MARST,
                          family = binomial, data = men_between_25_45)

stargazer(model_1,model_2,model_3, type = "text")
```

    ## 
    ## ==============================================================
    ##                                    Dependent variable:        
    ##                            -----------------------------------
    ##                                    as.factor(LABFORCE)        
    ##                                (1)         (2)         (3)    
    ## --------------------------------------------------------------
    ## AGE                         -0.096***   -0.018***   -0.010*** 
    ##                              (0.001)     (0.001)     (0.002)  
    ##                                                               
    ## MARSTmarried spouse absent  -1.087***   -0.787***   -0.674*** 
    ##                              (0.057)     (0.049)     (0.061)  
    ##                                                               
    ## MARSTseparated              -0.658***   -0.448***   -0.320*** 
    ##                              (0.066)     (0.055)     (0.077)  
    ##                                                               
    ## MARSTdivorced               -0.462***   -0.093**      0.029   
    ##                              (0.033)     (0.036)     (0.057)  
    ##                                                               
    ## MARSTwidowed                -0.963***   -0.717***   -1.088*** 
    ##                              (0.061)     (0.089)     (0.152)  
    ##                                                               
    ## MARSTnever married          -1.597***   -0.543***   -0.306*** 
    ##                              (0.028)     (0.022)     (0.027)  
    ##                                                               
    ## Constant                    6.532***    2.483***    2.079***  
    ##                              (0.053)     (0.052)     (0.085)  
    ##                                                               
    ## --------------------------------------------------------------
    ## Observations                 66,625      74,935      47,782   
    ## Log Likelihood             -31,426.050 -35,274.000 -21,991.900
    ## Akaike Inf. Crit.          62,866.090  70,561.990  43,997.790 
    ## ==============================================================
    ## Note:                              *p<0.1; **p<0.05; ***p<0.01

#### Model 1 - no age cap, captures the general form and tenancies of the data set.

  - AGE and the Constant show a downward trend, showing how age by of
    extent time has a long term negative effect on someone being in the
    labor force.

  - The spouse being absent, longer makes it more likely a man will have
    a job.

#### Model 2 - an age range of 25 - 55, captures early to late career stages.

  - Divorce is loosing its statistical significance for predicting labor
    force participation.

#### Model 3 - age 25 - 45, early to peak labor force participation.

  - Divorce is not significant predictor of a young man being in the
    labor force. This could be because less men are married young or
    because they have time to find another woman. A closer look should
    be taken.

  - The standard errors are becoming smaller as we the sample becomes
    smaller, we are moving away from the true population mean however
    that doesn’t mean this data set cannot tell us something about the
    young men and marriage.

### Heteroscedasticity

heteroscedasticity is the extent to which the variance of the residuals
depends on the predictor variable. Variance is the amount of difference
between the actual outcome and the outcome predicted by your model.

Data is heteroskedastic if the amount that the residuals vary from the
model changes as the predictor variable changes.

For example:

Once upon a time there was learner named Kevin. Kevin went to the best
schools where he gained advanced knowledge in Economics, Statistics and
Finance. First a BA degree, then a Masters and a Ph.D. One would assume
that as Kevin gained more education he would earn more money, since
Kevin could get any job he wanted; Investment Banker, Corporate
Consultant, Data Engineer, instead Kevin chose to be a teacher and earn
a significantly lower salary for some unknown reason. This discrepancy
causes residuals of kevins education vs salary to spread out. If Kevin
were a dot in a scatter plot the variance associated with his education
would increase as his education increases.

Essentially Heteroscedasticity represents the fact that the data is
influenced by something not accounted for in the model.

### Robust Standard Errors

  - Robust standard errors are used to test for Heteroscedasticity on
    OLS(vanilla regression) because the normal standard errors reported
    will be too small when Heteroscedasticity is present. There are ways
    to obtain better estimates besides Robust Standard Errors.

Rboust errors are called by using: “vcov = vcovHC(regression))” in
stargazer.

### General Linear Models

  - GLM models can also be used to correct for Heteroscedasticity.

### Breusch-Pagan Test - \#bptest(model)

Breusch-Pagan Test Can also be used to test for Heteroscedasticity.
Tests if, given the predicting variables, whether the r^2 increases
arbitrarily because of the added explanatories.

Where: \* H1: If the p value \< α level of significance then we have a
1-α level of confidence that the residuals are heteroskedastic.

  - H0: If the p value \> α level of significance then we have a 1-α
    level of confidence that the residuals are homoskedastic.

# Baseline model:

A model containing the most diverse mix of explanatories that should
provide the best results (maybe). \* Major and baseline education bench
marks have been selected. \* Family composition and marital status as
well as an interaction term for female + N child since women are always
made to take care of child rearing

``` r
attach(acs2017_ny)
```

    ## The following objects are masked from acs2017_ny (pos = 3):
    ## 
    ##     AfAm, AGE, Amindian, ANCESTR1, ANCESTR1D, ANCESTR2, ANCESTR2D,
    ##     Asian, below_150poverty, below_200poverty, below_povertyline, BPL,
    ##     BPLD, BUILTYR2, CITIZEN, CLASSWKR, CLASSWKRD, Commute_bus,
    ##     Commute_car, Commute_other, Commute_rail, Commute_subway, COSTELEC,
    ##     COSTFUEL, COSTGAS, COSTWATR, DEGFIELD, DEGFIELD2, DEGFIELD2D,
    ##     DEGFIELDD, DEPARTS, EDUC, educ_advdeg, educ_college, educ_hs,
    ##     educ_nohs, educ_somecoll, EDUCD, EMPSTAT, EMPSTATD, FAMSIZE,
    ##     female, foodstamps, FOODSTMP, FTOTINC, FUELHEAT, GQ,
    ##     has_AnyHealthIns, has_PvtHealthIns, HCOVANY, HCOVPRIV, HHINCOME,
    ##     Hisp_Cuban, Hisp_DomR, Hisp_Mex, Hisp_PR, HISPAN, HISPAND,
    ##     Hispanic, in_Bronx, in_Brooklyn, in_Manhattan, in_Nassau, in_NYC,
    ##     in_Queens, in_StatenI, in_Westchester, INCTOT, INCWAGE, IND,
    ##     LABFORCE, LINGISOL, MARST, MIGCOUNTY1, MIGPLAC1, MIGPUMA1,
    ##     MIGRATE1, MIGRATE1D, MORTGAGE, NCHILD, NCHLT5, OCC, OWNCOST,
    ##     OWNERSHP, OWNERSHPD, POVERTY, PUMA, PWPUMA00, RACE, race_oth,
    ##     RACED, RELATE, RELATED, RENT, ROOMS, SCHOOL, SEX, SSMC, TRANTIME,
    ##     TRANWORK, UHRSWORK, UNITSSTR, unmarried, veteran, VETSTAT,
    ##     VETSTATD, white, WKSWORK2, YRSUSA1

    ## The following objects are masked from men_over_25:
    ## 
    ##     AfAm, AGE, Amindian, ANCESTR1, ANCESTR1D, ANCESTR2, ANCESTR2D,
    ##     Asian, below_150poverty, below_200poverty, below_povertyline, BPL,
    ##     BPLD, BUILTYR2, CITIZEN, CLASSWKR, CLASSWKRD, Commute_bus,
    ##     Commute_car, Commute_other, Commute_rail, Commute_subway, COSTELEC,
    ##     COSTFUEL, COSTGAS, COSTWATR, DEGFIELD, DEGFIELD2, DEGFIELD2D,
    ##     DEGFIELDD, DEPARTS, EDUC, educ_advdeg, educ_college, educ_hs,
    ##     educ_nohs, educ_somecoll, EDUCD, EMPSTAT, EMPSTATD, FAMSIZE,
    ##     female, foodstamps, FOODSTMP, FTOTINC, FUELHEAT, GQ,
    ##     has_AnyHealthIns, has_PvtHealthIns, HCOVANY, HCOVPRIV, HHINCOME,
    ##     Hisp_Cuban, Hisp_DomR, Hisp_Mex, Hisp_PR, HISPAN, HISPAND,
    ##     Hispanic, in_Bronx, in_Brooklyn, in_Manhattan, in_Nassau, in_NYC,
    ##     in_Queens, in_StatenI, in_Westchester, INCTOT, INCWAGE, IND,
    ##     LABFORCE, LINGISOL, MARST, MIGCOUNTY1, MIGPLAC1, MIGPUMA1,
    ##     MIGRATE1, MIGRATE1D, MORTGAGE, NCHILD, NCHLT5, OCC, OWNCOST,
    ##     OWNERSHP, OWNERSHPD, POVERTY, PUMA, PWPUMA00, RACE, race_oth,
    ##     RACED, RELATE, RELATED, RENT, ROOMS, SCHOOL, SEX, SSMC, TRANTIME,
    ##     TRANWORK, UHRSWORK, UNITSSTR, unmarried, veteran, VETSTAT,
    ##     VETSTATD, white, WKSWORK2, YRSUSA1

``` r
use_varb_baseline <- (AGE >= 25) & (AGE <= 55)
baseline <- subset(acs2017_ny,use_varb_baseline) 
detach()

attach(baseline)
```

    ## The following objects are masked from acs2017_ny:
    ## 
    ##     AfAm, AGE, Amindian, ANCESTR1, ANCESTR1D, ANCESTR2, ANCESTR2D,
    ##     Asian, below_150poverty, below_200poverty, below_povertyline, BPL,
    ##     BPLD, BUILTYR2, CITIZEN, CLASSWKR, CLASSWKRD, Commute_bus,
    ##     Commute_car, Commute_other, Commute_rail, Commute_subway, COSTELEC,
    ##     COSTFUEL, COSTGAS, COSTWATR, DEGFIELD, DEGFIELD2, DEGFIELD2D,
    ##     DEGFIELDD, DEPARTS, EDUC, educ_advdeg, educ_college, educ_hs,
    ##     educ_nohs, educ_somecoll, EDUCD, EMPSTAT, EMPSTATD, FAMSIZE,
    ##     female, foodstamps, FOODSTMP, FTOTINC, FUELHEAT, GQ,
    ##     has_AnyHealthIns, has_PvtHealthIns, HCOVANY, HCOVPRIV, HHINCOME,
    ##     Hisp_Cuban, Hisp_DomR, Hisp_Mex, Hisp_PR, HISPAN, HISPAND,
    ##     Hispanic, in_Bronx, in_Brooklyn, in_Manhattan, in_Nassau, in_NYC,
    ##     in_Queens, in_StatenI, in_Westchester, INCTOT, INCWAGE, IND,
    ##     LABFORCE, LINGISOL, MARST, MIGCOUNTY1, MIGPLAC1, MIGPUMA1,
    ##     MIGRATE1, MIGRATE1D, MORTGAGE, NCHILD, NCHLT5, OCC, OWNCOST,
    ##     OWNERSHP, OWNERSHPD, POVERTY, PUMA, PWPUMA00, RACE, race_oth,
    ##     RACED, RELATE, RELATED, RENT, ROOMS, SCHOOL, SEX, SSMC, TRANTIME,
    ##     TRANWORK, UHRSWORK, UNITSSTR, unmarried, veteran, VETSTAT,
    ##     VETSTATD, white, WKSWORK2, YRSUSA1
    ## 
    ## The following objects are masked from men_over_25:
    ## 
    ##     AfAm, AGE, Amindian, ANCESTR1, ANCESTR1D, ANCESTR2, ANCESTR2D,
    ##     Asian, below_150poverty, below_200poverty, below_povertyline, BPL,
    ##     BPLD, BUILTYR2, CITIZEN, CLASSWKR, CLASSWKRD, Commute_bus,
    ##     Commute_car, Commute_other, Commute_rail, Commute_subway, COSTELEC,
    ##     COSTFUEL, COSTGAS, COSTWATR, DEGFIELD, DEGFIELD2, DEGFIELD2D,
    ##     DEGFIELDD, DEPARTS, EDUC, educ_advdeg, educ_college, educ_hs,
    ##     educ_nohs, educ_somecoll, EDUCD, EMPSTAT, EMPSTATD, FAMSIZE,
    ##     female, foodstamps, FOODSTMP, FTOTINC, FUELHEAT, GQ,
    ##     has_AnyHealthIns, has_PvtHealthIns, HCOVANY, HCOVPRIV, HHINCOME,
    ##     Hisp_Cuban, Hisp_DomR, Hisp_Mex, Hisp_PR, HISPAN, HISPAND,
    ##     Hispanic, in_Bronx, in_Brooklyn, in_Manhattan, in_Nassau, in_NYC,
    ##     in_Queens, in_StatenI, in_Westchester, INCTOT, INCWAGE, IND,
    ##     LABFORCE, LINGISOL, MARST, MIGCOUNTY1, MIGPLAC1, MIGPUMA1,
    ##     MIGRATE1, MIGRATE1D, MORTGAGE, NCHILD, NCHLT5, OCC, OWNCOST,
    ##     OWNERSHP, OWNERSHPD, POVERTY, PUMA, PWPUMA00, RACE, race_oth,
    ##     RACED, RELATE, RELATED, RENT, ROOMS, SCHOOL, SEX, SSMC, TRANTIME,
    ##     TRANWORK, UHRSWORK, UNITSSTR, unmarried, veteran, VETSTAT,
    ##     VETSTATD, white, WKSWORK2, YRSUSA1

``` r
baseline$LABFORCE <- as.factor(baseline$LABFORCE)
levels(baseline$LABFORCE) <- c("Not in LF","in LF", "N/A")

#Establish categorical levels for each different type of marital situations.
baseline$MARST <- as.factor(baseline$MARST)
levels(baseline$MARST) <- c("married spouse present","married spouse absent","separated","divorced","widowed","never married")


model_logit1 <- glm(as.factor(LABFORCE) ~ AGE + I(AGE^2) + female + educ_hs + educ_college + MARST + NCHILD + NCHILD * female, family = binomial, data = baseline)

summary(model_logit1)
```

    ## 
    ## Call:
    ## glm(formula = as.factor(LABFORCE) ~ AGE + I(AGE^2) + female + 
    ##     educ_hs + educ_college + MARST + NCHILD + NCHILD * female, 
    ##     family = binomial, data = baseline)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.3287   0.3451   0.5337   0.6863   1.7204  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 1.2202169  0.2167758   5.629 1.81e-08 ***
    ## AGE                         0.0434558  0.0110344   3.938 8.21e-05 ***
    ## I(AGE^2)                   -0.0007184  0.0001356  -5.297 1.18e-07 ***
    ## female                      0.0062340  0.0243209   0.256 0.797701    
    ## educ_hs                    -0.4027094  0.0211081 -19.078  < 2e-16 ***
    ## educ_college                0.5844911  0.0278567  20.982  < 2e-16 ***
    ## MARSTmarried spouse absent -0.6746141  0.0498211 -13.541  < 2e-16 ***
    ## MARSTseparated             -0.2089841  0.0562785  -3.713 0.000204 ***
    ## MARSTdivorced               0.0492796  0.0373105   1.321 0.186569    
    ## MARSTwidowed               -0.4464205  0.0908438  -4.914 8.92e-07 ***
    ## MARSTnever married         -0.4669677  0.0250042 -18.676  < 2e-16 ***
    ## NCHILD                      0.4580149  0.0190467  24.047  < 2e-16 ***
    ## female:NCHILD              -0.7027588  0.0204833 -34.309  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 73866  on 77381  degrees of freedom
    ## Residual deviance: 69460  on 77369  degrees of freedom
    ## AIC: 69486
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
stargazer(model_logit1, type = "text")
```

    ## 
    ## ======================================================
    ##                                Dependent variable:    
    ##                            ---------------------------
    ##                                as.factor(LABFORCE)    
    ## ------------------------------------------------------
    ## AGE                                 0.043***          
    ##                                      (0.011)          
    ##                                                       
    ## I(AGE2)                             -0.001***         
    ##                                     (0.0001)          
    ##                                                       
    ## female                                0.006           
    ##                                      (0.024)          
    ##                                                       
    ## educ_hs                             -0.403***         
    ##                                      (0.021)          
    ##                                                       
    ## educ_college                        0.584***          
    ##                                      (0.028)          
    ##                                                       
    ## MARSTmarried spouse absent          -0.675***         
    ##                                      (0.050)          
    ##                                                       
    ## MARSTseparated                      -0.209***         
    ##                                      (0.056)          
    ##                                                       
    ## MARSTdivorced                         0.049           
    ##                                      (0.037)          
    ##                                                       
    ## MARSTwidowed                        -0.446***         
    ##                                      (0.091)          
    ##                                                       
    ## MARSTnever married                  -0.467***         
    ##                                      (0.025)          
    ##                                                       
    ## NCHILD                              0.458***          
    ##                                      (0.019)          
    ##                                                       
    ## female:NCHILD                       -0.703***         
    ##                                      (0.020)          
    ##                                                       
    ## Constant                            1.220***          
    ##                                      (0.217)          
    ##                                                       
    ## ------------------------------------------------------
    ## Observations                         77,382           
    ## Log Likelihood                     -34,730.070        
    ## Akaike Inf. Crit.                  69,486.140         
    ## ======================================================
    ## Note:                      *p<0.1; **p<0.05; ***p<0.01

  - The statistical significance of female, being married and divorced
    were not the best predictors of being in the labor force. There may
    be other factors surrounding marriage that obscure its effects on
    work.
  - One aforementioned assumption, the relationship between females and
    NCHILD had the expected result, that being female and having a child
    had a negative effect on one’s propensity to be in the labor force.
  - Workers with only a high school education were less likely to be in
    the labor force while college educated workers were more likely to
    be working.

### Estimate a probit model (details in Lecture Notes) and OLS, with the same X and Y variables.

``` r
model_probit <- glm(as.factor(LABFORCE) ~ AGE + I(AGE^2) + female + AfAm + Asian + Hispanic + white 
            + educ_hs  + educ_college  + MARST + NCHILD + NCHILD * female,
            family = binomial(link = probit), data = baseline)

model_ols <- lm(as.factor(LABFORCE) ~ AGE + I(AGE^2) + female + AfAm + Asian + Hispanic + white 
            + educ_hs  + educ_college  + MARST + NCHILD + NCHILD * female, data = baseline)
```

    ## Warning in model.response(mf, "numeric"): using type = "numeric" with a factor
    ## response will be ignored

    ## Warning in Ops.factor(y, z$residuals): '-' not meaningful for factors

``` r
stargazer(model_logit1, model_probit, model_ols, type = "text")
```

    ## 
    ## ==========================================================
    ##                                  Dependent variable:      
    ##                            -------------------------------
    ##                                  as.factor(LABFORCE)      
    ##                             logistic     probit      OLS  
    ##                                (1)         (2)       (3)  
    ## ----------------------------------------------------------
    ## AGE                         0.043***    0.031***    0.007 
    ##                              (0.011)     (0.006)          
    ##                                                           
    ## I(AGE2)                     -0.001***  -0.0005***  -0.0001
    ##                             (0.0001)    (0.0001)          
    ##                                                           
    ## female                        0.006      -0.008    -0.0003
    ##                              (0.024)     (0.014)          
    ##                                                           
    ## AfAm                                    -0.113***  -0.033 
    ##                                          (0.025)          
    ##                                                           
    ## Asian                                   -0.106***  -0.023 
    ##                                          (0.027)          
    ##                                                           
    ## Hispanic                                -0.086***  -0.021 
    ##                                          (0.017)          
    ##                                                           
    ## white                                   0.083***    0.021 
    ##                                          (0.021)          
    ##                                                           
    ## educ_hs                     -0.403***   -0.229***  -0.063 
    ##                              (0.021)     (0.012)          
    ##                                                           
    ## educ_college                0.584***    0.309***    0.066 
    ##                              (0.028)     (0.015)          
    ##                                                           
    ## MARSTmarried spouse absent  -0.675***   -0.344***  -0.100 
    ##                              (0.050)     (0.030)          
    ##                                                           
    ## MARSTseparated              -0.209***   -0.095***  -0.025 
    ##                              (0.056)     (0.033)          
    ##                                                           
    ## MARSTdivorced                 0.049       0.018     0.007 
    ##                              (0.037)     (0.021)          
    ##                                                           
    ## MARSTwidowed                -0.446***   -0.249***  -0.075 
    ##                              (0.091)     (0.054)          
    ##                                                           
    ## MARSTnever married          -0.467***   -0.245***  -0.067 
    ##                              (0.025)     (0.015)          
    ##                                                           
    ## NCHILD                      0.458***    0.217***    0.035 
    ##                              (0.019)     (0.009)          
    ##                                                           
    ## female:NCHILD               -0.703***   -0.362***  -0.078 
    ##                              (0.020)     (0.010)          
    ##                                                           
    ## Constant                    1.220***    0.629***    1.761 
    ##                              (0.217)     (0.125)          
    ##                                                           
    ## ----------------------------------------------------------
    ## Observations                 77,382      77,382    77,382 
    ## Log Likelihood             -34,730.070 -34,607.570        
    ## Akaike Inf. Crit.          69,486.140  69,249.130         
    ## ==========================================================
    ## Note:                          *p<0.1; **p<0.05; ***p<0.01

Logit and Probit models have similar predicting power, both do not
estimate being married or divorced as good predictors of being in the
labor force.

The OLS regression displays no standard error terms (not that they would
be useful) given that they were regressed with a y variable of binomial
nature. The OLS as shows very low estimates for our data showing how
imaccurate it is at modeling on a 0 or 1 basis.

# Predicted Values of the baseline Model

``` r
baseline$LABFORCE <- droplevels(baseline$LABFORCE)

NNobs <- length(baseline$LABFORCE)
set.seed(123456789) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
baseline_graph <-subset(baseline,graph_obs)  

 plot(LABFORCE ~ jitter(AGE, factor = 2), pch = 16, ylim = c(0,1), data = baseline_graph, main = "Labor Force Participation Measured by Age", xlab = "Age", ylab = "Labor Force Status", col = c("grey","blue"))

to_be_predicted <- data.frame(AGE = 25:55, MARST = "never married", NCHILD = 1, female = 0, educ_college =1, educ_hs=0 )
to_be_predicted$yhat <- predict(model_logit1, newdata = to_be_predicted)



lines(yhat ~ AGE, data = to_be_predicted)
```

![](Homework-6_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> The
above graph shows many trends highlighted while we looked at men in the
labor force.

We saw that labor force paticipation was highest in the 25 - 35 area
when we can see here and a general decline as men age. The data, does
however, show some spikes and we have already established there should
be a steady decline,larger sample size or more variables may help
extrapolate this descrepancy.
