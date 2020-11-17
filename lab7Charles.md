Lab 7
================

``` r
#Load Required packages
library(stargazer)
library(kableExtra)
library(knitr)
library(tidyverse)
library(jtools)
library(sandwich)
library(np)
library(standardize)
```

## Econ B2000, MA Econometrics

## Charles Reid, the Colin Powell School at the City College of New York, CUNY

## Fall 2020

# Health Insurance

Estimate a variety of models to try to predict if a person has health
insurance. We want to understand what factors make an adult more likely
to have health insurance.

Download the NHIS data and load it into R.

``` r
load("NHIS_2014.RData")
```

The data includes a variety of people of all ages.

Some of the variable: \* disabl\_limit codes 0/1 if the person has any
limitations from a disability. \* RRP codes relationship to the person
answering the question \* HHX, FMX, and FPX just are ID numbers; SCHIP
is a children’s healthcare system \* sptn\_medical is a factor telling
how much the person spent on medical bills

There are more than 1000 different variables.

The person’s earnings could use a bit of recoding,

``` r
data_use1$earn_lastyr <- as.factor(data_use1$ERNYR_P)
levels(data_use1$earn_lastyr) <- c("0","$01-$4999","$5000-$9999","$10000-$14999","$15000-$19999","$20000-$24999","$25000-$34999","$35000-$44999","$45000-$54999","$55000-$64999","$65000-$74999","$75000 and over",NA,NA,NA)
```

Decide on how you’re defining your subgroup (all adults? Within certain
age? Other?) then find some basic statistics – what fraction are not
covered? (Later go back to look at simple stats for subgroups to see if
there are sharp differences.)

# Defining and Analyzing the Subset

The subset below looks at people between the ages 25 - 55

``` r
#subset data between ages 25 - 55
dat2 <- subset(data_use1, ((AGE_P >= 25) & (AGE_P <= 55)))

#apply subset to those not cover
covprop <- dat2$NOTCOV 

#apply subset to those covered
covtable <- table(covprop)

#combine subsets into a table
agecovtable <- table(dat2$AGE_P, dat2$NOTCOV)

#name columns in this new table
colnames(agecovtable) <- c("Covered", "Not Covered")

#add margins to the new table with columns
agetable <- addmargins(agecovtable)

#rename columns
names(covtable) <- c("Covered", "Not Covered")

#add margins
t1 <- addmargins(covtable)

#value in each cell divided my the amount of cells 
t2 <- prop.table(covtable)

#Create Kable Tables
kables(list(kable(t1, align = "l", col.names = c("Coverage", "Total")),main="Total Amount of People Covered/Not Covered"))
```

<table class="kable_wrapper">

<tbody>

<tr>

<td>

<table>

<thead>

<tr>

<th style="text-align:left;">

Coverage

</th>

<th style="text-align:left;">

Total

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Covered

</td>

<td style="text-align:left;">

37442

</td>

</tr>

<tr>

<td style="text-align:left;">

Not Covered

</td>

<td style="text-align:left;">

8795

</td>

</tr>

<tr>

<td style="text-align:left;">

Sum

</td>

<td style="text-align:left;">

46237

</td>

</tr>

</tbody>

</table>

</td>

<td>

Total Amount of People Covered/Not Covered

</td>

</tr>

</tbody>

</table>

``` r
  kables(list(kable(t2, col.names = c("Coverage", "Proportion")),main="Percentage of People Covered/NOT Covered"))
```

<table class="kable_wrapper">

<tbody>

<tr>

<td>

<table>

<thead>

<tr>

<th style="text-align:left;">

Coverage

</th>

<th style="text-align:right;">

Proportion

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Covered

</td>

<td style="text-align:right;">

0.8097844

</td>

</tr>

<tr>

<td style="text-align:left;">

Not Covered

</td>

<td style="text-align:right;">

0.1902156

</td>

</tr>

</tbody>

</table>

</td>

<td>

Percentage of People Covered/NOT Covered

</td>

</tr>

</tbody>

</table>

Start with logit like last week then some different models: Random
Forest, Support Vector Machines, and Elastic Net (which is not optimal
for a 0/1 dependent but it works for a demonstration).

Below we can see that the more positive coefficient entail someone is
less likely to be covered due the effects of that particular parameter.

``` r
model_logit1 <- glm(NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + RaceOther  
                    + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
                    + married + widowed + divorc_sep + veteran_stat + REGION + region_born,
                    family = binomial, data = dat2)

summary(model_logit1)
```

    ## 
    ## Call:
    ## glm(formula = NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + 
    ##     RaceOther + Hispanic + educ_hs + educ_smcoll + educ_as + 
    ##     educ_bach + educ_adv + married + widowed + divorc_sep + veteran_stat + 
    ##     REGION + region_born, family = binomial, data = dat2)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9575  -0.6385  -0.4320  -0.2393   2.9646  
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                   -0.9711397  0.2853035  -3.404 0.000664 ***
    ## AGE_P                          0.0311287  0.0148302   2.099 0.035816 *  
    ## I(AGE_P^2)                    -0.0007245  0.0001860  -3.896 9.77e-05 ***
    ## female                        -0.2987060  0.0265148 -11.266  < 2e-16 ***
    ## AfAm                          -0.1903598  0.0399682  -4.763 1.91e-06 ***
    ## Asian                         -0.1271264  0.0908938  -1.399 0.161926    
    ## RaceOther                      0.3640276  0.0816294   4.460 8.21e-06 ***
    ## Hispanic                       0.2319377  0.0434142   5.342 9.17e-08 ***
    ## educ_hs                       -0.2979181  0.0362223  -8.225  < 2e-16 ***
    ## educ_smcoll                   -0.6295006  0.0427272 -14.733  < 2e-16 ***
    ## educ_as                       -0.8456507  0.0497626 -16.994  < 2e-16 ***
    ## educ_bach                     -1.5671365  0.0507924 -30.854  < 2e-16 ***
    ## educ_adv                      -2.2067905  0.0800658 -27.562  < 2e-16 ***
    ## married                       -0.7303703  0.0304993 -23.947  < 2e-16 ***
    ## widowed                       -0.0218213  0.1335749  -0.163 0.870232    
    ## divorc_sep                    -0.1036226  0.0442037  -2.344 0.019068 *  
    ## veteran_stat                  -0.5426507  0.0707190  -7.673 1.68e-14 ***
    ## REGIONMidwest                  0.2835837  0.0487171   5.821 5.85e-09 ***
    ## REGIONSouth                    0.6903527  0.0421870  16.364  < 2e-16 ***
    ## REGIONWest                     0.2398669  0.0445597   5.383 7.32e-08 ***
    ## region_bornMex Cent Am Caribb  1.1290736  0.0469881  24.029  < 2e-16 ***
    ## region_bornS Am                0.9776730  0.0999178   9.785  < 2e-16 ***
    ## region_bornEur                 0.3557441  0.1235638   2.879 0.003989 ** 
    ## region_bornformer USSR         0.8994373  0.2494383   3.606 0.000311 ***
    ## region_bornAfrica              0.7660519  0.1253941   6.109 1.00e-09 ***
    ## region_bornMidE                0.6970800  0.1979245   3.522 0.000428 ***
    ## region_bornIndia subc          0.6843085  0.1462695   4.678 2.89e-06 ***
    ## region_bornAsia                0.8439813  0.1368071   6.169 6.87e-10 ***
    ## region_bornSE Asia             0.3106985  0.1247501   2.491 0.012754 *  
    ## region_bornElsewhere           0.2215335  0.1946339   1.138 0.255034    
    ## region_bornunknown            -0.0311133  0.2177011  -0.143 0.886355    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 44992  on 46236  degrees of freedom
    ## Residual deviance: 37762  on 46206  degrees of freedom
    ## AIC: 37824
    ## 
    ## Number of Fisher Scoring iterations: 5

### SMV - Support Vector Machines

Support vector machines (SVMs) are a set of supervised learning methods
used for classification, regression and outliers detection. Using a
hyperplane to maximize the margin from both series of obsersations.

Some of the estimation procedures are not as tolerant about factors so
we need to set those as dummies. Some are also intolerant of NA values.
I’ll show the code for the basic set of explanatory variables, which you
can modify as you see fit.

``` r
d_region <- data.frame(model.matrix(~ dat2$REGION))
d_region_born <- data.frame(model.matrix(~ factor(dat2$region_born))) 

# snips any with zero in the subgroup
dat_for_analysis_sub <- data.frame(
  dat2$NOTCOV,
  dat2$AGE_P,
  dat2$female,
  dat2$AfAm,
  dat2$Asian,
  dat2$RaceOther,
  dat2$Hispanic,
  dat2$educ_hs,
  dat2$educ_smcoll,
  dat2$educ_as,
  dat2$educ_bach,
  dat2$educ_adv,
  dat2$married,
  dat2$widowed,
  dat2$divorc_sep,
  d_region[,2:4],
  d_region_born[,2:12]) # need [] since model.matrix includes intercept term

names(dat_for_analysis_sub) <- c("NOTCOV",
                                 "Age",
                                 "female",
                                 "AfAm",
                                 "Asian",
                                 "RaceOther",
                                 "Hispanic",
                                 "educ_hs",
                                 "educ_smcoll",
                                 "educ_as",
                                 "educ_bach",
                                 "educ_adv",
                                 "married",
                                 "widowed",
                                 "divorc_sep",
                                 "Region.Midwest",
                                 "Region.South",
                                 "Region.West",
                                 "born.Mex.CentAm.Carib",
                                 "born.S.Am",
                                 "born.Eur",
                                 "born.f.USSR",
                                 "born.Africa",
                                 "born.MidE",
                                 "born.India.subc",
                                 "born.Asia",
                                 "born.SE.Asia",
                                 "born.elsewhere",
                                 "born.unknown")
```

# Standardized Data Object

(check what it does\! run summary(sobj$data) ) and split into training
and test sets. Summary(restrict\_1) will tell you how many are in the
training set vs test.

The training set composed of 15% of the original observations. Factors
of the parameter region may change the output of the model.

``` r
require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$NOTCOV)
# restrict_1 <- as.logical(round(runif(NN,min=0,max=0.6))) # use fraction as training data

#select 15% of training set
restrict_1 <- (runif(NN) < 0.15) 

#summarize data
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)
sobj <- standardize(NOTCOV ~ Age + female + AfAm + Asian + RaceOther + Hispanic + 
                      educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv + 
                      married + widowed + divorc_sep + 
                      Region.Midwest + Region.South + Region.West + 
                      born.Mex.CentAm.Carib + born.S.Am + born.Eur + born.f.USSR + 
                      born.Africa + born.MidE + born.India.subc + born.Asia + 
                      born.SE.Asia + born.elsewhere + born.unknown, dat_train, family = binomial)

s_dat_test <- predict(sobj, dat_test)
```

Then start with some models. I’ll give code for the Linear Probability
Model (ie good ol’ OLS) and logit, to show how to call those with the
standarized object.

``` r
# LPM (OLS)
model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)


#PREDICT
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > 0.6)
table(pred = pred_model_lpm1, true = dat_test$NOTCOV)


# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)

#PREDICT
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$NOTCOV)
```

Standardized variables seem to be good for measuring the contribution of
each pridictor, in a standardized way, that is across different groups
of measurement.On the other hand the logit regression provides us the
probability of the condition of not having health insurance, given each
variable. The logit regression is more forthcoming,in a directional
sense, as their probabilities are given on a \[0,1\] interval which is
easier to quantify.

# The Random Forest Model

Here is code for a Random Forest, which takes a bit of computing,

``` r
require('randomForest')
set.seed(54321)
model_randFor <- randomForest(as.factor(NOTCOV) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
print(model_randFor)
round(importance(model_randFor),2)
varImpPlot(model_randFor)


# look at confusion matrix for this too
pred_model1 <- predict(model_randFor,  s_dat_test)
table(pred = pred_model1, true = dat_test$NOTCOV)
```

Note that the estimation prints out a Confusion Matrix first but that’s
within the training data; the later one calculates how well it does on
the test data.

Next is Support Vector Machines. First it tries to find optimal tuning
parameter, next uses those optimal values to train. (Tuning takes a long
time so skip for now\!)

``` r
require(e1071)
# tuned_parameters <- tune.svm(as.factor(NOTCOV) ~ ., data = sobj$data, gamma = 10^(-3:0), cost = 10^(-2:1)) 
# summary(tuned_parameters)
# figure best parameters and input into next
svm.model <- svm(as.factor(NOTCOV) ~ ., data = sobj$data, cost = 10, gamma = 0.1)
svm.pred <- predict(svm.model, s_dat_test)
table(pred = svm.pred, true = dat_test$NOTCOV)
```

Here is Elastic Net. It combines LASSO with Ridge and the alpha
parameter (from 0 to 1) determines the relative weight. Begin with alpha
= 1 so just LASSO.

``` r
# Elastic Net
require(glmnet)
model1_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$NOTCOV) 
# default is alpha = 1, lasso

par(mar=c(4.5,4.5,1,4))
plot(model1_elasticnet)
vnat=coef(model1_elasticnet)
vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
axis(4, at=vnat,line=-.5,label=names(sobj$data[,-1]),las=1,tick=FALSE, cex.axis=0.5) 

plot(model1_elasticnet, xvar = "lambda")
plot(model1_elasticnet, xvar = "dev", label = TRUE)
print(model1_elasticnet)

cvmodel1_elasticnet = cv.glmnet(data.matrix(sobj$data[,-1]),data.matrix(sobj$data$NOTCOV)) 
cvmodel1_elasticnet$lambda.min
log(cvmodel1_elasticnet$lambda.min)
coef(cvmodel1_elasticnet, s = "lambda.min")

pred1_elasnet <- predict(model1_elasticnet, newx = data.matrix(s_dat_test), s = cvmodel1_elasticnet$lambda.min)
pred_model1_elasnet <- (pred1_elasnet < mean(pred1_elasnet)) 
table(pred = pred_model1_elasnet, true = dat_test$NOTCOV)

model2_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$NOTCOV, alpha = 0) 
# or try different alpha values to see if you can improve
```
