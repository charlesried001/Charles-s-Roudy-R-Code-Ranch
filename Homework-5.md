Homework 5, Lab 5, Econ B2000
================
Group: Charles Reid, Christopher Tinevra, Group Members: Akimawe Kadiri,
Nicole Kerrison, Mostafa Ragheb,
11/3/2020

## Model 1: Income Based on Education and Ethnicity

In the subset for model 1, income wages will be estimated depending on

educaltion levels:

  - High school degree
  - College degree
  - Advance degree

& ethnicity:

  - White
  - African American
  - Hispanics

The sample for this regression includes females with ages ranging from
25 to 65 years old and are currently part of the labor force and are
fully employed with (35 hour work week).

``` r
load("acs2017_ny_data.RData")
suppressMessages(attach(acs2017_ny))
use_varb <- (AGE >= 25) & (AGE <= 65) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35) & (female == 1) 
dat_use <- subset(acs2017_ny,use_varb)
detach()
```

The model will be using a 2nd degree polynomial of the Age parameter

``` r
suppressMessages(attach(dat_use))
model1 <- lm(INCWAGE ~ AGE + + I(AGE^2) + educ_hs + educ_college + educ_advdeg + white + AfAm + Hispanic)
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = INCWAGE ~ AGE + +I(AGE^2) + educ_hs + educ_college + 
    ##     educ_advdeg + white + AfAm + Hispanic)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -103008  -25529   -8136   11480  608946 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -49518.717   6040.617  -8.198 2.56e-16 ***
    ## AGE            3797.762    278.609  13.631  < 2e-16 ***
    ## I(AGE^2)        -35.145      3.122 -11.257  < 2e-16 ***
    ## educ_hs       -6770.665   1036.138  -6.535 6.49e-11 ***
    ## educ_college  29877.878   1011.847  29.528  < 2e-16 ***
    ## educ_advdeg   48276.166   1046.577  46.128  < 2e-16 ***
    ## white          1653.394   1028.571   1.607 0.107963    
    ## AfAm          -4895.436   1351.567  -3.622 0.000293 ***
    ## Hispanic      -6226.070   1138.101  -5.471 4.53e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 59730 on 27183 degrees of freedom
    ## Multiple R-squared:  0.1295, Adjusted R-squared:  0.1292 
    ## F-statistic: 505.4 on 8 and 27183 DF,  p-value: < 2.2e-16

``` r
suppressMessages(require(stargazer))
stargazer(model1, type = "text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                               INCWAGE          
    ## -----------------------------------------------
    ## AGE                        3,797.762***        
    ##                              (278.609)         
    ##                                                
    ## I(AGE2)                     -35.145***         
    ##                               (3.122)          
    ##                                                
    ## educ_hs                    -6,770.665***       
    ##                             (1,036.138)        
    ##                                                
    ## educ_college               29,877.880***       
    ##                             (1,011.847)        
    ##                                                
    ## educ_advdeg                48,276.170***       
    ##                             (1,046.577)        
    ##                                                
    ## white                        1,653.394         
    ##                             (1,028.571)        
    ##                                                
    ## AfAm                       -4,895.436***       
    ##                             (1,351.567)        
    ##                                                
    ## Hispanic                   -6,226.070***       
    ##                             (1,138.101)        
    ##                                                
    ## Constant                  -49,518.720***       
    ##                             (6,040.617)        
    ##                                                
    ## -----------------------------------------------
    ## Observations                  27,192           
    ## R2                             0.129           
    ## Adjusted R2                    0.129           
    ## Residual Std. Error   59,733.910 (df = 27183)  
    ## F Statistic         505.427*** (df = 8; 27183) 
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
detach()
```

The regression analysis and coefficients show a positive correlation for
income wages as the age increases, a negative effect on wages for those
who have high school diplomas only (in comparison to those who have
higher levels of education). It also tells researhers that being African
American or Hispanics has a negative effect on wages from income. Being
white is not a significant indicator of having a high income, HOWEVER
the coefficient shows that being white does correlate with having a
higher income which may suggest that the model fails to capture some
facet that may help whites attain a higher income such as generational
wealth, connections etc.

With an R squared of 0.129, the results of our regression is not
completely explained by the predictors included. We can attribute this
to 2 reasons:

  - The data may not perfectly follwing a linear structure.
  - In social sciences it is not possible to include all the relevant
    predictors to explain an outcome variable. This might lead to a
    lower R2 value. A low R squraed can be useful to decide of the model
    is the best fit for this data.

From the above regression a predictive model can be drawn using 50% of
the data from the 27,192 observations in the data - subset.

``` r
NNobs <- length(INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.5)
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ AGE, pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)

to_be_predicted1 <- data.frame(AGE = 25:65, female = 1,white = 1, Hispanic = 0, AfAm =0, educ_hs = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted1$yhat <- predict(model1, newdata = to_be_predicted1)

lines(yhat ~ AGE, data = to_be_predicted1)
```

![](Homework-5_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
initmax <- max(to_be_predicted1$yhat)
```

Within the plot for the predicted values the above code pulls out the
peak predicted value of 84609.87 for INCWAGES.

## Model 2: Adding Polynomial Regression on Age

Model 2 illustrates the linear regression ran above, but with a higher
degree polynomial on the Age parameter. When higher order polynomials,
(Age^3), (Age^4) and (Age^5) etc., to the regression, the absolute
values of the corresponding coefficients get progressively smaller.
Rsqaured, which was at 0.129 while under a 2nd degree polynomial is now
locked at .130 once enter 3rd and greater polynomials(larger numbers
where tried), this developement insinuates that while polynomial
regression can improve a regression, it may have diminishing returns or
other forms of transformation and refinement are needed to help improve
the accuracy of this particular subset and its predicting power.

``` r
suppressMessages(attach(dat_use))
model2 <- lm(INCWAGE ~ AGE + I(AGE^2)+ I(AGE^3)+ I(AGE^4)+ I(AGE^5)  + educ_hs + educ_college + educ_advdeg + white + AfAm + Hispanic)
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = INCWAGE ~ AGE + I(AGE^2) + I(AGE^3) + I(AGE^4) + 
    ##     I(AGE^5) + educ_hs + educ_college + educ_advdeg + white + 
    ##     AfAm + Hispanic)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -102780  -25514   -8198   11576  610581 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   8.178e+05  4.056e+05   2.016 0.043785 *  
    ## AGE          -1.063e+05  4.948e+04  -2.149 0.031682 *  
    ## I(AGE^2)      5.350e+03  2.356e+03   2.270 0.023200 *  
    ## I(AGE^3)     -1.270e+02  5.484e+01  -2.315 0.020595 *  
    ## I(AGE^4)      1.447e+00  6.243e-01   2.317 0.020492 *  
    ## I(AGE^5)     -6.390e-03  2.785e-03  -2.294 0.021783 *  
    ## educ_hs      -6.731e+03  1.036e+03  -6.498 8.27e-11 ***
    ## educ_college  2.987e+04  1.012e+03  29.527  < 2e-16 ***
    ## educ_advdeg   4.804e+04  1.049e+03  45.805  < 2e-16 ***
    ## white         1.737e+03  1.029e+03   1.689 0.091198 .  
    ## AfAm         -4.865e+03  1.351e+03  -3.600 0.000318 ***
    ## Hispanic     -6.264e+03  1.138e+03  -5.505 3.72e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 59720 on 27180 degrees of freedom
    ## Multiple R-squared:  0.1301, Adjusted R-squared:  0.1298 
    ## F-statistic: 369.6 on 11 and 27180 DF,  p-value: < 2.2e-16

``` r
suppressMessages(require(stargazer))
stargazer(model2, type = "text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                               INCWAGE          
    ## -----------------------------------------------
    ## AGE                       -106,297.700**       
    ##                            (49,475.030)        
    ##                                                
    ## I(AGE2)                     5,349.783**        
    ##                             (2,356.464)        
    ##                                                
    ## I(AGE3)                     -126.972**         
    ##                              (54.837)          
    ##                                                
    ## I(AGE4)                       1.447**          
    ##                               (0.624)          
    ##                                                
    ## I(AGE5)                      -0.006**          
    ##                               (0.003)          
    ##                                                
    ## educ_hs                    -6,731.209***       
    ##                             (1,035.857)        
    ##                                                
    ## educ_college               29,868.500***       
    ##                             (1,011.565)        
    ##                                                
    ## educ_advdeg                48,043.200***       
    ##                             (1,048.858)        
    ##                                                
    ## white                       1,737.401*         
    ##                             (1,028.551)        
    ##                                                
    ## AfAm                       -4,865.105***       
    ##                             (1,351.236)        
    ##                                                
    ## Hispanic                   -6,264.325***       
    ##                             (1,137.932)        
    ##                                                
    ## Constant                   817,797.400**       
    ##                            (405,605.700)       
    ##                                                
    ## -----------------------------------------------
    ## Observations                  27,192           
    ## R2                             0.130           
    ## Adjusted R2                    0.130           
    ## Residual Std. Error   59,715.370 (df = 27180)  
    ## F Statistic         369.619*** (df = 11; 27180)
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
NNobs <- length(INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1)
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ AGE, pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)

to_be_predicted2 <- data.frame(AGE = 25:65, female = 1, white = 1, Hispanic = 0, AfAm = 0,educ_hs = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model2, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)
```

![](Homework-5_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
detach()
```

The coefficient estimates for Model 2 linear regression continues to
demonstrate multiple income wages depending on educaltion level (high
school degree, college degree, Advance degree), ethnicity (white,
African American & Hispanics) but also taking into consideration age
squared. The sample for this regression includes females with ages
ranging from 25 to 65 years old and are currently part of the labor
force and are fully employed with (35 hour work week). The graph above
shows a positive correlation for income wages as the age increases, and
regression line refers to the predicted values taking into consideration
white females with a college degree. However the regression slope of the
graph is positive as age increases, then decreases, indicating
diminishing returns for wages as workers reach retirement age (62 years
old).

## Model 3: Log and Other Transformations

Model 3 illustrates the linear regression of income wages based on
education level, log of age, log of age squared, and ethnicity.

``` r
suppressMessages(attach(dat_use))
model3 <- lm(INCWAGE ~ log(AGE) + I(log(AGE^2)) + educ_hs + educ_college + educ_advdeg + white + AfAm + Hispanic)
summary(model3)
```

    ## 
    ## Call:
    ## lm(formula = INCWAGE ~ log(AGE) + I(log(AGE^2)) + educ_hs + educ_college + 
    ##     educ_advdeg + white + AfAm + Hispanic)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -108033  -25837   -8385   11222  605381 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -69312       5306 -13.063  < 2e-16 ***
    ## log(AGE)         30526       1372  22.258  < 2e-16 ***
    ## I(log(AGE^2))       NA         NA      NA       NA    
    ## educ_hs          -6836       1037  -6.589 4.52e-11 ***
    ## educ_college     29656       1013  29.281  < 2e-16 ***
    ## educ_advdeg      48595       1047  46.413  < 2e-16 ***
    ## white             1322       1029   1.284 0.199026    
    ## AfAm             -4920       1353  -3.635 0.000278 ***
    ## Hispanic         -5986       1139  -5.254 1.50e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 59810 on 27184 degrees of freedom
    ## Multiple R-squared:  0.1272, Adjusted R-squared:  0.127 
    ## F-statistic: 565.9 on 7 and 27184 DF,  p-value: < 2.2e-16

``` r
suppressMessages(require(stargazer))
stargazer(model3, type = "text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                               INCWAGE          
    ## -----------------------------------------------
    ## log(AGE)                   30,525.940***       
    ##                             (1,371.484)        
    ##                                                
    ## I(log(AGE2))                                   
    ##                                                
    ##                                                
    ## educ_hs                    -6,835.500***       
    ##                             (1,037.447)        
    ##                                                
    ## educ_college               29,655.810***       
    ##                             (1,012.797)        
    ##                                                
    ## educ_advdeg                48,595.430***       
    ##                             (1,047.025)        
    ##                                                
    ## white                        1,321.813         
    ##                             (1,029.158)        
    ##                                                
    ## AfAm                       -4,919.818***       
    ##                             (1,353.317)        
    ##                                                
    ## Hispanic                   -5,986.012***       
    ##                             (1,139.230)        
    ##                                                
    ## Constant                  -69,311.550***       
    ##                             (5,305.787)        
    ##                                                
    ## -----------------------------------------------
    ## Observations                  27,192           
    ## R2                             0.127           
    ## Adjusted R2                    0.127           
    ## Residual Std. Error   59,811.340 (df = 27184)  
    ## F Statistic         565.946*** (df = 7; 27184) 
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
detach()

NNobs <- length(INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1)
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ (AGE), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)

to_be_predicted3 <- data.frame(AGE = 25:65, female = 1, white = 1, Hispanic =0, AfAm = 0,educ_hs = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted3$yhat <- predict(model3, newdata = to_be_predicted3)
```

    ## Warning in predict.lm(model3, newdata = to_be_predicted3): prediction from a
    ## rank-deficient fit may be misleading

``` r
lines(yhat ~ AGE, data = to_be_predicted3)
```

![](Homework-5_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Model 3 expands on the conditions of model 1 but this time, the
regression model is based on log of age and log of age squared. However,
there is no reason to use Log on Age or Log on Age^2 given that age is a
discrete variable mostly on intervals between 1 to 100. Log of a given
number (x) is the exponent to which another fixed number base (b), must
be raised, to produce that number (x). Therefore, it is not practical to
present age in log terms. Log is helpful to determine the percentage
change in something and is more suitable for measuring values related to
income, distance and volume.

Model 4 illustrates the linear regression of income wages based on
education level, age, polynomial of ages (Age^exp 2,3,4), and ethnicity.

``` r
suppressMessages(attach(dat_use))
model4 <- lm(INCWAGE ~ AGE + I(AGE^2) + I(AGE^3) + I(AGE^4) +I(AGE^5) + I(AGE^6) + educ_hs + educ_college+ educ_advdeg + white + AfAm + Hispanic)

summary(model4)
```

    ## 
    ## Call:
    ## lm(formula = INCWAGE ~ AGE + I(AGE^2) + I(AGE^3) + I(AGE^4) + 
    ##     I(AGE^5) + I(AGE^6) + educ_hs + educ_college + educ_advdeg + 
    ##     white + AfAm + Hispanic)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -102325  -25515   -8205   11575  611159 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   3.116e+06  1.700e+06   1.833 0.066830 .  
    ## AGE          -4.472e+05  2.498e+05  -1.790 0.073472 .  
    ## I(AGE^2)      2.596e+04  1.499e+04   1.732 0.083353 .  
    ## I(AGE^3)     -7.772e+02  4.703e+02  -1.653 0.098443 .  
    ## I(AGE^4)      1.275e+01  8.144e+00   1.566 0.117463    
    ## I(AGE^5)     -1.091e-01  7.387e-02  -1.478 0.139541    
    ## I(AGE^6)      3.820e-04  2.744e-04   1.392 0.163927    
    ## educ_hs      -6.742e+03  1.036e+03  -6.508 7.72e-11 ***
    ## educ_college  2.985e+04  1.012e+03  29.513  < 2e-16 ***
    ## educ_advdeg   4.804e+04  1.049e+03  45.801  < 2e-16 ***
    ## white         1.722e+03  1.029e+03   1.674 0.094132 .  
    ## AfAm         -4.884e+03  1.351e+03  -3.614 0.000302 ***
    ## Hispanic     -6.273e+03  1.138e+03  -5.513 3.56e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 59710 on 27179 degrees of freedom
    ## Multiple R-squared:  0.1302, Adjusted R-squared:  0.1298 
    ## F-statistic:   339 on 12 and 27179 DF,  p-value: < 2.2e-16

``` r
suppressMessages(require(stargazer))
stargazer(model4, type = "text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                               INCWAGE          
    ## -----------------------------------------------
    ## AGE                        -447,159.300*       
    ##                            (249,815.700)       
    ##                                                
    ## I(AGE2)                     25,956.170*        
    ##                            (14,989.570)        
    ##                                                
    ## I(AGE3)                      -777.189*         
    ##                              (470.309)         
    ##                                                
    ## I(AGE4)                       12.750           
    ##                               (8.144)          
    ##                                                
    ## I(AGE5)                       -0.109           
    ##                               (0.074)          
    ##                                                
    ## I(AGE6)                       0.0004           
    ##                              (0.0003)          
    ##                                                
    ## educ_hs                    -6,741.937***       
    ##                             (1,035.868)        
    ##                                                
    ## educ_college               29,854.780***       
    ##                             (1,011.596)        
    ##                                                
    ## educ_advdeg                48,038.600***       
    ##                             (1,048.845)        
    ##                                                
    ## white                       1,721.916*         
    ##                             (1,028.593)        
    ##                                                
    ## AfAm                       -4,884.156***       
    ##                             (1,351.282)        
    ##                                                
    ## Hispanic                   -6,273.204***       
    ##                             (1,137.931)        
    ##                                                
    ## Constant                  3,115,841.000*       
    ##                           (1,699,961.000)      
    ##                                                
    ## -----------------------------------------------
    ## Observations                  27,192           
    ## R2                             0.130           
    ## Adjusted R2                    0.130           
    ## Residual Std. Error   59,714.340 (df = 27179)  
    ## F Statistic         338.991*** (df = 12; 27179)
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
detach()

NNobs <- length(INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1)
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ (AGE), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
to_be_predicted4 <- data.frame(AGE = 25:65, female = 1, white = 1, Hispanic =0, AfAm = 0,educ_hs = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted4$yhat <- predict(model4, newdata = to_be_predicted4)

lines(yhat ~ AGE, data = to_be_predicted4)
```

![](Homework-5_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Model 4 expands on the conditions of model 2 but this time, the
regression model includes age as a polynomial terms where age variable
raised by different exponents (2,3,4). Displaying age as a polynomial
shows during the predicted analysis that as age increases, wages will
increase and flatten out at retirement. If we express the dummy
variables in terms of polynomials it will not be effective in the
regression because dummy variables are binary and only provide values of
zero and one.

Lastly, Model 5 expands on the conditions of model 2 but this time, the
regression model is based on log of dependent variable which is income
wages.

``` r
suppressMessages(attach(dat_use))
model5 <- lm(log1p(INCWAGE) ~ AGE + I(AGE^2) + I(AGE^3) + I(AGE^4) + educ_hs + educ_college + educ_advdeg + white + AfAm + Hispanic)
summary(model5)
```

    ## 
    ## Call:
    ## lm(formula = log1p(INCWAGE) ~ AGE + I(AGE^2) + I(AGE^3) + I(AGE^4) + 
    ##     educ_hs + educ_college + educ_advdeg + white + AfAm + Hispanic)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.0858  -0.0621   0.3067   0.6693   3.4994 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   5.887e+00  3.214e+00   1.832 0.066980 .  
    ## AGE           3.622e-01  3.120e-01   1.161 0.245633    
    ## I(AGE^2)     -1.167e-02  1.102e-02  -1.058 0.289969    
    ## I(AGE^3)      1.686e-04  1.684e-04   1.001 0.316816    
    ## I(AGE^4)     -9.131e-07  9.403e-07  -0.971 0.331535    
    ## educ_hs      -1.201e-01  3.373e-02  -3.559 0.000373 ***
    ## educ_college  4.871e-01  3.294e-02  14.787  < 2e-16 ***
    ## educ_advdeg   7.655e-01  3.415e-02  22.412  < 2e-16 ***
    ## white         1.033e-01  3.350e-02   3.084 0.002047 ** 
    ## AfAm          1.146e-01  4.400e-02   2.604 0.009232 ** 
    ## Hispanic     -1.891e-01  3.706e-02  -5.103 3.37e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.945 on 27181 degrees of freedom
    ## Multiple R-squared:  0.03474,    Adjusted R-squared:  0.03439 
    ## F-statistic: 97.84 on 10 and 27181 DF,  p-value: < 2.2e-16

``` r
suppressMessages(require(stargazer))
stargazer(model5, type = "text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           log1p(INCWAGE)       
    ## -----------------------------------------------
    ## AGE                            0.362           
    ##                               (0.312)          
    ##                                                
    ## I(AGE2)                       -0.012           
    ##                               (0.011)          
    ##                                                
    ## I(AGE3)                       0.0002           
    ##                              (0.0002)          
    ##                                                
    ## I(AGE4)                      -0.00000          
    ##                              (0.00000)         
    ##                                                
    ## educ_hs                      -0.120***         
    ##                               (0.034)          
    ##                                                
    ## educ_college                 0.487***          
    ##                               (0.033)          
    ##                                                
    ## educ_advdeg                  0.765***          
    ##                               (0.034)          
    ##                                                
    ## white                        0.103***          
    ##                               (0.033)          
    ##                                                
    ## AfAm                         0.115***          
    ##                               (0.044)          
    ##                                                
    ## Hispanic                     -0.189***         
    ##                               (0.037)          
    ##                                                
    ## Constant                      5.887*           
    ##                               (3.214)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                  27,192           
    ## R2                             0.035           
    ## Adjusted R2                    0.034           
    ## Residual Std. Error     1.945 (df = 27181)     
    ## F Statistic         97.839*** (df = 10; 27181) 
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
detach()

NNobs <- length(log1p(INCWAGE))
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1)
dat_graph <-subset(dat_use,graph_obs)  

plot(log1p(INCWAGE) ~ (AGE), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(10,11), data = dat_graph)

to_be_predicted5 <- data.frame(AGE = 25:65, female = 1, white = 1, Hispanic=0, AfAm = 0,educ_hs = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted5$yhat <- predict(model5, newdata = to_be_predicted5)

lines(yhat ~ AGE, data = to_be_predicted5)
```

![](Homework-5_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Model 5 expands on the conditions of model 2 but this time, the
regression model is based on log of income wages which is the dependent
variable. Since, Log expression is more sutable to simplyfy large
numbers like income wages, the log values will be expressed within a
smaller range compared to the original values. In our case, there would
not be high values for the regression line given that the Log of income
ranges between 10 to 11. Similar to the results from Model 2 regression
line, the regression slope for Model 5 shows a positive slope as age
increases, the income wages also increase but then decreases indicating
a diminishing returns for wages as workers reach retirement age (62
years old).

The table below demonstrates the estimates for the coefficient variables
for all the models that were run above (Models 1-5). The estimates
coefficient values shows an increase of income wages for female workers
(either white, hispanics or African American) depending on education
level ranging from High school degree to college degree to Advance
degree and also taking into consideration age. The reversing the
variables is not ideal due to age and education level do not depend on
different income wages.

``` r
suppressMessages(require(stargazer))
stargazer(model1, model2, model3, model4, model5, type = "text")
```

    ## 
    ## ============================================================================================================================================================
    ##                                                                               Dependent variable:                                                           
    ##                     ----------------------------------------------------------------------------------------------------------------------------------------
    ##                                                                        INCWAGE                                                          log1p(INCWAGE)      
    ##                                (1)                         (2)                        (3)                         (4)                        (5)            
    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    ## AGE                        3,797.762***              -106,297.700**                                          -447,159.300*                  0.362           
    ##                             (278.609)                 (49,475.030)                                           (249,815.700)                 (0.312)          
    ##                                                                                                                                                             
    ## I(AGE2)                     -35.145***                 5,349.783**                                            25,956.170*                   -0.012          
    ##                              (3.122)                   (2,356.464)                                           (14,989.570)                  (0.011)          
    ##                                                                                                                                                             
    ## I(AGE3)                                                -126.972**                                              -777.189*                    0.0002          
    ##                                                         (54.837)                                               (470.309)                   (0.0002)         
    ##                                                                                                                                                             
    ## I(AGE4)                                                  1.447**                                                12.750                     -0.00000         
    ##                                                          (0.624)                                                (8.144)                   (0.00000)         
    ##                                                                                                                                                             
    ## I(AGE5)                                                 -0.006**                                                -0.109                                      
    ##                                                          (0.003)                                                (0.074)                                     
    ##                                                                                                                                                             
    ## log(AGE)                                                                         30,525.940***                                                              
    ##                                                                                   (1,371.484)                                                               
    ##                                                                                                                                                             
    ## I(log(AGE2))                                                                                                                                                
    ##                                                                                                                                                             
    ##                                                                                                                                                             
    ## I(AGE6)                                                                                                         0.0004                                      
    ##                                                                                                                (0.0003)                                     
    ##                                                                                                                                                             
    ## educ_hs                   -6,770.665***               -6,731.209***              -6,835.500***               -6,741.937***                -0.120***         
    ##                            (1,036.138)                 (1,035.857)                (1,037.447)                 (1,035.868)                  (0.034)          
    ##                                                                                                                                                             
    ## educ_college              29,877.880***               29,868.500***              29,655.810***               29,854.780***                 0.487***         
    ##                            (1,011.847)                 (1,011.565)                (1,012.797)                 (1,011.596)                  (0.033)          
    ##                                                                                                                                                             
    ## educ_advdeg               48,276.170***               48,043.200***              48,595.430***               48,038.600***                 0.765***         
    ##                            (1,046.577)                 (1,048.858)                (1,047.025)                 (1,048.845)                  (0.034)          
    ##                                                                                                                                                             
    ## white                       1,653.394                  1,737.401*                  1,321.813                  1,721.916*                   0.103***         
    ##                            (1,028.571)                 (1,028.551)                (1,029.158)                 (1,028.593)                  (0.033)          
    ##                                                                                                                                                             
    ## AfAm                      -4,895.436***               -4,865.105***              -4,919.818***               -4,884.156***                 0.115***         
    ##                            (1,351.567)                 (1,351.236)                (1,353.317)                 (1,351.282)                  (0.044)          
    ##                                                                                                                                                             
    ## Hispanic                  -6,226.070***               -6,264.325***              -5,986.012***               -6,273.204***                -0.189***         
    ##                            (1,138.101)                 (1,137.932)                (1,139.230)                 (1,137.931)                  (0.037)          
    ##                                                                                                                                                             
    ## Constant                  -49,518.720***              817,797.400**              -69,311.550***             3,115,841.000*                  5.887*          
    ##                            (6,040.617)                (405,605.700)               (5,305.787)               (1,699,961.000)                (3.214)          
    ##                                                                                                                                                             
    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    ## Observations                  27,192                     27,192                      27,192                     27,192                      27,192          
    ## R2                            0.129                       0.130                      0.127                       0.130                      0.035           
    ## Adjusted R2                   0.129                       0.130                      0.127                       0.130                      0.034           
    ## Residual Std. Error  59,733.910 (df = 27183)     59,715.370 (df = 27180)    59,811.340 (df = 27184)     59,714.340 (df = 27179)       1.945 (df = 27181)    
    ## F Statistic         505.427*** (df = 8; 27183) 369.619*** (df = 11; 27180) 565.946*** (df = 7; 27184) 338.991*** (df = 12; 27179) 97.839*** (df = 10; 27181)
    ## ============================================================================================================================================================
    ## Note:                                                                                                                            *p<0.1; **p<0.05; ***p<0.01
