Homework 4, Lab 4, Econ B2000
================
Christopher Tinevra, Group Members: Isabela Vieira, Akimawe Kadiri,
Nicole Kerrison, Mostafa Ragheb, Charles Reed
10/12/2020

Lab 4 Results:

Re-run of the original codes provided by the lab 4 initial coding
guidelines.

``` r
load("acs2017_ny.RData")
attach(acs2017_ny)
use_varb <- (AGE >= 23) & (AGE <= 62) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35)
dat_use <- subset(acs2017_ny,use_varb) 
detach()

attach(dat_use)
model_temp1 <- lm(INCWAGE ~ AGE + female + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(model_temp1)
```

    ## 
    ## Call:
    ## lm(formula = INCWAGE ~ AGE + female + AfAm + Asian + Amindian + 
    ##     race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + 
    ##     educ_advdeg)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -151816  -33073  -10469   12817  626216 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     2049.69    2068.07   0.991 0.321635    
    ## AGE             1043.19      28.82  36.193  < 2e-16 ***
    ## female        -24707.27     645.97 -38.248  < 2e-16 ***
    ## AfAm          -11705.29    1026.87 -11.399  < 2e-16 ***
    ## Asian           -671.82    1272.21  -0.528 0.597448    
    ## Amindian       -9608.08    5525.35  -1.739 0.082056 .  
    ## race_oth       -7388.53    1178.54  -6.269 3.65e-10 ***
    ## Hispanic       -4155.99    1089.58  -3.814 0.000137 ***
    ## educ_hs        10182.76    1612.48   6.315 2.72e-10 ***
    ## educ_somecoll  22123.91    1654.00  13.376  < 2e-16 ***
    ## educ_college   55779.21    1635.19  34.112  < 2e-16 ***
    ## educ_advdeg    85088.48    1680.78  50.624  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 77610 on 59422 degrees of freedom
    ## Multiple R-squared:  0.1511, Adjusted R-squared:  0.151 
    ## F-statistic: 961.8 on 11 and 59422 DF,  p-value: < 2.2e-16

``` r
suppressMessages(require(stargazer))
stargazer(model_temp1, type = "text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                               INCWAGE          
    ## -----------------------------------------------
    ## AGE                        1,043.188***        
    ##                              (28.823)          
    ##                                                
    ## female                    -24,707.260***       
    ##                              (645.970)         
    ##                                                
    ## AfAm                      -11,705.290***       
    ##                             (1,026.870)        
    ##                                                
    ## Asian                        -671.823          
    ##                             (1,272.207)        
    ##                                                
    ## Amindian                    -9,608.085*        
    ##                             (5,525.345)        
    ##                                                
    ## race_oth                   -7,388.530***       
    ##                             (1,178.536)        
    ##                                                
    ## Hispanic                   -4,155.991***       
    ##                             (1,089.577)        
    ##                                                
    ## educ_hs                    10,182.760***       
    ##                             (1,612.484)        
    ##                                                
    ## educ_somecoll              22,123.910***       
    ##                             (1,654.000)        
    ##                                                
    ## educ_college               55,779.210***       
    ##                             (1,635.192)        
    ##                                                
    ## educ_advdeg                85,088.480***       
    ##                             (1,680.779)        
    ##                                                
    ## Constant                     2,049.689         
    ##                             (2,068.072)        
    ##                                                
    ## -----------------------------------------------
    ## Observations                  59,434           
    ## R2                             0.151           
    ## Adjusted R2                    0.151           
    ## Residual Std. Error   77,607.120 (df = 59422)  
    ## F Statistic         961.797*** (df = 11; 59422)
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
suppressMessages(require(AER))

NNobs <- length(INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1)
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)

to_be_predicted1 <- data.frame(AGE = 25:55, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted1$yhat <- predict(model_temp1, newdata = to_be_predicted1)

lines(yhat ~ AGE, data = to_be_predicted1)
```

![](unnamed-chunk-1-1.png)

``` r
detach()
```

The preliminary results from the above 1st linear regression illustrated
a positive correlation between income wages and ages ranging from 25 to
55 while using restriction with the variables of the dataset including
focusing on female participants only, restrictions on race (looking at
Hispanics, Indian American, and other) and highest education achieve
(college).

For the following linear regression, we took into consideration similar
variables of the dataset like education level (using advance college as the
base education) and modifying other variables like age range, and focused
on total income and gender. The age range will span from 23 to 62,
assuming the average age of college graduates is 23 years old and the
average age for retirement is 62 years old in the USA. The main goal of
the linear regressions below is to observe whether there is a
significant difference between the total income between females and
males ranging from ages 23 to 62.

Our null hypothesis for the below linear regressions illustrates no
difference for total income between females and males with ages from 23
to 62 years old.

Our alternative hypothesis for the below linear regression illustrates a
difference seen between the total income between females and males with
ages from 23 to 62 years old.

This 2nd linear regression illustrates the correlation between total
income of male advance college graduates ranging from ages 23 to 62 years old.

``` r
#Dependent Variable = Total Income (INCTOT)
#Independent Variable = Age, Male population, Education Level (Advanced Degrees)
model_temp2 <- lm(INCTOT ~ AGE + female + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(model_temp2)
```

    ## 
    ## Call:
    ## lm(formula = INCTOT ~ AGE + female + educ_hs + educ_somecoll + 
    ##     educ_college + educ_advdeg)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -163615  -35895  -12511   12447 1275034 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -11426.2     2134.1  -5.354 8.63e-08 ***
    ## AGE             1333.1       31.2  42.731  < 2e-16 ***
    ## female        -29788.6      700.9 -42.501  < 2e-16 ***
    ## educ_hs        12442.6     1723.8   7.218 5.33e-13 ***
    ## educ_somecoll  25816.4     1761.0  14.660  < 2e-16 ***
    ## educ_college   63733.7     1732.7  36.782  < 2e-16 ***
    ## educ_advdeg    97086.8     1777.2  54.629  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 84500 on 59427 degrees of freedom
    ## Multiple R-squared:  0.1575, Adjusted R-squared:  0.1574 
    ## F-statistic:  1852 on 6 and 59427 DF,  p-value: < 2.2e-16

``` r
coeftest(model_temp2,vcovHC) #Heteroskedasticity
```

    ## 
    ## t test of coefficients:
    ## 
    ##                 Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)   -11426.239   1539.518  -7.422 1.169e-13 ***
    ## AGE             1333.122     29.782  44.762 < 2.2e-16 ***
    ## female        -29788.561    706.226 -42.180 < 2.2e-16 ***
    ## educ_hs        12442.591    966.837  12.869 < 2.2e-16 ***
    ## educ_somecoll  25816.396   1011.853  25.514 < 2.2e-16 ***
    ## educ_college   63733.705   1211.553  52.605 < 2.2e-16 ***
    ## educ_advdeg    97086.843   1564.964  62.038 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
suppressMessages(require(stargazer))
stargazer(model_temp2, type = "text")
```

    ## 
    ## ================================================
    ##                         Dependent variable:     
    ##                     ----------------------------
    ##                                INCTOT           
    ## ------------------------------------------------
    ## AGE                         1,333.122***        
    ##                               (31.198)          
    ##                                                 
    ## female                     -29,788.560***       
    ##                              (700.890)          
    ##                                                 
    ## educ_hs                    12,442.590***        
    ##                             (1,723.780)         
    ##                                                 
    ## educ_somecoll              25,816.400***        
    ##                             (1,760.949)         
    ##                                                 
    ## educ_college               63,733.710***        
    ##                             (1,732.735)         
    ##                                                 
    ## educ_advdeg                97,086.840***        
    ##                             (1,777.200)         
    ##                                                 
    ## Constant                   -11,426.240***       
    ##                             (2,134.131)         
    ##                                                 
    ## ------------------------------------------------
    ## Observations                   59,434           
    ## R2                             0.157            
    ## Adjusted R2                    0.157            
    ## Residual Std. Error   84,496.470 (df = 59427)   
    ## F Statistic         1,851.555*** (df = 6; 59427)
    ## ================================================
    ## Note:                *p<0.1; **p<0.05; ***p<0.01

``` r
suppressMessages(require(AER))

NNobs <- length(INCTOT)
set.seed(12345)
graph_obs2 <- (runif(NNobs) < 0.1) 
dat_graph2 <-subset(dat_use,graph_obs2)  

plot(INCTOT ~ (AGE), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,200000), data = dat_graph2)

to_be_predicted2 <- data.frame(AGE = 23:62, female = 0, educ_hs = 0 , educ_somecoll = 0, educ_college = 0, educ_advdeg = 1)
to_be_predicted2$yhat <- predict(model_temp2, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)
```

![](unnamed-chunk-2-1.png)

``` r
detach()
```

This 3rd linear regression illustrates the correlation between total
income of female advance college graduates ranging from ages 23 to 62 years old.

``` r
#Dependent Variable = Total Income (INCTOT)
#Independent Variable = Age, Female population, Education Level (Advanced Degrees)
model_temp3 <- lm(INCTOT ~ AGE + female +  educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(model_temp3)
```

    ## 
    ## Call:
    ## lm(formula = INCTOT ~ AGE + female + educ_hs + educ_somecoll + 
    ##     educ_college + educ_advdeg)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -163615  -35895  -12511   12447 1275034 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -11426.2     2134.1  -5.354 8.63e-08 ***
    ## AGE             1333.1       31.2  42.731  < 2e-16 ***
    ## female        -29788.6      700.9 -42.501  < 2e-16 ***
    ## educ_hs        12442.6     1723.8   7.218 5.33e-13 ***
    ## educ_somecoll  25816.4     1761.0  14.660  < 2e-16 ***
    ## educ_college   63733.7     1732.7  36.782  < 2e-16 ***
    ## educ_advdeg    97086.8     1777.2  54.629  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 84500 on 59427 degrees of freedom
    ## Multiple R-squared:  0.1575, Adjusted R-squared:  0.1574 
    ## F-statistic:  1852 on 6 and 59427 DF,  p-value: < 2.2e-16

``` r
coeftest(model_temp3,vcovHC) #Heteroskedasticity
```

    ## 
    ## t test of coefficients:
    ## 
    ##                 Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)   -11426.239   1539.518  -7.422 1.169e-13 ***
    ## AGE             1333.122     29.782  44.762 < 2.2e-16 ***
    ## female        -29788.561    706.226 -42.180 < 2.2e-16 ***
    ## educ_hs        12442.591    966.837  12.869 < 2.2e-16 ***
    ## educ_somecoll  25816.396   1011.853  25.514 < 2.2e-16 ***
    ## educ_college   63733.705   1211.553  52.605 < 2.2e-16 ***
    ## educ_advdeg    97086.843   1564.964  62.038 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
suppressMessages(require(stargazer))
stargazer(model_temp3, type = "text")
```

    ## 
    ## ================================================
    ##                         Dependent variable:     
    ##                     ----------------------------
    ##                                INCTOT           
    ## ------------------------------------------------
    ## AGE                         1,333.122***        
    ##                               (31.198)          
    ##                                                 
    ## female                     -29,788.560***       
    ##                              (700.890)          
    ##                                                 
    ## educ_hs                    12,442.590***        
    ##                             (1,723.780)         
    ##                                                 
    ## educ_somecoll              25,816.400***        
    ##                             (1,760.949)         
    ##                                                 
    ## educ_college               63,733.710***        
    ##                             (1,732.735)         
    ##                                                 
    ## educ_advdeg                97,086.840***        
    ##                             (1,777.200)         
    ##                                                 
    ## Constant                   -11,426.240***       
    ##                             (2,134.131)         
    ##                                                 
    ## ------------------------------------------------
    ## Observations                   59,434           
    ## R2                             0.157            
    ## Adjusted R2                    0.157            
    ## Residual Std. Error   84,496.470 (df = 59427)   
    ## F Statistic         1,851.555*** (df = 6; 59427)
    ## ================================================
    ## Note:                *p<0.1; **p<0.05; ***p<0.01

``` r
suppressMessages(require(AER))

NNobs <- length(INCTOT)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.1)
dat_graph3 <-subset(dat_use,graph_obs) 
plot(INCTOT ~ (AGE), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph3)

to_be_predicted3 <- data.frame(AGE = 23:62, female = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 0, educ_advdeg = 1)
to_be_predicted3$yhat <- predict(model_temp3, newdata = to_be_predicted3)

lines(yhat ~ AGE, data = to_be_predicted3)
```

![](unnamed-chunk-3-1.png)

``` r
detach()
```

In conclusion, given that the p-value for both linear regression
resulted in p-value:\< 2.2e-16 which is under the p\<0.01 for
significant level, thus the smaller the p-value then the stronger
evidence in favor of supporting our alternative hypothesis. Therefore,
we are rejecting our null hypothesis which states that there is no
difference in total income between female and male ranging from ages 23
to 62 years old. Other linear regression using different education
levels output similar illustration but the total income will vary
depending on the education level where high school graduates and college graduates will earn
lower than advance college graduates. Additionally, the linear regressions illustrates a difference in total income when comparing males and females as even with the same level of education, males (from 2nd linear regression) start and earn more money throughout the ages of 23 to 62 in comparison with females (from the 3rd linear regression).
