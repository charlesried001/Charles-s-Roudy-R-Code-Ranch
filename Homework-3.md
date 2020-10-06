Homework 3, Lab 3, Econ B2000
================
Christopher Tinevra, Group Members: Isabela Vieira, Akimawe Kadiri,
Nicole Kerrison, Mostafa Ragheb, Charles Reed

10/05/2020

Lab results will be gathered from using a simple k-nn technique to try
in classifying people’s neighborhoods, there will be multiple runs
utilizing different categories.

For the first example, this run will follow the variables of household
cost anf income total which were provided from the Lab 3 guidelines:

``` r
load('acs2017_ny.RData')
dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 18) & (acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels= c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))
```

``` r
norm_varb <- function(X_in) {
  (max(X_in, na.rm = TRUE) - X_in)/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}
is.na(OWNCOST) <- which(OWNCOST == 9999999)
housing_cost <- OWNCOST + RENT
norm_inc_tot <- norm_varb(INCTOT)
norm_housing_cost <- norm_varb(housing_cost)
data_use_prelim <- data.frame(norm_inc_tot,norm_housing_cost)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)
set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
summary(cl_data)
```

    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##          5110          5500          1971         12807         11376

``` r
prop.table(summary(cl_data))
```

    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##    0.13899467    0.14960287    0.05361223    0.34835709    0.30943314

``` r
summary(train_data)
```

    ##   norm_inc_tot    norm_housing_cost
    ##  Min.   :0.0000   Min.   :0.00000  
    ##  1st Qu.:0.9500   1st Qu.:0.02216  
    ##  Median :0.9744   Median :0.03131  
    ##  Mean   :0.9591   Mean   :0.40903  
    ##  3rd Qu.:0.9890   3rd Qu.:0.97495  
    ##  Max.   :1.0000   Max.   :1.00000

``` r
summary(test_data)
```

    ##   norm_inc_tot    norm_housing_cost
    ##  Min.   :0.0000   Min.   :0.00000  
    ##  1st Qu.:0.9493   1st Qu.:0.02216  
    ##  Median :0.9742   Median :0.03083  
    ##  Mean   :0.9583   Mean   :0.40534  
    ##  3rd Qu.:0.9890   3rd Qu.:0.97466  
    ##  Max.   :0.9990   Max.   :1.00000

``` r
suppressMessages(require(class))
for (indx in seq(1, 9, by= 2)) {
 pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
num_correct_labels <- sum(pred_borough == true_data)
correct_rate <- (num_correct_labels/length(true_data))*100
print(c(indx,correct_rate))
print(summary(pred_borough))
}
```

    ## [1]  1.00000 35.63447
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##           991          1258           426          3467          2897 
    ## [1]  3.00000 36.22082
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##           854          1176           325          3616          3068 
    ## [1]  5.00000 37.21651
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##           768          1115           228          3708          3220 
    ## [1]  7.00000 38.02412
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##           727          1035           168          3844          3265 
    ## [1]  9.00000 37.80285
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##           654          1003           151          3901          3330

For the second run, we will be using the categories of Poverty, Family
size and Household income for possible classification.

``` r
fam_pov <- POVERTY + FAMSIZE
norm_houseinc <- norm_varb(HHINCOME)
norm_fam_pov <- norm_varb(fam_pov)
data_use_prelim <- data.frame(norm_houseinc,norm_fam_pov)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)
set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
summary(cl_data)
```

    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##          4790          4964          1919         12433         11119

``` r
prop.table(summary(cl_data))
```

    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##    0.13598297    0.14092264    0.05447835    0.35295955    0.31565649

``` r
summary(train_data)
```

    ##  norm_houseinc     norm_fam_pov    
    ##  Min.   :0.0000   Min.   :0.00000  
    ##  1st Qu.:0.9234   1st Qu.:0.03089  
    ##  Median :0.9544   Median :0.28378  
    ##  Mean   :0.9382   Mean   :0.35720  
    ##  3rd Qu.:0.9759   3rd Qu.:0.65058  
    ##  Max.   :1.0000   Max.   :0.99807

``` r
summary(test_data)
```

    ##  norm_houseinc     norm_fam_pov    
    ##  Min.   :0.2368   Min.   :0.00000  
    ##  1st Qu.:0.9224   1st Qu.:0.03089  
    ##  Median :0.9543   Median :0.28764  
    ##  Mean   :0.9375   Mean   :0.36184  
    ##  3rd Qu.:0.9761   3rd Qu.:0.66216  
    ##  Max.   :1.0000   Max.   :0.99807

``` r
suppressMessages(require(class))
for (indx in seq(1, 9, by= 2)) {
 pred_borough1 <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
num_correct_labels <- sum(pred_borough1 == true_data)
correct_rate <- (num_correct_labels/length(true_data))*100
print(c(indx,correct_rate))
print(summary(pred_borough1))
}
```

    ## [1]  1.00000 61.84591
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##          1032          1150           361          3310          2804 
    ## [1]  3.0000 44.6575
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##           955          1123           344          3388          2847 
    ## [1]  5.00000 43.17893
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##           874          1055           301          3547          2880 
    ## [1]  7.00000 42.30103
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##           842           972           252          3644          2947 
    ## [1]  9.0000 41.6426
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##           789           955           177          3717          3019

In the second run, the k-nn algorithm gave an output accuracy higher
than the first run sequences. Also, the distribution of the people had a
similar count per borough in comparison to the first run.

For the third run, we will be using the categories of cost utilities
including water, heat, gas & electricity and Household income for
possible classification.

``` r
utility_cost <- COSTELEC + COSTFUEL + COSTGAS + COSTWATR
norm_houseinc2 <- norm_varb(HHINCOME)
norm_utility_cost <- norm_varb(utility_cost)
data_use_prelim <- data.frame(norm_houseinc2,norm_utility_cost)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)
set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
summary(cl_data)
```

    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##          4790          4964          1919         12433         11119

``` r
prop.table(summary(cl_data))
```

    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##    0.13598297    0.14092264    0.05447835    0.35295955    0.31565649

``` r
summary(train_data)
```

    ##  norm_houseinc2   norm_utility_cost
    ##  Min.   :0.0000   Min.   :0.0000   
    ##  1st Qu.:0.9234   1st Qu.:0.2231   
    ##  Median :0.9544   Median :0.4460   
    ##  Mean   :0.9382   Mean   :0.3937   
    ##  3rd Qu.:0.9759   3rd Qu.:0.5700   
    ##  Max.   :1.0000   Max.   :0.9780

``` r
summary(test_data)
```

    ##  norm_houseinc2   norm_utility_cost
    ##  Min.   :0.2368   Min.   :0.0000   
    ##  1st Qu.:0.9224   1st Qu.:0.2203   
    ##  Median :0.9543   Median :0.4460   
    ##  Mean   :0.9375   Mean   :0.3920   
    ##  3rd Qu.:0.9761   3rd Qu.:0.5685   
    ##  Max.   :1.0000   Max.   :0.9645

``` r
suppressMessages(require(class))
for (indx in seq(1, 9, by= 2)) {
 pred_borough2 <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
num_correct_labels <- sum(pred_borough2 == true_data)
correct_rate <- (num_correct_labels/length(true_data))*100
print(c(indx,correct_rate))
print(summary(pred_borough2))
}
```

    ## [1]  1.000 79.866
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##          1127          1160           473          3164          2733 
    ## [1]  3.00000 53.77151
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##          1093          1120           418          3261          2765 
    ## [1]  5.00000 52.16588
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##          1055          1045           324          3364          2869 
    ## [1]  7.00000 50.12129
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##          1049          1050           268          3417          2873 
    ## [1]  9.0000 49.3589
    ##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
    ##           999          1067           238          3477          2876

In the third run, the k-nn algorithm gave an output accuracy much higher
in comparison to the second run sequences and first run sequences.
Additionally, the distribution of the people had a similar count per
borough in comparison to the first run.

In conclusion, we are using the k-nearest neighbor algorithm to predict
the living location of a random individual in our dataset. We first
start by restricting our dataset to people living in NY (since pums data
reflects the entire state of NY), so we can classify 80% of our data as
living in one of the 5 boroughs of NYC. The likeability that a person
lives in Brooklyn is 35%, Bronx is 14%, Manhattan is 15%, Queens is 31%.
Using that probability distribution and other 2 or more variables of our
choice, we try to predict the living location of the remaining 20% of
our data.

That prediction is based on the proximity of a random non-classified
datapoint (individual) to other classified data points. For example, if
the majority of the houses in Manhattan have a very high utility cost,
then if a random datapoint has a high utility we assume that household
to be located in Manhattan. The level of accuracy we achieve from the
second run using variables of Poverty, family size and household income
gave us a 61.85% for the first sequence. While the level of accuracy we
achieve from the third run using variables of cost of multiple utilities
household income gave us a 79.87% for the first sequence. Therefore,
utilizing more information or variables with the k-nn algorithm from the
categories within the dataset can lead to higher accuracy to predict the
living location of random individuals.
