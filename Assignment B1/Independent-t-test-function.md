STAT 545B Assignment B1
================

# Independent 2 Sample T-Test Function

``` r
suppressPackageStartupMessages(library(dplyr)) # data manipulation
suppressPackageStartupMessages(library(testthat)) # testing
suppressPackageStartupMessages(library(readr)) # reading and writing data
suppressPackageStartupMessages(library(devtools)) # tools for function
suppressPackageStartupMessages(library(tidyr)) # data tidying
suppressPackageStartupMessages(library(car)) # data analysis
suppressPackageStartupMessages(library(broom)) # data analysis
suppressPackageStartupMessages(library(datateachr)) # data sets
suppressPackageStartupMessages(library(digest)) # digesting output
```

@description The function conducts an independent 2 sample t-test on the
data of interest. It will determine whether the independent t-test
requires the use of equal variances or unequal variances based on the
outcome of Levene’s and Bartlett’s tests at a significance level of
0.05. The t-test conducted also uses a significance level of 0.05, since
this is the level of significance commonly used.

@param exp_var Qualitative explanatory variable (The use of exp_var
should be appropriate in this instance since it is referring to the
explanatory variable)  
@param out_var Quantitative outcome variable (The use of out_var should
be appropriate in this instance since it is referring to the outcome
variable) @param data The data of interest (The use of data should be
appropriate in this instance since it is referring to the data) @param …
Other parameters to pass to

@return p-value an independent 2-sample t-test based on the method used.
@export

Data to test

``` r
Cancer_sample <- as_tibble(datateachr::cancer_sample) # cancer data set
apt <- as_tibble(datateachr::apt_buildings) # apt data set
park <- as_tibble(datateachr::parking_meters) #Parking data set

SmokingEffect_SK20Q5RVE903 <- read_csv("~/Desktop/UBC/Graduate School/Courses/Term 1/SPPH 400/Assignments/Assignment 3/SmokingEffect_SK20Q5RVE903.csv") # data from SPPH 400
```

    ## Rows: 684 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (6): Caesarean, Age, Gender, Smoke, Height, FEV
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Smoking_effect <- tibble(SmokingEffect_SK20Q5RVE903) # creating a tibble
Smoking_effect_2 <- Smoking_effect %>% 
 mutate(Gender = factor(case_when(Gender == 0 ~ "Female", # converting entry 0 to Female
                                      Gender == 1 ~ "Male", # converting entry 1 to Male
                                      TRUE ~ "Unknown"), # unknown class
                            levels = c("Female", "Male", "Unknown"))) %>%
     mutate(Caesarean = factor(case_when(Caesarean == 0 ~ "No", # converting entry 0 to Non-cesarean birth
                                         Caesarean == 1 ~ "Yes", # converting entry 1 to Cesarean birth
                                         TRUE ~ "Unknown"), # unknown class
                               levels = c("No", "Yes", "Unknown"))) %>%
     mutate(Smoke = factor(case_when(Smoke == 0 ~ "No", # converting 0 to non-smoker
                                     Smoke == 1 ~ "Yes", # converting 1 to smoker
                                     TRUE ~ "Unknown"), # unknown class
                           levels = c("No", "Yes", "Unknown"))) # ordering levels
```

Function

``` r
Independent_t_test <- function (exp_var, out_var, data, ...) {
  exp_var <- as.factor(exp_var) # coercing the explanatory variable into a factor
    if(!is.factor(exp_var)) {
    stop("Sorry, you need to ensure your qualitative variable is a factor", " The currently entered explanatory variable is a ", class(exp_var)) #Checks to see if the exp_var input is numeric
  }
  if(!is.numeric(out_var)) {
    stop("Sorry, you need to ensure your quantitative variable is numeric.", " The currently entered outcome variable is a ", class(out_var)) #Checks to see if the out_var input is numeric
  }
  car::leveneTest(out_var ~ exp_var, data) # Levene's test
  bartlett.test(out_var ~ exp_var, data) # Bartlett's test
  if (c(bartlett.test(out_var ~ exp_var, data, ...)$p.value < 0.05, car::leveneTest(out_var ~ exp_var, data)$p.value < 0.05)){
    model <- t.test(out_var ~ exp_var, data, paired=FALSE, 
                    var.eq= FALSE, conf.level=0.95, alt="two.sided") # t-test for unequal variance at a confidence level of 0.95
  }
  else {model <- t.test(out_var ~ exp_var, data, paired=FALSE, 
               var.eq= TRUE, conf.level=0.95, alt="two.sided")  # t-test for equal variance at a confidence level of 0.95
  }
  tidy_model <- broom::tidy(model) # cleaning up the output
  return(tidy_model$p.value) # returning only the p-value
}
```

<p>
For this function to work effectively, a single qualitative variable
with two levels is required for the exp_var and a single quantitative
variable is required for the out_var. If use of the function is
required, the data can be adjusted accordingly. If the levels are
numeric in the qualitative explanatory variable, Levene’s test and
Bartlett’s test will not work and the function will produce the
following error message “Sorry, you need to ensure your qualitative
variable is a factor. The currently entered explanatory variable is a”
class(exp_var). Once the function is placed into a package, more
guidance can be provided regarding it’s use. Perhaps the inputs can even
be adjusted to change the confidence and the type of hypothesis test
(one sided versus two sided).
</p>

@examples

``` r
Independent_t_test(Smoking_effect_2$Caesarean, Smoking_effect_2$FEV, Smoking_effect_2) # should be 0.7097405
```

    ## [1] 0.7097405

``` r
Independent_t_test(Cancer_sample$diagnosis, Cancer_sample$perimeter_mean, Cancer_sample) # should be 1.023141e-66
```

    ## [1] 1.023141e-66

``` r
Independent_t_test(Cancer_sample$diagnosis, Cancer_sample$texture_mean, Cancer_sample) # should be 4.058636e-25
```

    ## [1] 4.058636e-25

``` r
Independent_t_test(Cancer_sample$diagnosis, Cancer_sample$concavity_mean, Cancer_sample) # should be 3.742121e-58
```

    ## [1] 3.742121e-58

``` r
Independent_t_test(apt$balconies, apt$no_of_storeys, apt) # should be 3.968512e-135
```

    ## [1] 3.968512e-135

``` r
Independent_t_test(apt$exterior_fire_escape, apt$no_of_storeys, apt) # should be 4.225185e-10
```

    ## [1] 4.225185e-10

@Tests

``` r
Caesarean_vs_FEV <- digest(Independent_t_test(Smoking_effect_2$Caesarean, Smoking_effect_2$FEV, Smoking_effect_2)) # digesting the Independent_t_test input
Diagnosis_vs_perimeter_mean <- digest(Independent_t_test(Cancer_sample$diagnosis, Cancer_sample$perimeter_mean, Cancer_sample)) # digesting the Independent_t_test input
Exterior_fire_escape_vs_no_of_storeys <- digest(Independent_t_test(apt$balconies, apt$no_of_storeys, apt)) # digesting the Independent_t_test input

test_that("Testing Indepenedent t-test function design",{ 
          expect_equal(Independent_t_test(Cancer_sample$diagnosis, Cancer_sample$texture_mean, Cancer_sample), 4.058636e-25) # expect_equal test comparing values
          expect_equal(digest(Independent_t_test(Smoking_effect_2$Caesarean, Smoking_effect_2$FEV, Smoking_effect_2)), Caesarean_vs_FEV) # expect_equal test comparing digested input
          expect_equal(digest(Independent_t_test(Cancer_sample$diagnosis, Cancer_sample$perimeter_mean, Cancer_sample)), Diagnosis_vs_perimeter_mean) # expect_equal test comparing digested input
          expect_equal(digest(Independent_t_test(apt$balconies, apt$no_of_storeys, apt)), Exterior_fire_escape_vs_no_of_storeys) # expect_equal test comparing digested input
          expect_error(Independent_t_test(park$credit_card, park$r_mf_9a_6p, park), "Sorry, you need to ensure your quantitative variable is numeric. The currently entered outcome variable is a character") # expect_error test comparing the input error to the predicted error
})
```

    ## Test passed 🎊