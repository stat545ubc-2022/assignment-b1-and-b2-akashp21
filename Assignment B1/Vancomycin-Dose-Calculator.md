STAT 545B Assignment B1
================

# Vancomycin Dose Calculator Function

# Table of contents

1.  [Exercise 1 - Function creation](#Exercise%201)
2.  [Exercise 2 - Documenting the function](#Exercise%202)
3.  [Exercise 3 - Examples](#Exercise%203)
4.  [Exercise 4 - Testing the function](#Exercise%204)

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
suppressPackageStartupMessages(library(devtools)) # devtools package
```

<a name="Exercise 2"></a>

(Exercise 2) @description The function calculates a new dosing regimen
for patients receiving Vancomycin using the Sawchuk-Zaske calculation
strategy. This strategy determines the elimination rate constant(k), the
half-life(t1/2), the volume of distribution (Vd), the new dosing
interval (tau), the new dosing rate recommendation (k0), the expected
peak level (C 1h), and the expected trough level (C tr). Any relevant
equations can be found online or by viewing the equations in the
function. Two or three serum concentrations are needed, along with the
assumption that the patient is at steady-state (receiving their 4th or
5th dose depending on the half-life of the drug). Using the patient
parameters, individualized dosing regimens can be created to achieve
ideal target levels. The goal of this function is to facilitate the
dosing adjustments that are typically conducted in a hospital setting,
and to allow clinical pharmacists to make patient specific judgements
quicker.

@param pre_level Patient level taken prior to providing the next dose
(mg/L) @param post_level Patient level taken after infusion time of dose
(mg/L) @param dose Amount of drug given to the patient (mg) @param
regimen Dosing interval currently used for the patient (Q8h, Q12h, Q18h,
Q24h, Q36h, Q48h) @param infusion_time Time over which the drug is
infused (h) @param peak_time The time when the post level is drawn (h)
@param trough_time The time with the pre_level is drawn (h) @param
peak_conc The ideal peak concentration for the patient (mg/L) @param
trough_conc The ideal trough concentration for the patient (mg/L)

@return The recommended dosing interval is returned through this
function @export

<a name="Exercise 1"></a>

Function (Exercise 1)

``` r
Vanco_SS_dose_calculator <- function(pre_level, post_level, dose, regimen, infusion_time, peak_time, trough_time, peak_conc, trough_conc) {
  x <- c(pre_level, post_level, dose, regimen, infusion_time, peak_time, trough_time, peak_conc, trough_conc) # storing inputs
  if(!is.numeric(x)) {
    stop("Sorry, you need to ensure all your inputs are numeric. The currently entered inputs are ", 
         class(pre_level), " ", 
         class(post_level), " ", 
         class(post_level)," ", 
         class(dose)," ", 
         class(regimen)," ", 
         class(infusion_time)," ",
         class(peak_time)," ", 
         class(trough_time)," ", 
         class(peak_conc)," ", 
         class(trough_conc)) # check to see if the inputs are numeric
  }

  k <- log(post_level/pre_level)*(1/(regimen-(infusion_time + peak_time + 
                                                trough_time))) # calculating elimination rate constant
  t_half <- log(2)/k # calculating half life
  
  Vd_num <- ((dose/infusion_time)*(1-exp(-k*infusion_time))*(exp(-k*peak_time))) # numerator for Vd
  Vd_denom <- (k*post_level*(1-exp(-k*regimen))) # denominator for Vd
  Vd <- Vd_num/Vd_denom # calculating volume of distribution
  
  new_regimen <- log(peak_conc/trough_conc)*1/k + infusion_time + 1 # calculating new regimen
  new_regimen <- (ifelse(new_regimen<10, 8,
                    ifelse(new_regimen<15, 12,
                    ifelse(new_regimen<21, 18, 
                    ifelse(Cancer$area_mean<30, 24, 
                    ifelse(Cancer$area_mean<42, 36,
                    ifelse(Cancer$area_mean<48, 48 ,'Unknown'))))))) # rounding regimen to appropriate time
  
  rate_num <- (k*Vd*peak_conc*(1-exp(-k*new_regimen))) # dose rate numerator
  rate_denom <- ((1-exp(-k*infusion_time))*(exp(-k*peak_time))) # dose rate denominator
  rate <- rate_num/rate_denom # determining new dose rate
  
  new_dose <- rate*infusion_time # calculating dose from dose rate
  rounded_dose <- plyr::round_any(new_dose, 250) # rounding the dose to nearest available formulation
  
  new_peak <- (peak_conc*rounded_dose)/new_dose # calculating new peak level
  new_trough <- new_peak*exp(-k*(new_regimen-(infusion_time+peak_time))) # calculating new trough level
  
  return(c("This function recommends a new dose of", rounded_dose, "given over Q",
           new_regimen, "h. This will produce a peak of", new_peak,
           "and a trough value of", new_trough)) # function return

}
```

<p>
For this function to work effectively, the patient levels are required.
If any of the inputs are non-numeric the function will produce the
following error message “Sorry, you need to ensure all your inputs are
numeric. The currently entered inputs are”, class(c(pre_level,
post_level, dose, regimen, infusion_time, peak_time, trough_time,
peak_conc, trough_conc)). Once the function is placed into a package,
more guidance can be provided regarding it’s use. Ideally, this
calculator is intended for the determination of peak and trough levels
in a hospital setting.
</p>

<a name="Exercise 3"></a>

@examples (Exercise 3) Example dose calculation with patient parameters

``` r
Vanco_SS_dose_calculator(11.0, 22.1, 1000, 12, 1.5, 1.33333, 55/60, 35, 17.5) # Should be "This function recommends a new dose of", 1500,"given over Q",12,"h",". This will produce a peak of", 33.15,"and a trough value of", 15.2692370301831
```

    ## [1] "This function recommends a new dose of"
    ## [2] "1500"                                  
    ## [3] "given over Q"                          
    ## [4] "12"                                    
    ## [5] "h. This will produce a peak of"        
    ## [6] "33.15"                                 
    ## [7] "and a trough value of"                 
    ## [8] "15.2692370301831"

Example dose calculation with different patient parameters

``` r
Vanco_SS_dose_calculator(25.0, 33.5, 1500, 8, 1.5, 115/60, 92/60, 33.5, 17.5) # Should be "This function recommends a new dose of", 1500,"given over Q",8,"h",". This will produce a peak of", 33.5,"and a trough value of", 21.5794478677179
```

    ## [1] "This function recommends a new dose of"
    ## [2] "1500"                                  
    ## [3] "given over Q"                          
    ## [4] "8"                                     
    ## [5] "h. This will produce a peak of"        
    ## [6] "33.5"                                  
    ## [7] "and a trough value of"                 
    ## [8] "21.5794478677179"

Example of an error in the dose calculation

``` r
Vanco_SS_dose_calculator("25.0", 43.0, 1000, 8, 1, 1.5, 1, 35, 17.5) # Should be "This function recommends a new dose of", 750,"given over Q",8,"h",". This will produce a peak of", 32.25,"and a trough value of", 16.6211709853472
```

    ## Error in Vanco_SS_dose_calculator("25.0", 43, 1000, 8, 1, 1.5, 1, 35, : Sorry, you need to ensure all your inputs are numeric. The currently entered inputs are character numeric numeric numeric numeric numeric numeric numeric numeric numeric

Example dose calculation with multiple patient parameters

``` r
Vanco_SS_dose_calculator(c(25,25), c(33.5, 43.0), c(1500,1000), c(8,8), c(1.5,1), c(115/60,1.5), c(92/60, 1), c(33.5,35), c(17.5:17.5)) # Should be "This function recommends a new dose of", 1500, 750,"given over Q",8,8,"h",". This will produce a peak of", 33.5, 32.25,"and a trough value of", 21.5794478677179, 16.6211709853472
```

    ##  [1] "This function recommends a new dose of"
    ##  [2] "1500"                                  
    ##  [3] "750"                                   
    ##  [4] "given over Q"                          
    ##  [5] "8"                                     
    ##  [6] "8"                                     
    ##  [7] "h. This will produce a peak of"        
    ##  [8] "33.5"                                  
    ##  [9] "32.25"                                 
    ## [10] "and a trough value of"                 
    ## [11] "21.5794478677179"                      
    ## [12] "16.6211709853472"

<a name="Exercise 4"></a>

@Tests (Exercise 4)

``` r
Vanco_SS_1 <- digest(Vanco_SS_dose_calculator(11.0, 22.1, 1000, 12, 1.5, 1.33333, 55/60, 35, 17.5)) # digesting the Vanco_SS_dose_calculator input
Vanco_SS_2 <- digest(Vanco_SS_dose_calculator(25.0, 33.5, 1500, 8, 1.5, 115/60, 92/60, 33.5, 17.5)) # digesting the Vanco_SS_dose_calculator input
Vanco_SS_3 <- digest(Vanco_SS_dose_calculator(c(25,25), c(33.5, 43.0), c(1500,1000), c(8,8), c(1.5,1), c(115/60,1.5), c(92/60, 1), c(33.5,35), c(17.5:17.5))) # digesting the Vanco_SS_dose_calculator input
```

``` r
test_that("Testing Vanco_SS_dose_calculator function design",{ 
          expect_equal(digest(Vanco_SS_dose_calculator(11.0, 22.1, 1000, 12, 1.5, 1.33333, 55/60, 35, 17.5)), Vanco_SS_1)
  }) # expect_equal test comparing the input of Vanco_SS_dose_calculator(11.0, 22.1, 1000, 12, 1.5, 1.33333, 55/60, 35, 17.5) to the digested input
```

    ## Test passed 🌈

``` r
test_that("Testing Vanco_SS_dose_calculator function design",{ 
          expect_equal(digest(Vanco_SS_dose_calculator(25.0, 33.5, 1500, 8, 1.5, 115/60, 92/60, 33.5, 17.5)), Vanco_SS_2)
  }) # expect_equal test comparing the input of Vanco_SS_dose_calculator(25.0, 33.5, 1500, 8, 1.5, 115/60, 92/60, 33.5, 17.5) to the digested input
```

    ## Test passed 😀

``` r
test_that("Testing Vanco_SS_dose_calculator function design",{ 
          expect_equal(digest(Vanco_SS_dose_calculator(c(25,25), c(33.5, 43.0), c(1500,1000), c(8,8), c(1.5,1), c(115/60,1.5), c(92/60, 1), c(33.5,35), c(17.5:17.5))), Vanco_SS_3)
}) # expect_equal test comparing the input of Vanco_SS_dose_calculator(c(25,25), c(33.5, 43.0), c(1500,1000), c(8,8), c(1.5,1), c(115/60,1.5), c(92/60, 1), c(33.5,35), c(17.5:17.5)) to the digested input
```

    ## Test passed 🌈

``` r
test_that("Testing Vanco_SS_dose_calculator function design",{
          expect_error(Vanco_SS_dose_calculator('25.0', 43.0, 1000, 8, 1, 1.5, 1, 35, 17.5), "Sorry, you need to ensure all your inputs are numeric. The currently entered inputs are character numeric numeric numeric numeric numeric numeric numeric numeric numeric") # expect_error test comparing the input error to the predicted error
})
```

    ## Test passed 😸
