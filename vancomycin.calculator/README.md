
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vancomycin.calculator

<!-- badges: start -->
<!-- badges: end -->

The goal of vancomycin.calculator is to facilitate the dosing
adjustments that are typically conducted in a hospital setting, and to
allow clinical pharmacists to make patient specific judgements quicker.
The function calculates a new dosing regimen for patients receiving
Vancomycin using the Sawchuk-Zaske calculation strategy.

## Installation

You can install the development version of vancomycin.calculator from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stat545ubc-2022/assignment-b1-and-b2-akashp21/vancomycin.calculator")
#> Downloading GitHub repo stat545ubc-2022/assignment-b1-and-b2-akashp21@HEAD
#> 
#> * checking for file ‘/private/var/folders/x7/8jy19_tx0d5df24sylr0vlm80000gn/T/Rtmp7TNFc6/remotes136cb238bf716/stat545ubc-2022-assignment-b1-and-b2-akashp21-ad648a6/vancomycin.calculator/DESCRIPTION’ ... OK
#> * preparing ‘vancomycin.calculator’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘vancomycin.calculator_0.1.0.tar.gz’
#> Installing package into '/private/var/folders/x7/8jy19_tx0d5df24sylr0vlm80000gn/T/RtmpMxgAPx/temp_libpath1289c101061d7'
#> (as 'lib' is unspecified)
```

## Examples

This is a basic example which shows you how to solve a common problem:

``` r
library(vancomycin.calculator)
## basic example code
Vanco_SS_dose_calculator(6.4, 24.3, 400, 8, 1, 2, 0, 37.5, 12.5) # should equal "This function recommends a new dose of 500 mg given over Q 8 h. This will produce a peak of 30.375 mg/L and a trough value of 8 mg/L"
#> [1] "This function recommends a new dose of 500 mg given over Q 8 h. This will produce a peak of 30.375 mg/L and a trough value of 8 mg/L"
Vanco_SS_dose_calculator(11.0, 22.1, 1000, 12, 1.5, 1.33333, 55/60, 35, 17.5) # should be "This function recommends a new dose of 1500 mg given over Q 12 h. This will produce a peak of 33.15 mg/L and a trough value of 15.2692370301831 mg/L"
#> [1] "This function recommends a new dose of 1500 mg given over Q 12 h. This will produce a peak of 33.15 mg/L and a trough value of 15.2692370301831 mg/L"
```
