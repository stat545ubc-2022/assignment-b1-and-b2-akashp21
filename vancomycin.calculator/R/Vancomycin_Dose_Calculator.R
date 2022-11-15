#' Vancomycin Dose Calculator
#' @description The function calculates a new dosing regimen for patients receiving Vancomycin using the Sawchuk-Zaske calculation
#' strategy. This strategy determines the elimination rate constant(k), the half-life(t1/2), the volume of distribution (Vd), the
#' new dosing interval (tau), the new dosing rate recommendation (k0), the expected peak level (C 1h), and the expected trough
#' level (C tr). Any relevant equations can be found online or by viewing the equations in the function. Two or three serum
#' concentrations are needed, along with the assumption that the patient is at steady-state (receiving their 4th or 5th dose
#' depending on the half-life of the drug). Using the patient parameters, individualized dosing regimens can be created to achieve
#' ideal target levels. The goal of this function is to facilitate the dosing adjustments that are typically conducted in a
#' hospital setting, and to allow clinical pharmacists to make patient specific judgements quicker.

#' @param pre_level Patient level taken prior to providing the next dose (mg/L)
#' @param post_level Patient level taken after infusion time of dose (mg/L)
#' @param dose Amount of drug given to the patient (mg)
#' @param regimen Dosing interval currently used for the patient (Q8h, Q12h, Q18h, Q24h, Q36h, Q48h)
#' @param infusion_time Time over which the drug is infused (h)
#' @param peak_time The time when the post level is drawn (h)
#' @param trough_time The time with the pre_level is drawn (h)
#' @param peak_conc The ideal peak concentration for the patient (mg/L)
#' @param trough_conc The ideal trough concentration for the patient (mg/L)

#' @return The recommended dosing interval is returned through this function
#' @export
#'
#' @examples
#'
#' Vanco_SS_dose_calculator(11.0, 22.1, 1000, 12, 1.5, 1.33333, 55/60, 35, 17.5) # should be "This function recommends a new dose of 1500 mg given over Q 12 h. This will produce a peak of 33.15 mg and a trough value of 15.2692370301831 mg"
#' Vanco_SS_dose_calculator(25.0, 33.5, 1500, 8, 1.5, 115/60, 92/60, 33.5, 17.5) # should be "This function recommends a new dose of 1500 mg given over Q 8 h. This will produce a peak of 33.5 mg and a trough value of 21.5794478677179 mg"
#' Vanco_SS_dose_calculator(c(25,25), c(33.5, 43.0), c(1500,1000), c(8,8), c(1.5,1), c(115/60,1.5), c(92/60, 1), c(33.5,35), c(17.5:17.5)) # should be "This function recommends a new dose of 1500 750 mg given over Q 8 8 h. This will produce a peak of 33.5 32.25 mg and a trough value of 21.5794478677179 16.6211709853472 mg"
#'

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
                         ifelse(new_regimen<30, 24,
                         ifelse(new_regimen<42, 36,
                         ifelse(new_regimen<48, 48 ,'Unknown'))))))) # rounding regimen to appropriate time

  rate_num <- (k*Vd*peak_conc*(1-exp(-k*new_regimen))) # dose rate numerator
  rate_denom <- ((1-exp(-k*infusion_time))*(exp(-k*peak_time))) # dose rate denominator
  rate <- rate_num/rate_denom # determining new dose rate

  new_dose <- rate*infusion_time # calculating dose from dose rate
  rounded_dose <- plyr::round_any(new_dose, 250) # rounding the dose to nearest available formulation

  new_peak <- (peak_conc*rounded_dose)/new_dose # calculating new peak level
  new_trough <- new_peak*exp(-k*(new_regimen-(infusion_time+peak_time))) # calculating new trough level
  output <-c("This function recommends a new dose of", rounded_dose, "mg", "given over Q", new_regimen, "h. This will produce a peak of",
             new_peak, "mg","and a trough value of", new_trough, "mg")
  return(paste(output,collapse=' ')) # function return
}


