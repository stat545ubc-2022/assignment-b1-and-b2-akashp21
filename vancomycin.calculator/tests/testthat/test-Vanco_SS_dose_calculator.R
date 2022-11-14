Vanco_SS_1 <- digest::digest(Vanco_SS_dose_calculator(11.0, 22.1, 1000, 12, 1.5, 1.33333, 55/60, 35, 17.5)) # digesting the Vanco_SS_dose_calculator input
Vanco_SS_2 <- digest::digest(Vanco_SS_dose_calculator(25.0, 33.5, 1500, 8, 1.5, 115/60, 92/60, 33.5, 17.5)) # digesting the Vanco_SS_dose_calculator input
Vanco_SS_3 <- digest::digest(Vanco_SS_dose_calculator(c(25,25), c(33.5, 43.0), c(1500,1000), c(8,8), c(1.5,1), c(115/60,1.5), c(92/60, 1), c(33.5,35), c(17.5:17.5))) # digesting the Vanco_SS_dose_calculator input

test_that("Testing Vanco_SS_dose_calculator function design",{
  expect_equal(digest::digest(Vanco_SS_dose_calculator(11.0, 22.1, 1000, 12, 1.5, 1.33333, 55/60, 35, 17.5)), Vanco_SS_1)
}) # expect_equal test comparing the input of Vanco_SS_dose_calculator(11.0, 22.1, 1000, 12, 1.5, 1.33333, 55/60, 35, 17.5) to the digested input

test_that("Testing Vanco_SS_dose_calculator function design",{
  expect_equal(digest::digest(Vanco_SS_dose_calculator(25.0, 33.5, 1500, 8, 1.5, 115/60, 92/60, 33.5, 17.5)), Vanco_SS_2)
}) # expect_equal test comparing the input of Vanco_SS_dose_calculator(25.0, 33.5, 1500, 8, 1.5, 115/60, 92/60, 33.5, 17.5) to the digested input

test_that("Testing Vanco_SS_dose_calculator function design",{
  expect_equal(digest::digest(Vanco_SS_dose_calculator(c(25,25), c(33.5, 43.0), c(1500,1000), c(8,8), c(1.5,1), c(115/60,1.5), c(92/60, 1), c(33.5,35), c(17.5:17.5))), Vanco_SS_3)
}) # expect_equal test comparing the input of Vanco_SS_dose_calculator(c(25,25), c(33.5, 43.0), c(1500,1000), c(8,8), c(1.5,1), c(115/60,1.5), c(92/60, 1), c(33.5,35), c(17.5:17.5)) to the digested input

test_that("Testing Vanco_SS_dose_calculator function design",{
  expect_error(Vanco_SS_dose_calculator('25.0', 43.0, 1000, 8, 1, 1.5, 1, 35, 17.5), "Sorry, you need to ensure all your inputs are numeric. The currently entered inputs are character numeric numeric numeric numeric numeric numeric numeric numeric numeric") # expect_error test comparing the input error to the predicted error
})
