penguins <- na.omit(NewPackageLecture9::my_penguins)

pen_num_forest <- penguins %>% dplyr::select(bill_length_mm,
                                             bill_depth_mm,
                                             flipper_length_mm,
                                             body_mass_g)

test_that("my_rf_cv works mathematically", {
  expect_type(my_rf_cv(5), "list")
})

test_that("my_rf_cv throws proper errors", {
  expect_error(my_rf_cv("string"))
})
