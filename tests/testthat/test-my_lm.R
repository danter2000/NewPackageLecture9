penguins <- NewPackageLecture9::my_penguins

penguins <- penguins %>% na.omit

frm_1 <- penguins %>% dplyr::select(bill_length_mm,
                                                           bill_depth_mm,
                                                           flipper_length_mm,
                                                           body_mass_g)
test_that("my_lm works mathematically", {
  expect_type(my_lm(body_mass_g ~ bill_length_mm + bill_depth_mm, frm_1), "double")
})

test_that("my_lm throws proper errors", {
  expect_error(my_lm("string" ~ "string2" + "string3", frm_1))
})
