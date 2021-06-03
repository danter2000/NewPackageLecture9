penguins <- na.omit(NewPackageLecture9::my_penguins)

pen_num <- penguins %>% dplyr::select(bill_length_mm,
                                       bill_depth_mm,
                                       flipper_length_mm,
                                       body_mass_g)

pen_cl <- penguins %>% dplyr::pull(species)

pen_cl <- as.character(pen_cl)

test_that("my_knn_cv works mathematically", {
  expect_type(my_knn_cv(pen_num, pen_cl, 5, 5), "list")
})

test_that("my_knn_cv throws proper erros", {
  expect_error(my_knn_cv(pen_num, pen_cl, "string", "string"))
})
