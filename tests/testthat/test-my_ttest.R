test_that("my_t.test works for less-than tests", {
  expect_type(my_t.test(1:15, "less", 0), "list")
})

test_that("my_t.test works for greater-than tests", {
  expect_type(my_t.test(1:15, "greater", 0), "list")
})

test_that("my_t.test works for two-sided tests", {
  expect_type(my_t.test(1:15, "two.sided", 0), "list")
})

test_that("my_t.test throws proper errors", {
  expect_error(my_t.test(1:15, 5, "string"))
})
