test_that("multiplication works", {
  expect_equal(sum(1, 2), 1 %>% sum(2))
})
