test_that("correct class", {
  skip_on_ci()
   skip_on_cran()
  ml_scopus_standard <- standard_scopus_search("machine learning", 2)
  expect_is(ml_scopus_standard, "scopus")
  expect_is(ml_scopus_standard, "scopus_standard")
})
