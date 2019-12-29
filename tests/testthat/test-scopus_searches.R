ml_scopus_standard <- standard_scopus_search("machine learning", 2)

test_that("correct class", {
  expect_is(ml_scopus_standard, "scopus")
  expect_is(ml_scopus_standard, "scopus_standard")
})
