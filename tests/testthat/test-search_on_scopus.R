test_that("correct output", {
    skip_on_ci()
    skip_on_cran()
    expect_is(search_on_scopus("machine learning", 2), "bibliography")
})
