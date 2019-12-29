test_that("stops on wrong input", {
    expect_error(search_on_pubmed(c("a", "b")))

    expect_error(search_on_pubmed("a", max_to_get = "ten"))
    expect_error(search_on_pubmed("a", max_to_get = 1.5))
    expect_error(search_on_pubmed("a", max_to_get = -1))

    expect_error(search_on_pubmed("a", api_key = 1234))
})


test_that("correct output", {
    result <- search_on_pubmed("machine learning", 1)
    expect_is(result, "bibliography")
    expect_equal(length(result), 1)
    expect_named(result[[1]])
    expect_true("type" %in% names(result[[1]]))

    result <- suppressWarnings(search_on_pubmed("a", 1))
    expect_is(result, "bibliography")
    expect_equal(length(result), 1)
    expect_equal(result[[1]], list())

})


test_that("correct restricted results", {
    search_string <- "(machine learning) AND lanera[Author]"
    result <- search_on_pubmed(search_string, max_to_get = 2)

    expect_is(result, "bibliography")
    expect_equal(length(result), 2)
    expect_named(result[1])
    expect_true("type" %in% names(result[[1]]))
})
