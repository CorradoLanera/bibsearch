test_that("stops on wrong input", {
    expect_error(search_on_pubmed(c("a", "b")))

    expect_error(search_on_pubmed("a", max_to_get = "ten"))
    expect_error(search_on_pubmed("a", max_to_get = 1.5))
    expect_error(search_on_pubmed("a", max_to_get = -1))

    expect_error(search_on_pubmed("a", batch_size = "ten"))
    expect_error(search_on_pubmed("a", batch_size = 5001))
    expect_error(search_on_pubmed("a", batch_size = 1.5))
    expect_error(search_on_pubmed("a", batch_size = -1))

    expect_error(search_on_pubmed("a", api_key = 1234))
})


test_that("correct output", {
    search_string <- "(machine learning) AND lanera[Author]"
    expect_warning(result <- search_on_pubmed(search_string))
    expect_is(result, "bibliography")
    expect_gte(length(result), 6)
    expect_named(result[[1]])
    expect_true("type" %in% names(result[[1]]))
})


test_that("correct restricted results", {
    search_string <- "(machine learning) AND lanera[Author]"
    expect_warning(
        result <- search_on_pubmed(search_string, max_to_get = 2)
    )
    expect_is(result, "bibliography")
    expect_equal(length(result), 2)
    expect_named(result[[1]])
    expect_true("type" %in% names(result[[1]]))
})
