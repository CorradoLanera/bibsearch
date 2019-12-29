ok <- pubmed_search("machine learning", 1)
ko <- suppressWarnings(pubmed_search("a", 1))


test_that("return error on wrong inputs", {
    expect_error(pubmed_search("", "empty"))
    expect_error(pubmed_search(c("a", "b")), "has length 2")

    expect_error(
        pubmed_search("aaaa", max_to_get = "ten"),
        "is not of class 'numeric'"
    )
    expect_error(
        pubmed_search("aaaa", max_to_get = 1.5),
        "is_whole_number"
    )
    expect_error(pubmed_search("aaaa", max_to_get = -1), "is_positive ")

    expect_error(pubmed_search("aaaa", api_key = 1234))
})


test_that("correct output", {
    expect_is(ok, "pubmed")
    expect_is(ok, "list")
    expect_equal(length(ok), 1)
    expect_named(ko[1])

    expect_is(ko, "pubmed")
    expect_is(ko, "list")
    expect_equal(length(ko), 1)
    expect_equal(length(ko[[1]]), 0)
    expect_named(ko[1])
})
