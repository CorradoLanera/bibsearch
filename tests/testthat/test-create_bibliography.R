test_that("default works", {
    expect_error(create_bibliography("foo"), "is of class",
        class = "usethis_error"
    )
})


test_that("correct empty bibliography", {
    expect_is(
        suppressWarnings(
            create_bibliography.medline(list("0" = character()))
        ),
        "bibliography"
    )
})

