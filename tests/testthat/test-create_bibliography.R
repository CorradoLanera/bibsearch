ml_scopus_complete <- readRDS(
    file.path("testing_data", "ml_scopus_complete.RDS")
)
ml_medline_complete <- medline_from_scopus(ml_scopus_complete)

ml_scopus_standard <- readRDS(
    file.path("testing_data", "ml_scopus_standard.RDS")
)
ml_medline_standard <- medline_from_scopus(ml_scopus_standard)





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

test_that("works on medline objects", {
    expect_is(
        create_bibliography(ml_medline_complete),
        "bibliography"
    )
    expect_is(
        create_bibliography(ml_medline_standard),
        "bibliography"
    )
    expect_length(create_bibliography(ml_medline_standard), 2)
    expect_length(
        create_bibliography(ml_medline_standard)[[1]],
        18
    )
})

test_that("works on scopus objects", {
    expect_is(
        create_bibliography(ml_scopus_complete),
        "bibliography"
    )
    expect_is(
        create_bibliography(ml_scopus_standard),
        "bibliography"
    )
    expect_length(create_bibliography(ml_scopus_complete), 2)
    expect_length(
        create_bibliography(ml_scopus_complete)[[1]],
        21
    )
})

