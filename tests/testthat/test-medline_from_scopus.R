ml_scopus_comlete <- readRDS(
    file.path("testing_data", "ml_scopus_complete.RDS")
)
ml_medline_complete <- medline_from_scopus(ml_scopus_comlete)

test_that("Correct class and structure for complete searches", {
    expect_is(ml_medline_complete, "medline")
    expect_is(ml_medline_complete[[1]], "character")
})


test_that("Correct length", {
    expect_length(ml_medline_complete[[1]], 69)
})
