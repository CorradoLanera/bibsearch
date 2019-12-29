# Complete view ---------------------------------------------------

ml_scopus_complete <- readRDS(
    file.path("testing_data", "ml_scopus_complete.RDS")
)
ml_medline_complete <- medline_from_scopus(ml_scopus_complete)



test_that("Correct class and structure for complete searches", {
    expect_is(ml_medline_complete, "medline")
    expect_is(ml_medline_complete[[1]], "character")
})


test_that("Correct output", {
    expect_length(ml_medline_complete[[1]], 69)
    expect_equal(
        stringr::str_extract(ml_medline_complete[[1]], "\\w+"),
        c(
            "PT", "AID", "SI", "IS", "IS", "DP", "TI", "LID", "OID",
            "AB", "CI", "AU", "FAU", "AUID", "AU", "FAU", "AUID", "JT",
            "OT", "OT", "OT", "OT", "OT", "OT", "SO", "IP", "VI", "PG",
            NA, "PT", "AID", "SI", "IS", "IS", "DP", "TI", "LID", "OID",
            "AB", "CI", "AU", "FAU", "AUID", "AU", "FAU", "AUID", "AU",
            "FAU", "AUID", "AU", "FAU", "AUID", "JT", "OT", "OT", "OT",
            "OT", "OT", "OT", "OT", "OT", "OT", "OT", "OT", "SO", "IP",
            "VI", "PG", NA
        )
    )
})



# Standard view ---------------------------------------------------


ml_scopus_standard <- readRDS(
    file.path("testing_data", "ml_scopus_standard.RDS")
)
ml_medline_standard <- medline_from_scopus(ml_scopus_standard)

test_that("Correct class and structure for standard searches", {
    expect_is(ml_medline_standard, "medline")
    expect_is(ml_medline_standard[[1]], "character")
})


test_that("Correct output", {
    expect_length(ml_medline_standard[[1]], 42)
    expect_equal(
        stringr::str_extract(ml_medline_standard[[1]], "\\w+"),
        c(
            "PT", "AID", "SI", "IS", "IS", "DP", "TI", "LID", "OID",
            "AB", "CI", "AU", "FAU", "AUID", "JT", "OT", "SO", "IP",
            "VI", "PG", NA, "PT", "AID", "SI", "IS", "IS", "DP", "TI",
            "LID", "OID", "AB", "CI", "AU", "FAU", "AUID", "JT", "OT",
            "SO", "IP", "VI", "PG", NA
        )
    )
})
