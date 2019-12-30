test_that("pb_len", {
  expect_error(
      pb_len("a"), "must be an integer", class = "usethis_error"
  )
  expect_error(
      pb_len(1.2), "must be an integer", class = "usethis_error"
  )

  pb_aux <- pb_len(1)
  expect_is(pb_aux, "progress_bar")
  expect_false(pb_aux$finished)
  tick(pb_aux)
  expect_true(pb_aux$finished)
})


test_that("check_scopus online", {
    skip_if(Sys.getenv("Elsevier_API") != "")
    expect_false(check_scopus_pat())
})
test_that("check_scopus local", {
    skip_on_ci()
    skip_on_cran()
    expect_true(check_scopus_pat())
})

