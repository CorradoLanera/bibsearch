biblio_short <- suppressWarnings(search_on_pubmed(
    "(machine learning) AND lanera[Author]"
))

test_that("stops on wrong input", {
    expect_error(write_bibliography(c("a", "b")))
    expect_error(write_bibliography(biblio_short, file_type = "medline"))
    expect_error(write_bibliography(biblio_short, out_dir = 1))
    expect_error(write_bibliography(biblio_short, base_name = 1))
    expect_error(write_bibliography(biblio_short, base_name = "1_name"))
    expect_error(
        write_bibliography(biblio_short, max_each_file = c(1, 2))
    )
    expect_error(
        write_bibliography(biblio_short, max_each_file = "1")
    )
    expect_error(
        write_bibliography(biblio_short, max_each_file = 1.5)
    )
    expect_error(
        write_bibliography(biblio_short, max_each_file = -1)
    )
    expect_error(
        write_bibliography(biblio_short, max_each_file = 5000)
    )
})


out_dir <- tempdir()
write_bibliography(biblio_short, out_dir = out_dir, max_each_file = 3)
write_bibliography(biblio_short, out_dir = out_dir, base_name = "foo")

test_that("write files", {
    expect_true(write_bibliography(biblio_short, out_dir = out_dir))
    expect_true(suppressWarnings(
        write_bibliography(
            biblio_short,
            out_dir = file.path(out_dir, "foodir")
        )
    ))
    expect_true(fs::file_exists(file.path(out_dir, "exported.ris")))
    expect_true(
        fs::file_exists(file.path(out_dir, "foodir", "exported.ris"))
    )

    expect_true(
        write_bibliography(biblio_short,
            out_dir = out_dir,
            file_type = "bib"
        )
    )
    expect_true(fs::file_exists(file.path(out_dir, "exported.bib")))
    expect_true(fs::file_exists(file.path(out_dir, "exported_1.ris")))
    expect_true(fs::file_exists(file.path(out_dir, "exported_4.ris")))
    expect_true(fs::file_exists(file.path(out_dir, "foo.ris")))
})
