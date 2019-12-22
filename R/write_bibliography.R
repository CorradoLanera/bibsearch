#' Export Research form a bibliography
#'
#' This function store in a file a bibliography of class
#' \code{\link[revtools]{bibliography-class}} both in RIS and BibTeX
#' file-format.,Moreover, it includes an option to split the
#' bibliography in multiple files accordingly to an user-defined number
#' of records to write in each file.
#'
#' @param bib an object of class
#'   \code{\link[revtools]{bibliography-class}}
#' @param file_type (character vector of length 1) Format of the
#'   exported file. Should be either "ris" (default) or "bib"
#' @param out_dir (character vector of length 1): path for the directory
#'   in which to store the output (default is the current/project one)
#' @param base_name (character vector of length 1): base name (w/o
#'   extension) for the output file(s)
#' @param max_each_file Integer (>=1) number of record to store
#'   and write into each file. Default is `length(bib)`, which means
#'   that a single file will be written including all the records in the
#'   bibliography.
#'
#' @return invisibly `TRUE`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'     # This save the (short) set of results in the current project
#'     # directory as a RIS file.
#'     biblio_short <- search_on_pubmed(
#'         "(machine learning) AND lanera[Author]"
#'     )
#'     write_bibliography(biblio_short)
#'     write_bibliography(biblio_short, file_type = "bib")
#'#'
#'     # Big ammount of reults here (we retain only a few).
#'     ml_reduced <- search_on_pubmed("machine learning",
#'         max_to_get = 50
#'     )
#'
#'     # Write some RIS files, each with a limited pre-defined ammount
#'     # of record, and save them in a specific folder.
#'     write_bibliography(ml_reduced,
#'         out_dir = "multiple",
#'         max_each_file = 25
#'     )
#'
#'     # Or save them all into a single file (default)
#'     write_bibliography(ml_reduced, out_dir = "single")
#' }
write_bibliography <- function(
    bib,
    file_type = c("ris", "bib"),
    out_dir = here::here(),
    base_name = "exported",
    max_each_file = length(bib)
) {

    # formal checks ---------------------------------------------------

    assertive::assert_is_any_of(bib, "bibliography")

    file_type <- match.arg(file_type)

    assertive::assert_is_a_string(out_dir)
    if (!fs::dir_exists(out_dir)) {
        ui_warn("Directory {ui_code(out_dir)} does not exists.")
        ui_todo("Try to create {ui_code(out_dir)}.")
        fs::dir_create(out_dir)
        ui_done("Directory {ui_code(out_dir)} created.")
    }

    assertive::assert_is_a_string(base_name)
    assertive::assert_all_are_valid_variable_names(base_name)

    assertive::assert_is_a_number(max_each_file)
    assertive::assert_all_are_whole_numbers(max_each_file)
    assertive::assert_all_are_positive(max_each_file)
    assertive::assert_all_are_greater_than_or_equal_to(
        length(bib), max_each_file
    )


# Export records --------------------------------------------------

    step <- purrr::set_names(seq(1, length(bib), by = max_each_file))

    if (length(step) == 1) {

        ui_todo("Saving output file on disk.")
        output_path <- fs::path(out_dir, base_name, ext = file_type)
        revtools::write_bibliography(bib, output_path, file_type)
        ui_done("file {ui_code(output_path)} written on disk.")

    } else {

        purrr::iwalk(step, ~{
            output_path <- fs::path(
                out_dir, paste0(base_name, "_", .y), ext = file_type
            )
            start <- .x
            end <- min(max_each_file * .x, length(bib))

            revtools::write_bibliography(bib[start:end],
                filename = output_path,
                format = file_type
            )
            ui_done("file {ui_code(output_path)} written on disk.")
        })
        ui_done("All files saved on disk")

    }

    invisible(TRUE)
}
