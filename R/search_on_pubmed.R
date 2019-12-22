#' Perform a search on PubMed
#'
#' This function wraps
#' \code{easyPubMed::\link[easyPubMed]{get_pubmed_ids}} and
#' \code{easyPubMed::\link[easyPubMed]{fetch_pubmed_data}} to collect
#' results of a query to
#' [PubMed](https://www.ncbi.nlm.nih.gov/pubmed/)
#'
#' @param search_string (character vector of length 1) that is used for
#'   querying PubMed (standard PubMed syntax, see reference for
#'   details).
#' @param batch_size Integer (>= 1): size of the batch of PubMed records
#'   to be retrieved at one time.
#' @param max_to_get Integer (>= 1): maximum number of records to
#'   retrieve. Note that, if they are not finished, a complete batch
#'   will be always retrieved; hence, more than `max_to_get` records
#'   will be retrieved Set it to `Inf` to be sure to retrieve all
#'   the possible results.
#' @param api_key (character vector of length 1): user-specific API key
#'   to increase the limit of queries per second above three. You can
#'   obtain your key from NCBI (see
#'   \url{https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/}
#'
#' @return An object of class
#'   \code{\link[revtools]{bibliography-class}}.
#' @export
#'
#' @references
#' \url{https://www.ncbi.nlm.nih.gov/books/NBK3827/#_pubmedhelp_Search_Field_Descriptions_and_}
#'
#' @examples
#' \dontrun{
#'     # This retrieve a (short) set of results (batch size auto-resize
#'     # itself).
#'     search_on_pubmed("(machine learning) AND lanera[Author]")
#'
#'     # More reults but retreivet just a few
#'     search_on_pubmed("machine learning", max_to_get = 200)
#'
#'     # -> ATTENTION <- big ammount of reults here: 36296! (2019-12-22)
#'     search_on_pubmed("machine learning",
#'         batch_size = 5000,       # this is the maximum for Scopus API
#'         max_to_get = Inf
#'     )
#' }
search_on_pubmed <- function(
    search_string,
    batch_size = 25,
    max_to_get = 25,
    api_key = NULL
) {

# formal checks ---------------------------------------------------

    assertive::assert_is_a_string(search_string)

    assertive::assert_is_a_number(batch_size)
    assertive::assert_all_are_whole_numbers(batch_size)
    assertive::assert_all_are_positive(batch_size)
    assertive::assert_all_are_less_than_or_equal_to(batch_size, 5000)

    (max_to_get == Inf) || (
        assertive::assert_is_a_number(max_to_get) &&
            assertive::assert_all_are_whole_numbers(max_to_get) &&
            assertive::assert_all_are_positive(max_to_get)
    )

    if (!is.null(api_key)) assertive::assert_is_a_string(api_key)

# retrieve records ------------------------------------------------

    entrez_ids <- easyPubMed::get_pubmed_ids(search_string, api_key)


    # We need to iterated over steps of size = batch_size to retrieve
    # all the records
    n_records <- min(as.integer(entrez_ids[["Count"]]), max_to_get)

    if (n_records < batch_size) {
        ui_warn("{ui_field('batch_size')} is {ui_value(batch_size)}, greater than number of records ({ui_value(n_records)}).")
        ui_todo("Resizing {ui_field('batch_size')}...")
        batch_size <- n_records
        ui_done("{ui_field('batch_size')} resized to {ui_value(n_records)}.")
    }

    steps <- purrr::set_names(
        seq(from = 0L, to = n_records - 1, by = batch_size)
    )


    ui_todo("starting retrieving records from PubMed.")

    # Do not want to brake the retrievement in case of possible errors
    # in some batches.
    safe_retrieve <- purrr::safely(easyPubMed::fetch_pubmed_data)


    pb <- depigner::pb_len(length(steps))
    retrived_lists <- purrr::imap(steps, ~ {
        res <- safe_retrieve(
            entrez_ids,
            retstart = .x,
            retmax = batch_size,
            format = "medline"
        )
        depigner::tick(pb, .y)
        res
    }) %>%
        purrr::transpose()
    ui_done("All records retreived.")

    # In case of some errors, we will work on correct data only
    errors <- retrived_lists[["error"]]
    oks <- purrr::map_lgl(errors, is.null)
    results <- retrived_lists[["result"]][oks]


# Set-up the bibliography -----------------------------------------

    # Prepare what is need for package **revtools**
    test_rows <- min(c(200, length(results[[1]])))
    test_results <- results[[1]][seq_len(test_rows)]
    delimiter <- revtools:::detect_delimiter(test_results)

    ui_todo("Set-up bibliography")
    pb <- depigner::pb_len(length(results))
    bibliograpy <- purrr::imap(results, ~{

        res <- revtools:::prep_ris(.x, delimiter) %>%
            revtools:::read_medline()
        res[] <- purrr::map(res, ~{
            nm <- names(.x)
            if ("type" %in% nm) .x else c(type = "JOUR", .x)
        })

        depigner::tick(pb, .y)
        res
    })
    ui_done("Bibliography ready")



# Allert errors, and return teh bibliography ----------------------

    if (!all(oks)) ui_warn(
        "Batches starting at {ui_field(names(oks[!oks]))} non retreived (nor exported)."
    )

    structure(purrr::flatten(bibliograpy), class = "bibliography")
}

