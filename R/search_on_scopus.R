#' Perform a search on Scopus
#'
#' This function wraps
#' \code{rscopus::\link[rscopus]{scopus_search}} to collect
#' results of a query to
#' [Scopus](https://www.scopus.com/)
#'
#' @inheritParams complete_scopus_search
#'
#' @return An object of class
#'   \code{\link[revtools]{bibliography-class}}.
#' @export
#'
#' @references
#' \url{http://schema.elsevier.com/dtds/document/bkapi/search/SCOPUSSearchTips.htm}
#'
#' \url{https://dev.elsevier.com/api_key_settings.html}
#'
#' \url{https://dev.elsevier.com/tecdoc_developer_faq.html}
#'
#' @seealso [standard_scopus_search], [complete_scopus_search],
#'   [create_bibliography].
#'
#' @examples
#' \dontrun{
#'     # This retrieve a (short) set of results (batch size auto-resize
#'     # itself).
#'     search_on_scopus("ALL(machine AND learning) AND AUTHOR-NAME (lanera)")
#' }
search_on_scopus <- function(search_string, max_to_get = 25) {

# retrieve records ------------------------------------------------
    retrieved_lists <- if (complete_search_works()) {
        complete_scopus_search(search_string, max_to_get)
    } else {
        standard_scopus_search(search_string, max_to_get)
    }


# Set-up the bibliography -----------------------------------------
    create_bibliography(retrieved_lists)

}
