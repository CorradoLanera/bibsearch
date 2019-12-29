#' Perform a search on PubMed
#'
#' Create a \code{\link[revtools]{bibliography-class}} from a query to
#' [PubMed](https://www.ncbi.nlm.nih.gov/pubmed/)
#'
#' @inheritParams pubmed_search
#'
#' @return An object of class
#'   \code{\link[revtools]{bibliography-class}}.
#' @export
#'
#' @references
#' \url{https://www.ncbi.nlm.nih.gov/books/NBK3827/#_pubmedhelp_Search_Field_Descriptions_and_}
#'
#' @seealso [pubmed_search]
#'
#' @examples
#' \dontrun{
#'     # This retrieve a (short) set of results
#'     search_on_pubmed("(machine learning) AND lanera[Author]")
#'
#'     # More reults but retrieved just a few
#'     search_on_pubmed("machine learning", max_to_get = 200)
#'
#'     # -> ATTENTION <- big ammount of reults here: 36296! (2019-12-22)
#'     search_on_pubmed("machine learning", max_to_get = Inf)
#' }
search_on_pubmed <- function(
    search_string,
    max_to_get = 25,
    api_key = NULL
) {

# retrieve records ------------------------------------------------
    retrieved_lists <- pubmed_search(search_string, max_to_get, api_key)

# Set-up the bibliography -----------------------------------------
    create_bibliography(retrieved_lists)

}

