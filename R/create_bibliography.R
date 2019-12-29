create_bibliography <- function(retrieved_object) {
    UseMethod("create_bibliography", retrieved_object)
}


create_bibliography.default <- function(retrieved_object) {
    obj_class <- class(retrieved_object)
    ui_stop(
        "Your retrieved object is of class {ui_value(obj_class)},
this class is not managed by {ui_field('create_bibliography()')} yet."
    )
}


create_bibliography.pubmed <- function(retrieved_object) {

    ui_todo("Set-up PubMed bibliography")
    pb <- pb_len(length(retrieved_object))

    # Prepare what is need for package **revtools**
    test_rows <- min(c(200, length(retrieved_object[[1]])))

    if (!test_rows) {
        bibliography <- list("0" = list())
        class(bibliography) <- "bibliography"

        ui_warn("Bibliography is empty")
        return(bibliography)
    }

    test_results <- retrieved_object[[1]][seq_len(test_rows)]
    delimiter <- revtools:::detect_delimiter(test_results)


    bibliography <- purrr::imap(retrieved_object, ~{

        res <- revtools:::prep_ris(.x, delimiter) %>%
            revtools:::read_medline()

        res[] <- purrr::map(res, ~{
            nm <- names(.x)
            if ("type" %in% nm) .x else c(type = "JOUR", .x)
        })

        tick(pb, .y)
        res
    }) %>%
        purrr::flatten()

    class(bibliography) <- "bibliography"

    ui_done("Bibliography ready")
    bibliography

}



create_bibliography.scopus <- function(retrieved_object) {

    ui_todo("Set-up Scopus bibliography")

    retrieved_object <- medline_from_scopus(retrieved_object)


    pb <- pb_len(length(retrieved_object[[1]]))

        # Prepare what is need for package **revtools**
    test_rows <- min(c(200, length(retrieved_object[[1]])))

    if (!test_rows) {
        bibliography <- list("0" = list())
        class(bibliography) <- "bibliography"

        ui_warn("Bibliography is empty")
        return(bibliography)
    }

    test_results <- retrieved_object[[1]][seq_len(test_rows)]
    delimiter <- revtools:::detect_delimiter(test_results)


    bibliography <- purrr::imap(retrieved_object, ~{

        res <- revtools:::prep_ris(.x, delimiter) %>%
            revtools:::read_medline()

        res[] <- purrr::map(res, ~{
            nm <- names(.x)
            if ("type" %in% nm) .x else c(type = "JOUR", .x)
        })

        tick(pb, .y)
        res
    }) %>%
        purrr::flatten()

    class(bibliography) <- "bibliography"

    ui_done("Bibliography ready")
    bibliography

}
