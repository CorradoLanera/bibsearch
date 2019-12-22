pb_len <- function(.x, width = 76, show_after = 2, clear = FALSE) {

    if (!is.numeric(.x) || (.x != trunc(.x))) usethis::ui_stop(
        "{usethis::ui_code('.x')} must be an integer."
    )

    progress::progress_bar$new(
        format =
            "evaluated: :what [:bar] :percent in :elapsed [ETA: :eta]",
        total = .x,
        width = width,
        clear = clear,
        show_after = show_after
    )
}

tick <- function(pb, what = "") {
    pb$tick(tokens = list(what = what))
}
