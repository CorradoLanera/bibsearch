pb_len <- function(.x, width = 76, show_after = 2, clear = FALSE) {

    if (!is.numeric(.x) || (.x != trunc(.x))) {
        ui_stop("{ui_code('.x')} must be an integer.")
    }

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

check_scopus_pat <- function() {

    if (!rscopus::have_api_key()) {
        ui_oops("You do not have setup a Scopus API key")
        ui_todo("Connect to Internet with registered IP on Scopus (i.e. Istitutional)")
        ui_info("If you do not have phisical access to an istitutional cable you would connect through VPN")
        ui_todo("Log in to Scopus (using SSO of your institution)")
        ui_todo("Clik on the link 'Scopus API' (bottom left page, in the footer)")
        ui_todo("Clik link '2. GetAPI Key >'")
        ui_todo("Give it a name, put a website (personal is ok), tick both checks")
        ui_todo("from R: {ui_code('rscopus::set_api_key(\\'<your_api_key>\\')')}")
        ui_todo("check running: {ui_code('rscopus::get_api_key()')}")
        ui_todo("You need to setup an API key! Restart when your are all set.")
        return(invisible(FALSE))
    }

    ui_done("Scopus API key correctly setup.")
    invisible(TRUE)

}


complete_search_works <- function() {

    tryCatch(
        {
            complete_scopus_search("a", 1)
            TRUE
        },
        error = function(e) FALSE
    )

}
