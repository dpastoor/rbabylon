gstop <- function(..., .call = FALSE) {
    stop(glue::glue(...), call. = .call)
}