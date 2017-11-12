#' create a new client for babylon queue
#' @return Babylon instance
#' @param init whether to initialize bbq
#' @param host hostname
#' @param port port bbq listens on
#' @param verbose more information about whats going on under the hood
#' @param must_work must be able to ping a valid bbq server to initialize
#' @export
bbq_client <- function(init = NULL,
                       host = "http://localhost",
                       port = 3333,
                       verbose = TRUE,
                       must_work = TRUE) {
    Babylon$new(init = init,
                host = host,
                port = port,
                verbose = verbose,
                must_work = must_work)
}

#bab <- Babylon$new()
#tmpresp <- bab$get_models()

#safe_get("http://localhost:3333/models")$result
