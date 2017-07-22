safe_get <- purrr::safely(httr::GET)

parse_response <- function(resp) {
    if (!is.null(resp)) {
       return(jsonlite::fromJSON(rawToChar(resp$content), simplifyDataFrame = F))
    }
    return(NULL)
}

#' calculate the time difference
#' @param start_time the start time
#' @param end_time the end time
#' @param units units to calculate, defaults to secs
#' @export
time_difference <- function(end_time, start_time, units = "secs") {
    as.numeric(difftime(end_time, start_time, units = units))
}

#' Create a new Babylon server manager
#'
#' The Babylon generator creates a new 'Babylon'-object, which contains logic to interact
#' with a running bbq server from the babylon ecosystem.
#'
#' @format NULL
#' @usage NULL
#'
#'
#' @importFrom R6 R6Class
#' @importFrom glue glue
#'
#' @section Initialization:
#' A new Babylon object is initialized with the new() method:
#'
#' `bbq <- Babylon$new()`
#'
#' and can optionally take some initialization arguments:
#'
#' * `init` - whether to initialize a bbq instance on the local machine
#' * `host` - the hostname the server is running on - defaults to 'http://localhost'
#' * `port` - port the server is listening on - defaults to 3333.
#' * `verbose` - whether internal status information should be displayed
#' * `mustWork` - on initialization, check and confirm a bbq server is listening on the host/port configuration set
#'
#' @section Methods:
#' * `get_models(status, STATUSES)` - get information about models that have been submitted
#' * `get_model(id)` - get information about a model
#' @export
Babylon <-
    R6::R6Class("Babylon",
        public = list(
          initialize = function(init = NULL,
                                host = "http://localhost",
                                port = 3333,
                                verbose = TRUE,
                                must_work = TRUE) {
              if (!is.null(init)) {
                  stop("init functionality not yet implemented")
              }
              private$address <- ifelse(!is.null(port), glue("{host}:{port}"), host)
              if (must_work) {
                  if(!self$ping()) {
                      stop(glue("server not responding at {srvr}", srvr = private$address))
                  }
              }
              if (verbose) {
                 message("")
              }
          },
          ping = function() {
              resp <- safe_get(glue("{address}/ping", address = private$address))$result
              if (!is.null(resp)) {
                  return(TRUE)
              }
              return(FALSE)
          },
          submit_models = function(...) {
              submit_models(private$address, ...)
          },
          get_models = function(status = NULL, STATUSES = c("QUEUED", "RUNNING", "COMPLETE", "ERROR")) {
              if (is.null(status)) {
                  models_resp <- safe_get(glue("{address}/models", address = private$address))$result
              } else {
                  if (!(status %in% STATUSES)) {
                      stop(glue("invalid status, must be one of: {paste0(STATUSES, collapse = ", ")}"))
                  }
                  models_resp <- safe_get(glue("{address}/models?status={status}", address = private$address))$result
              }
              return(models_resp)

          }
        ),
        private = list(
            address = NULL
        )
)

#bab <- Babylon$new()
#tmpresp <- bab$get_models()

#safe_get("http://localhost:3333/models")$result
