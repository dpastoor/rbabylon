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
          get_models = function(status = NULL, STATUSES = c("QUEUED", "RUNNING", "COMPLETED", "ERROR"), parse = TRUE) {
              if (is.null(status)) {
                  models_resp <- safe_get(glue("{address}/models", address = private$address))$result
              } else {
                  if (!(status %in% STATUSES)) {
                      stop(glue("invalid status, must be one of: {paste0(STATUSES, collapse = ", ")}"))
                  }
                  models_resp <- safe_get(glue("{address}/models?status={status}", address = private$address))$result
              }
              if (parse) {
                return(parse_response(models_resp))
              }
              return(models_resp)
          },
          get_model = function(id, parse = TRUE) {
              model_resp <- safe_get(glue("{address}/models/{id}", address = private$address))$result
              if (parse) {
                 return(parse_response(model_resp))
              }
              return(model_resp)
          },
          poll = function(.ids, until = c("COMPLETED", "ERROR"), timeout = Inf, interval = 1, print = FALSE) {
             start_time <- Sys.time()
             tick <- time_difference(Sys.time(), start_time)
             while(tick < timeout) {
                 # this could easily be optimized to track which models have met the until status
                 # and stop polling them.
                 resp <- purrr::map(.ids, ~ self$get_model(.x))
                 until_status <- purrr::map_lgl(resp, ~ .x$Status %in% until)
                 if (!all(until_status)) {
                     if (print) {
                        purrr::walk2(resp, until_status, function(.m, .s) {
                             message(glue("model {.m$ID}, ready: {.s}, status: {.m$Status}"))
                         })
                     }
                    Sys.sleep(interval)
                    tick <- time_difference(Sys.time(), start_time)
                 } else {
                     return(resp)
                 }
             }
             if (print) {
                 warning("timed out!")
             }
             # right now returning null if not all complete, however it may also be reasonable
             # to return all the ones that did complete in that timeframe, or even just the last resp
             # so the user can decide what to do downstream. Maybe nested list(complete = TRUE/FALSE, resp = resp)
             return(NULL)
          }
        ),
        private = list(
            address = NULL
        )
)

#bab <- Babylon$new()
#tmpresp <- bab$get_models()

#safe_get("http://localhost:3333/models")$result
