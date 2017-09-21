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
#' * `submit_models(modelpath, ...)` - check ?submit_models for arguments, the server is automatically set internally
#' * `get_models(status, STATUSES)` - get information about models that have been submitted
#' * `get_model(id)` - get information about a model
#' * `poll(.ids, until, timeout, interval, print, parse)` - poll a vector of models by ID name until completion
#'     * ids - vector if model ids
#'     * until - status criteria to poll against, default to COMPLETED or ERROR
#'     * timeout - length of time to poll before stopping
#'     * print - whether to print the status of all models each poll
#'     * parse - whether to parse the http response
#'
#' @examples \dontrun{
#' bbq <- Babylon$new()
#' bbq$get_models()
#'
#' # get all ids
#' bbq$get_models %>% map_dbl("ID")
#'
#' # find all queued models
#' bbq$get_models(status = "QUEUED") %>% map_dbl("ID")
#'
#' bbq$poll(1:5) # poll for models 1-5 to complete
#'
#' # get all directories for completed runs
#' bbq$get_models(status = "COMPLETED") %>% map_chr(~ .x$RunInfo$RunDir)
#' }
#' @docType class
#' @rdname bbq_client
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
                  version <- self$get_version()
                  if (compareVersion(version, "1.0.0") < 0) {
                     stop("please upgrade bbq to at least version 1.0.0 to maintain compatibility")
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
          get_version = function() {
              if (!is.null(private$version)) {
                  return(private$version)
              }
              resp <- safe_get(glue("{address}/version", address = private$address))$result
              if (!is.null(resp)) {
                  version <- rawToChar(resp$content)
                  private$version <- version
                  return(version)
              }
              return(FALSE)
          },
          submit_models = function(
                         modelpath,
                         clean_lvl = 1,
                         copy_lvl = 1,
                         ...,
                         .cache_dir = "",
                         .save_exe = "",
                         .exe_name = "",
                         .one_est = FALSE,
                         .print = FALSE,
                         .no_submit = FALSE,
                         .gitignore = FALSE,
                         parse = TRUE
          ) {
             resp <- submit_models(glue("{private$address}/models"),
                         modelpath = modelpath,
                         clean_lvl = clean_lvl,
                         copy_lvl = copy_lvl,
                         .cache_dir = .cache_dir,
                         .save_exe = .save_exe,
                         .exe_name = .exe_name,
                         .one_est = .one_est,
                         .print = .print,
                         .no_submit = .no_submit,
                         .gitignore = .gitignore
                        )
              if (parse) {
                  return(parse_response(resp))
              }
             return(resp)
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
          poll = function(.ids,
                          until = c("COMPLETED", "ERROR"),
                          timeout = Inf,
                          interval = 1,
                          print = FALSE,
                          parse = TRUE) {
             start_time <- Sys.time()
             tick <- time_difference(Sys.time(), start_time)
             while(tick < timeout) {
                 # this could easily be optimized to track which models have met the until status
                 # and stop polling them.
                 resp <- purrr::map(.ids, ~ self$get_model(.x, parse = parse))
                 until_status <- purrr::map_lgl(resp, ~ .x$status %in% until)
                 if (!all(until_status)) {
                     if (print) {
                        purrr::walk2(resp, until_status, function(.m, .s) {
                             message(glue("model {.m$id}, ready: {.s}, status: {.m$status}"))
                         })
                     }
                    Sys.sleep(interval)
                    tick <- time_difference(Sys.time(), start_time)
                 } else {
                     # will be parsed or not during the self$get_model
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
            address = NULL,
            version = NULL
        )
)

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
