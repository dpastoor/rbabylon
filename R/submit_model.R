#' submit a model to an nmq server
#' @param srvr address to the server
#' @param modelpath absolute path to the model file to run
#' @param clean_lvl level to clean nonmem intermediate files
#' @param copy_lvl level to determine how many files to copy back to parent dir
#' @param ... nothing right now
#' @param .run_settings override run settings by providing a named list
#' @param .print print out the json representation of settings submitted
#' @param .no_submit dont actually submit, instead return the list of settings
#' @details \dontrun{
#' # copy and clean lvl of 5 so only table files copied up and all intermediate files deleted
#; res <- submit_model("http://localhost:3333/models", files, 5, 5)
#' }
#' @export
submit_model <- function(srvr,
                         modelpath,
                         clean_lvl = 1,
                         copy_lvl = 1,
                         ...,
                         .run_settings = NULL,
                         .print = FALSE,
                         .no_submit = FALSE) {
    submission_values <- lapply(modelpath, function(m) {
        list(
            ID = 0,
            Status = "QUEUED",
            ModelInfo = list(
                ModelPath = m,
                RunSettings = list(
                    Git = TRUE,
                    SaveExe = "",
                    Verbose = FALSE,
                    Debug = FALSE,
                    CleanLvl = clean_lvl,
                    CopyLvl = copy_lvl,
                    CacheDir = "",
                    ExeNameInCache = "",
                    NmExecutableOrPath = "nmfe74"
                )
            ),
            RunInfo = list(
                QueueTime = 0,
                StartTime = 0,
                Duration = 0
            )
        )
    })
    if (!is.null(.run_settings)) {
        stop("overriding runSettings not yet implemented")
    }
    submission_json <- jsonlite::toJSON(submission_values, auto_unbox = T)

    if (.print) {
        print(submission_json)
    }
    if (.no_submit) {
        return(submission_values)
    }
    httr::POST(srvr, body = submission_json, encode = "json")
}

