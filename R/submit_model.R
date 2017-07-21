#' submit models to an bbq server
#' @param srvr address to the server
#' @param modelpath absolute path to the model file to run
#' @param clean_lvl level to clean nonmem intermediate files
#' @param copy_lvl level to determine how many files to copy back to parent dir
#' @param ... nothing right now
#' @param .cache_dir cache directory
#' @param .save_exe name to save the precompiled executable for later reference
#' @param .exe_name name of executable to use for execution
#' @param .one_est only estimate if no estimation dir exists
#' @param .print print out the json representation of settings submitted
#' @param .no_submit dont actually submit, instead return the list of settings
#' @param .gitignore add a blanket gitignore to ignore the folder from git
#' @examples \dontrun{
#' # copy and clean lvl of 5 so only table files copied up and all intermediate files deleted
#' res <- submit_model("http://localhost:3333/models", files, 5, 5)
#' }
#' @export
submit_models <- function(srvr,
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
                         .gitignore = FALSE) {
    submission_values <- lapply(modelpath, function(m) {
        list(
            ID = 0,
            Status = "QUEUED",
            ModelInfo = list(
                ModelPath = m,
                RunSettings = list(
                    Git = .gitignore,
                    SaveExe = .save_exe,
                    Verbose = FALSE,
                    Debug = FALSE,
                    CleanLvl = clean_lvl,
                    CopyLvl = copy_lvl,
                    CacheDir = .cache_dir,
                    ExeNameInCache = .exe_name,
                    NmExecutableOrPath = "nmfe74",
                    OneEst = .one_est
                )
            ),
            RunInfo = list(
                QueueTime = 0,
                StartTime = 0,
                Duration = 0
            )
        )
    })

    submission_json <- jsonlite::toJSON(submission_values, auto_unbox = T)

    if (.print) {
        print(submission_json)
    }
    if (.no_submit) {
        return(submission_values)
    }
    httr::POST(srvr, body = submission_json, encode = "json")
}

