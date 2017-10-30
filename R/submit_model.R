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
#' @param nm_executable_or_path name or path to nonmem executable, DEFAULT: nmfe74
#' @param debug debug mode
#' @param verbose verbose output from server
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
                         .gitignore = FALSE,
                         nm_executable_or_path = "nmfe74",
                         debug = FALSE,
                         verbose = FALSE
                         ) {
    submission_values <- lapply(modelpath, function(m) {
        list(
            id = 0,
            status = "QUEUED",
            model_info = list(
                model_path = m,
                run_settings = list(
                    git = .gitignore,
                    save_exe = .save_exe,
                    verbose = verbose,
                    debug = debug,
                    clean_lvl = clean_lvl,
                    copy_lvl = copy_lvl,
                    cache_dir = .cache_dir,
                    exe_name_in_cache = .exe_name,
                    nm_executable_or_path = "nmfe74",
                    one_est = .one_est,
                    proposed_run_dir = ""
                )
            ),
            run_info = list(
                queue_time = 0,
                start_time = 0,
                duration = 0,
                run_dir = "",
                error = ""
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

