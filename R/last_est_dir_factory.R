#' factory to create a mapping to an estimation directory
#' @param basepath the base directory path of the estimation directory
#' @details
#' returns a function that will determine the last estimation directory
#' given a run name, for example, given run001_est_01 run001_est02
#' last_est_dir("run001") --> "path/to/run001_est_01"
#' @export
last_est_dir_factory <- function(basepath) {
    return(function(run, subdir = "") {
    location <- normalizePath(file.path(basepath, subdir))
    dir_info <- normalizePath(dir(location,
                    pattern = paste0(run, "_est_"),
                    full.names = T))
    if (!length(dir_info)) {
        stop(paste0("no estimation directories detected at:", location))
    }

    file.path(dir_info[length(dir_info)])
    })
}

