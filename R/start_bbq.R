#' start a bbq server
#' @param workers number of worker threads, defaults to number of physical cores
#' @param stdout where to pipe stdout
#' @param stderr where to pipe stderr
#' @export
start_bbq <- function(workers = parallel::detectCores(logical = FALSE),
                      stdout = "bbq_server.stdout",
                      stderr = "bbq_server.stderr") {
    processx::process$new("bbq", c("-w", workers), stdout = stdout, stderr = stderr)
}

#' create config
#' @param nm_executable nonmem executable
#' @param clean_lvl clean level 1-5
#' @param copy_lvl copy level 1-5
#' @param ... additional config key values to create
#' @export
create_config <- function(
    nm_executable = "nmfe74",
    clean_lvl = 2,
    copy_lvl = 2,
    ...
) {
    .dots <- list(...)
    output <- list(
        nmExecutable = nm_executable,
        cleanLvl = clean_lvl,
        copyLvl = copy_lvl
    )
    if (length(.dots)) {
       output <- modifyList(output, .dots)
    }
    return(output)
}
#' start a bbq server
#' @param config list of configuration information
#' @param ... parameters passed to [start_bbq]
#' @details
#' if config contains a configuration labelled start_dir
#' will automatically start from that directory
#' @examples \dontrun{
#' bbq_pid <- start_bbq_with_config(create_config(start_dir = "path/to/dir"))
#' }
#' @return processx process
#' @export
start_bbq_with_config <- function(
    config,
    ...
){
    if (!is.null(config$start_dir)) {
        owd <- normalizePath(getwd())
        on.exit(setwd(owd), add = TRUE)
        setwd(config$start_dir)
        config$start_dir <- NULL
    }
    config_toml <- map2(names(config), config, function(.n, .c) {
        glue::glue('{.n} = "{.c}"')
    })
    write_lines(config_toml, "babylonconfig.toml")
    start_bbq(...)
}
