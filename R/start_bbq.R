#' get the bbq version on the system
#' @param bbq_path path to bbq
#' @export
bbq_version <- function(bbq_path = "bbq") {
    ver <- tryCatch(
        processx::process$new(bbq_path, "--version", stdout = "|"),
        error = function(e) e
    )
    if (inherits(ver, 'simpleError')) {
        gstop("error trying to detect bbq, are you sure it is on path {bbq_path}")
    }
    package_version(ver$read_all_output_lines())
}

#' start a bbq server
#' @param bbq_path path to bbq DEFAULT: 'bbq'
#' @param workers number of worker threads, defaults to number of physical cores
#' @param stdout where to pipe stdout
#' @param stderr where to pipe stderr
#' @export
start_bbq <- function(bbq_path = "bbq",
                      workers = parallel::detectCores(logical = FALSE),
                      stdout = "bbq_server.stdout",
                      stderr = "bbq_server.stderr") {
    bbqv <- bbq_version(bbq_path)
    if (!(bbqv >= package_version("1.1.1"))) {
        stop("bbq version must be >= 1.1.1")
    }
    pid <- processx::process$new(bbq_path, c("-w", workers),
                                 stdout = stdout, stderr = stderr)
    "!DEBUG starting bbq with version `bbqv` on pid `pid$get_pid()`"
    return(pid)
}

#' create config
#' @param nm_executable nonmem executable
#' @param clean_lvl clean level 1-5
#' @param copy_lvl copy level 1-5
#' @param ... additional config key values to create
#' @importFrom utils modifyList
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
#'
#' configuration is set by writing the babylonconfig.toml
#' file in that directory, which should be picked up
#' @examples \dontrun{
#' bbq_pid <- start_bbq_with_config(create_config(start_dir = "path/to/dir"))
#' }
#' @return processx process
#' @importFrom utils modifyList
#' @export
start_bbq_with_config <- function(
    config,
    ...
){
    if (!is.null(config$start_dir)) {
        owd <- normalizePath(getwd())
        on.exit(setwd(owd), add = TRUE)
        setwd(config$start_dir)
        # this will remove start_dir
        config <- modifyList(config, list(start_dir = NULL))
    }
    if (length(config)) {
        config_toml <- purrr::map2(names(config), config, function(.n, .c) {
            # this is pretty hacky but basically can keep the resulting
            # toml to generate a string or numeric quote for outputs
            if (is.numeric(.c)) {
                glue::glue('{.n} = {.c}')
            } else {
                glue::glue('{.n} = "{.c}"')
            }
        })
        readr::write_lines(config_toml, "babylonconfig.toml")
    }
    start_bbq(...)
}
