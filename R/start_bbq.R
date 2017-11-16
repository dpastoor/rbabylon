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
