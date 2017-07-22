#' read fort files and apply a name structure
#' @param basepath base directory
#' @param .fort fortran file name
#' @param .names column names
#' @examples \dontrun{
#' read_fort51 <- read_fort_factory("../modeling", "fort.51")
#' read_fort51(last_est_dir("run001"))
#' read_fort51("run001_est_01")
#' }
#' @export
read_fort_factory <- function(basepath,
                        .fort,
                        .names = c("ITERATION", "ID", "TIME", "F", "DV", "CL", "V")
) {
    default_names <- .names
    return(function(runpath, .names = NULL) {
        data <- data.table::fread(normalizePath(file.path(basepath, runpath, .fort), mustWork = F),
                                  data.table = F,
                                  header = F,
                                  na.strings = ".")
        if (is.null(.names)) {
            .names <- default_names
        }
        if (length(data) != length(.names)) {
            stop("column name mismatch, not naming")
        }
        names(data) <- .names
        return(tibble::as_tibble(data))
    })
}
