#' An Rmd child template that produces a collapsed session info box
#'
#' @section Examples:
#'
#' @export
child_session_info <- function() {
  knitr::knit_child(fs::path_package("rmdtmp/session_info.Rmd",
                                     package = "hugodownplus"),
                    envir = environment(),
                    quiet = TRUE)
}
