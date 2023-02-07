#' An Rmd child template that produces a collapsed session info box
#'
#' When called in an Rmd document this function generates child document containing an expandable
#' session info box.
#'
#' @section Examples:
#'
#' Call this function on a separate line using inline code `{r, eval = FALSE, echo = TRUE}r child_session_info()`.
#'
#' @section css:
#' In CSS Use the `.session` class to target the whole expandable box.
#' Use `summary.session-header` to target the box header.
#' Use `.session-details` to target the content of the box.
#'
#' @export
child_session_info <- function() {
  knitr::knit_child(fs::path_package("rmdtmp/session_info.Rmd",
                                     package = "hugodownplus"),
                    envir = environment(),
                    quiet = TRUE)
}
