#' An Rmd child template that produces a collapsed session info box
#'
#' When called in an Rmd document this function generates child document containing an expandable
#' session info box.
#'
#' @param pkgs Which packages to show. It may be:
#'   * `NULL` or `"loaded"`: show all loaded packages,
#'   * `"attached"`: show all attached packages,
#'   * `"installed"`: show all installed packages,
#'   * a character vector of package names. Their (hard) dependencies are
#'     also shown by default, see the `dependencies` argument.
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
child_session_info <- function(pkgs = c("loaded", "attached", "installed")[1]) {
  knitr::knit_child(fs::path_package("rmdtmp/session_info.Rmd",
                                     package = "hugodownplus"),
                    envir = environment(),
                    quiet = TRUE)
}
