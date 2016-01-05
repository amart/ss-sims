#' Build the development version of r4ss from GitHub.
#' Copied from github: hadley/R/build-github-devtools.r
#'
#' This function is especially useful for Windows users who want to upgrade
#' their version of r4ss to the development version hosted on on GitHub.
#' In Windows, it's not possible to upgrade r4ss while the package is loaded
#' because there is an open DLL, which in Windows can't be overwritten. This
#' function allows you to build a binary package of the development version of
#' r4ss; then you can restart R (so that r4ss isn't loaded) and install
#' the package.
#'
#' Mac and Linux users don't need this function; they can use
#' \code{\link{install_github}} to install r4ss directly, without going
#' through the separate build-restart-install steps.
#'
#' This function requires a working development environment. On Windows, it
#' may need \url{http://cran.r-project.org/bin/windows/Rtools/}.
#'
#' @param outfile The name of the output file. If NULL (the default), it uses
#'   ./r4ss.tgz (Mac and Linux), or ./r4ss.zip (Windows).
#' @return a string giving the location (including file name) of the built
#'  package
#' @examples
#' \dontrun{
#' library(devtools)
#' build_github_r4ss()
#'
#' #### Restart R before continuing ####
#' install.packages("./r4ss.zip", repos = NULL)
#'
#' # Remove the package after installation
#' unlink("./r4ss.zip")
#' }
#' @export
build_github_r4ss <- function(github.user="r4ss",outfile = NULL) {

  library(devtools)

#   if (!has_devel()) {
#     stop("This requires a working development environment.")
#   }

  ext <- if (.Platform$OS.type == "windows") "zip" else "tgz"
  outfile <- paste0("./r4ss.", ext)

  url <- paste("https://github.com/",github.user,"/r4ss/archive/master.zip",sep="")
  message("Downloading r4ss from ", url)
  bundle <- file.path(tempdir(), "r4ss-master.zip")

  # Download package file
  request <- httr::GET(url)
  httr::stop_for_status(request)
  writeBin(httr::content(request, "raw"), bundle)
  on.exit(unlink(bundle))

  utils::unzip(bundle, exdir = tempdir())

  # Build binary package
  pkgdir <- file.path(tempdir(), "r4ss-master")
  built_pkg <- devtools::build(pkgdir, binary = TRUE)

  message("Renaming file to ", outfile)
  file.rename(built_pkg, outfile)

  invisible(outfile)
}
