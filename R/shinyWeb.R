#' shinyWeb
#'
#' An interactive Shiny app to create portal positions and create webs.
#'
#' Note to run \code{shinyWeb} you should install the packages \code{shiny} and \code{shinydashboard} before.
#'
#' @return runs the Shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' shinyWeb()
#' }
shinyWeb<- function() {
  appDir <- system.file("shiny", package = "ingressWeb")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
