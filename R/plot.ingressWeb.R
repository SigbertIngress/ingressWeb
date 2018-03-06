#' plot
#'
#' plots a web
#'
#' @param x ingressWeb: the web
#' @param text logical: should the portal number plotted (default: FALSE)
#' @param ... further parameter for plotting the portal position (default: asp=TRUE, xlab="x", ylab="y", pch=19, axes=FALSE)
#'
#' @return a plot with the linked web
#' @method plot ingressWeb
#' @import graphics
#' @export
#'
#' @examples
#' web <- ingressWeb(random2(15))
#' plot(web, main="Unlinked web")
plot.ingressWeb <- function (x, text=FALSE, ...) {
  args <- list(...)
  args$x <- x$x
  args$y <- x$y
  if(is.null(args$asp))  args$asp  <- TRUE
  if(is.null(args$xlab)) args$xlab <- 'x'
  if(is.null(args$ylab)) args$ylab <- 'y'
  if(is.null(args$pch))  args$pch  <- 19
  if(is.null(args$axes)) args$axes <- FALSE
  do.call('plot', args)
  if (text) text(x$x, x$y)
  n <- length(x$x)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (x$link[i,j]>0) lines(x$x[c(i,j)], x$y[c(i,j)], col=x$link[i,j])
    }
  }
  box()
}
