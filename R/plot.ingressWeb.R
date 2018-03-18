#' plot
#'
#' plots a web. The plot consists of two plots: the web and it's link and additionally of a frequency table of links per portal (\code{links(x, TRUE)}).
#' If either \code{pos} or \code{palette} is set the plot of the frequency table is shown. Possible values for \code{pos} are \code{"topright"},
#' \code{"topleft"}, \code{"bottomleft"} or \code{"bottomright"}.
#'
#' @param x ingressWeb: the web
#' @param text logical: should the portal number plotted (default: FALSE)
#' @param pos text: position of plot table portals by number of links (default: NULL, no plotting)
#' @param palette color(s): colors used for plotting portal position by number of links (default: black). Note that using \code{col=...} overturns this setting.
#' @param ... further parameter for plotting the portal position (default: asp=TRUE, xlab="x", ylab="y", pch=19, axes=FALSE, col="black")
#'
#' @return a plot with the linked web
#' @method plot ingressWeb
#' @import graphics TeachingDemos
#' @export
#'
#' @examples
#' web <- ingressWeb(random2(15))
#' plot(web, main="Unlinked web")
plot.ingressWeb <- function (x, text=FALSE, palette=NULL, pos=NULL, ...) {
  args <- list(...)
  args$x <- x$x
  args$y <- x$y
  if(is.null(args$asp))  args$asp  <- TRUE
  if(is.null(args$xlab)) args$xlab <- 'x'
  if(is.null(args$ylab)) args$ylab <- 'y'
  if(is.null(args$pch))  args$pch  <- 19
  if(is.null(args$axes)) args$axes <- FALSE
  lpp <- links(x, TRUE)
  if (is.null(pos) && !is.null(palette)) pos <- 'topright'
  if(is.null(args$col)) {
    if (!is.null(palette)) {
      if (palette=='rainbow') palette <- c('black', rev(rainbow(9, start=0, end=2/3)))
      col      <- 1+lpp
      col[col>length(palette)] <- length(palette)
      args$col <- palette[col]
    }
  }
  do.call('plot', args)
  if (!is.null(pos) && (max(lpp)>0)) {
    lvl <- c(0:max(c(8, lpp)))
    tab <- table(factor(lpp, levels=lvl))
    subplot({ if(is.null(palette)) col='black' else {
                col <- lvl+1
                col[col>length(palette)] <- length(palette)
                col <- palette[col]
              }
              plot(tab, axes=FALSE, xlab='', ylab='', col=col)
              axis(3, at=seq(0, max(lvl), by=8))
            }, x=pos)
  }
  if (text) text(x$x, x$y)
  n <- length(x$x)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (x$link[i,j]>0) lines(x$x[c(i,j)], x$y[c(i,j)], col=x$link[i,j])
    }
  }
  box()
}
