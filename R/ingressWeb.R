#' ingressWeb
#'
#' creates an empty web from x-y coordinates or updates/an existing web
#'
#' @param x numeric(n,2): data frame with x-y coordinates or ingressWeb object
#' @param ... further (named) parameters
#'
#' @return a ingressWeb object
#' @rdname ingressWeb
#' @export
#'
#' @examples
#' web <- ingressWeb(random2(15))
#' plot(web)
ingressWeb <- function(x, ...) { UseMethod("ingressWeb") }

#' @rdname ingressWeb
#' @method ingressWeb default
#' @export
ingressWeb.default<- function (x, ...) {
  xycoords <- function(x, y=NULL) {
    xr   <- NULL
    yr   <- NULL
    name <-sname <- NULL
    if (is.null(y)) {
      if (is.data.frame(x)) {
        if(is.null(x$x)) {
          xn <- which(sapply(x, is.numeric))
          if (length(xn)>0) xr <- x[,xn[1]]
        } else xr <- x$x
        if(is.null(x$y)) {
          xn <- which(sapply(x, is.numeric))
          if (length(xn)>1) yr <- x[,xn[2]]
        } else yr <- x$y
        if (!is.null(x$name)) name <- x$name
        if (!is.null(x$shortname)) sname <- x$shortname
      }
      if (is.list(x)) {
        if(!is.null(x$x)) xr <- x$x
        if(!is.null(x$y)) yr <- x$y
        if (!is.null(x$name)) name <- x$name
        if (!is.null(x$shortname)) sname <- x$shortname
      }
      if (is.matrix(x)) {
        xr<- if ('x' %in% colnames(x)) x[,'x'] else x[,1]
        xr<- if ('y' %in% colnames(x)) x[,'y'] else x[,2]
      }
    } else {
      xr <- x
      yr <- y
    }
    if (is.null(xr)) stop('No x found')
    if (is.null(yr)) stop('No y found')
    if (length(xr)!=length(yr)) stop('x and y length differs')
    if (is.null(name)) name <- as.character(1:length(xr))
    return(list(x=xr, y=yr, name=name, shortname=sname))
  }
  xy         <- xycoords(x, ...)
  ret        <- list(x=xy$x, y=xy$y, name=xy$name, shortname=xy$shortname, link=matrix(0, ncol=length(xy$x), nrow=length(xy$y)))
  class(ret) <- 'ingressWeb'
  return(ret)
}

#' @rdname ingressWeb
#' @method ingressWeb ingressWeb
#' @export
ingressWeb.ingressWeb <- function (x, ...) {
  args <- list(...)
  for (arg in names(args)) x[[arg]] <- args[[arg]]
  x
}
