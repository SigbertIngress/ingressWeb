#' ingressWeb
#'
#' creates an empty web from x-y coordinates or updates/an existing web
#'
#' @param x numeric(n,2): x-y coordinates or ingressWeb object
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
  args <- list(...)
  args$x <- x
  xy <- do.call('xy.coords', args)
  ret <- list(x=xy$x, y=xy$y, link=matrix(0, ncol=length(xy$x), nrow=length(xy$y)))
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
