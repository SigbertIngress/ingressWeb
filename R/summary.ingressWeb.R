#' summary.ingressWeb
#'
#' Computes a summary for a web. Note that the mindunits are rounded up to the next integer number.
#'
#' @param object ingressWeb: ingress web object
#' @param density numeric: mind unit density (default: 1)
#' @param ... further parameters
#'
#' @return numeric: summary data about the web
#'
#' @method summary ingressWeb
#' @export
#'
#' @examples
#'
#' web  <- ingressWeb(fishbone(9))
#' summary(web)
#' web1 <- make.links(web, fishbone=3)
#' summary(web1)
summary.ingressWeb <- function (object, density=1, ...) {
  c('portals' = portals(object),
    'links'   = links(object),
     fields(object),
     ceiling(mindunits(object, density))
  )
}
