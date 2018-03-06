#' portals
#'
#' Returns the number portals for a web.
#'
#' @param plan ingressWeb: ingress web object
#'
#' @return integer: the number of portals
#' @export
#'
#' @examples
#' web  <- ingressWeb(random2(10))
#' portals(web)
portals <- function(plan) {
  if (!'ingressWeb' %in% class(plan)) stop('ingressWeb object required')
  length(plan$x)
}
