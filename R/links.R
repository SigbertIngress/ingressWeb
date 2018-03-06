#' links
#'
#' Returns the number links for web. For a full web (all portals included) the number should be 3*#portals-6.
#'
#' @param plan ingressWeb: ingress web object
#' @param perportal logical: return links per portal or the whole web (default: FALSE)
#'
#' @return integer: the number of links
#' @export
#'
#' @examples
#' web  <- ingressWeb(random2(10))
#' links(web)
#' web1 <- make.links(web)
#' links(web1)
#' # Number of links per portal
#' plot(table(links(web1, TRUE)))
links <- function(plan, perportal=FALSE) {
  if (!'ingressWeb' %in% class(plan)) stop('ingressWeb object required')
  ret <- rowSums(plan$link>0)-diag(plan$link>0)
  if (perportal) return(ret)
  sum(ret)/2
}
