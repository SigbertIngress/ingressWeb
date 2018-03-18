
#' accesspoints
#'
#' Computes the access points (AP) for a web. Since the AP for building a web depends on current status of the portals, links and fields
#' \code{accesspoints} can give only very rough estimates. Therefore it is not integrated in \code{summarize}. It will return a vector with
#' the components
#'
#' \describe{
#' \item{\code{portal_ap}}{gives the AP you get if you capture all portals and deploy always eight resonators,}
#' \item{\code{mod_ap}}{gives the AP if deploy each portal with two mods,}
#' \item{\code{link_ap}}{gives the AP if you make all links and}
#' \item{\code{field_ap}}{gives the AP if you make all fields.}
#' }
#'
#' @param plan ingressWeb: Ingress web object
#'
#' @return numeric: a vector with accesspoints.
#' @export
#'
#' @examples
#' set.seed(0)
#' web  <- ingressWeb(random2(10))
#' web2 <- make.links(web, maxarea=10)
#' accesspoints(web2)
accesspoints <- function (plan) {
  c('portal_ap'=portals(plan)*(500+1125), 'mod_ap'=250*portals(plan), 'link_ap'=links(plan)*313, 'field_ap'=1250*max(fields(plan)))
}
