#' mindunits
#'
#' Returns the miminal (no multilayers) and maximal (all possible multilayers) mind units of a web.
#' The mind units in Ingress are calculated by the area of the fields multiplied with some  mind unit density.
#' The mind unit density depends on the population living in that area, but exact values for the mind unit density
#' are not known for the public. Therefore the computation can be very rough. If density is set to one (default)
#' then area of the fields will calculated.
#'
#' @details Since Niantic does not give much information about mind unit calculation. It is believed that the formula is
#' \eqn{MU=area*(a+b*population density)}. This supported by the calculations of Matthew Baumgartner (see \url{http://www.fieldsizeestimator.com/}).
#' How exactly the population density is computed is also not known, but is known that it has changed over time.
#'
#' The largest field ever created:
#' \preformatted{
#' 12:55PM SSKumk created a Control Field @Фигурачистильщика,околообув
#' (ulitsa Kirova, 139, Chelyabinsk, Chelyabinsk Oblast, Russia) +728600617722 MUs
#' }
#' It turned out that there was a bug in Niantics MU calculation. Not the area between the three portals was used for the calculation,
#' but all the area outside was computed.
#'
#' @param plan ingressWeb: ingress web object
#' @param density numeric: mutliplicator for the mindunit density (default: 1)
#'
#' @return numeric(2): Mimimal and maximal area of all fields (if density=1)
#' @export
#'
#' @examples
#' web  <- ingressWeb(random2(10))
#' mindunits(web)
#' web1 <- make.links(web)
#' mindunits(web1)
mindunits <- function (plan, density=1) {
  area <- function(x, y, i, j, k) {
    # from https://math.stackexchange.com/questions/516219/finding-out-the-area-of-a-triangle-if-the-coordinates-of-the-three-vertices-are
    abs((x[j]-x[i])*(y[k]-y[i])-(x[k]-x[i])*(y[j]-y[i]))
  }
#
  if (!'ingressWeb' %in% class(plan)) stop('ingressWeb object required')
  n  <- nrow(plan$link)
  f  <- 0
  if (!'ingressWeb' %in% class(plan)) stop('ingressWeb object required')
  n  <- nrow(plan$link)
  fmin <- fmax <- 0
  for (i in 1:(n-2)) {
    for (j in (i+1):(n-1)) {
      if (plan$link[i,j]) {
        for (k in (j+1):n) {
          if (plan$link[i,k] && plan$link[j,k]) {
            bc   <- (rowSums(barycentric(plan$x[-c(i,j,k)], plan$y[-c(i,j,k)], plan$x[c(i,j,k)], plan$y[c(i,j,k)])>0)==3)
            fmax <- fmax+area(plan$x, plan$y, i, j, k)
            if (!sum(bc)) fmin <- fmin+area(plan$x, plan$y, i, j, k)
          }
        }
      }
    }
  }
  density/2*c('min_mu'=fmin, 'max_mu'=fmax)
}
