#' fields
#'
#' Returns the miminal (no multilayers) and maximal (all possible multilayers) number of the fields of a web.
#' Depending how you are link you may create or not create multilayers.
#'
#' @param plan ingressWeb: ingress web object
#'
#' @return integer(2): minimum and maximum number of fields
#' @export
#'
#' @examples
#' web  <- ingressWeb(random2(10))
#' fields(web)
#' web1 <- make.links(web)
#' fields(web1)
#' #
#' web  <- ingressWeb(fishbone(9))
#' web1 <- make.links(web, fishbone=3)
#' fields(web1)
fields <- function(plan) {
  if (!'ingressWeb' %in% class(plan)) stop('ingressWeb object required')
  n  <- nrow(plan$link)
  fmin <- fmax <- 0
  for (i in 1:(n-2)) {
    for (j in (i+1):(n-1)) {
      if (plan$link[i,j]) {
        for (k in (j+1):n) {
          if (plan$link[i,k] && plan$link[j,k]) {
            bc   <- (rowSums(barycentric(plan$x[-c(i,j,k)], plan$y[-c(i,j,k)], plan$x[c(i,j,k)], plan$y[c(i,j,k)])>0)==3)
            fmax <- fmax+1
            if (!sum(bc)) fmin <- fmin+1
          }
        }
      }
    }
  }
  c('min_field'=fmin, 'max_field'=fmax)
}
