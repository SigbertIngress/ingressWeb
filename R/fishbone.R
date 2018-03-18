#' fishbone
#'
#' generates positions of \code{n} portals for a (distorted) fishbone web.
#'
#' @param n numeric: number of points to generate
#' @param d numeric: distortion of point coordinates (default: 0)
#'
#' @return data frame: with x-y coordinates for fish bone web portals
#' @export
#'
#' @examples
#' xy <- fishbone(6)
#' plot(xy)
#' #
#' xy <- fishbone(6, 0.5)
#' plot(xy)
fishbone <- function(n, d=0) {
  x <- c(0, 0, seq(1/(n-3), 1, by=1/(n-3))+d*runif(n-3), 2)
  y <- c(1, -1, d*(runif(n-3)-0.5), 0)
  data.frame(x=x-mean(x), y=y-mean(y))
}
