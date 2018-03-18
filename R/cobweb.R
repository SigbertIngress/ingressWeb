#' cobweb
#'
#' generates positions of \code{3*n} portals for a (distorted) cobweb.
#'
#' @param n numeric: number of points for one leg
#' @param d numeric: distortion of point coordinates (default: 0)
#'
#' @return data frame: with x-y coordinates for cobweb portals
#' @export
#'
#' @examples
#' xy <- cobweb(6)
#' plot(xy)
#' #
#' xy <- cobweb(6, 0.3)
#' plot(xy)
cobweb <- function(n, d=0) {
  r <- rep(1:n, 3)
  a <- c(rep(0, n), rep(2/3*pi, n), rep(4/3*pi,n))
  data.frame(x=r*cos(a)+d*(2*runif(3*n)-1), y=r*sin(a)+d*(2*runif(3*n)-1))
}
