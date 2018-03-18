#' random2
#'
#' generates random positions of \code{n} portals for a web.
#'
#' @param n numeric: number of points for one leg
#' @param d numeric: distance for outer triangle (default: 1.05)
#' @param random function: univariate random number generator (default: \code{\link{rnorm}})
#' @param ... further parameters for \code{random}
#'
#' @return x-y coordinates portals for randomly generated web.
#' @import stats
#' @export
#'
#' @examples
#' xy <- random2(20)
#' plot(xy, asp=TRUE)
#' #
#' xy <- random2(20, random="runif")
#' plot(xy, asp=TRUE)
random2 <- function(n, d=1.05, random="rnorm", ...) {
  args   <- list(...)
  args$n <- n-3
  x  <- do.call(random, args)
  y  <- do.call(random, args)
  x  <- x-mean(x)
  y  <- y-mean(y)
  r  <- 2*d*sqrt(max(x^2+y^2))
  a  <- (1:3)*2*pi/3
  data.frame(x=c(x,r*cos(a)), y=c(y, r*sin(a)))
}
