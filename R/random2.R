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
#' xy <- random2(6)
#' plot(xy)
#' #
#' xy <- random2(6, random="runif")
#' plot(xy)
random2 <- function(n, d=1.05, random="rnorm", ...) {
  args   <- list(...)
  args$n <- n-3
  x  <- do.call(random, args)
  y  <- do.call(random, args)
  xy <- cbind(x-mean(x), y-mean(y))
  r  <- 2*d*sqrt(max(rowSums(xy^2)))
  a  <- (1:3)*2*pi/3
  rbind(xy, r*cbind(cos(a), sin(a)))
}
