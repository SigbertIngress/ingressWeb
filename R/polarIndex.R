#' polarIndex
#'
#' Under the assumption that i is the index of the outer triangle of the x-y coordinates, it computes the order of the datapoint
#' with decreasing/increasing order by the (polar) angles in relation to point i. A negative value of i reverses the order
#'
#' @param x numeric(n): x coordinates
#' @param y numeric(n): y coordinates
#' @param i numeric: index of observation
#'
#' @return index or ordered points
#' @export
#'
#' @examples
#' x <- random2(15)
#' plot(x, asp=TRUE)
#' polarIndex(x[,1], x[,2], 15)
#' polarIndex(x[,1], x[,2], -15)
polarIndex <- function(x, y, i) {
  k <- abs(i)
  x <- x-x[k]
  y <- y-y[k]
  r <- sqrt(x^2+y^2)
  a <- atan2(y, x)
  if (diff(range(a))>pi) a <- (a>0)*(a-pi)+(a<0)*(a+pi)
  if (diff(range(a))>pi) stop("polar angles are larger than pi")
  o <- order(a)
  b <- which.min(r)
  o <- o[o!=b]
  return(c(b, if(i>0) o else rev(o)))
}
