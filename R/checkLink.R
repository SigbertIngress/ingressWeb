#' checkLink
#'
#' Checks whether in given portal/link structure it is possible to make a link from portal 'f' to portal 't'
#'
#' @param g ingessWeb: ingressWeb
#' @param f integer: link source
#' @param t integer: link destination
#'
#' @return boolean value
#' @export
#'
#' @examples
#' web <- ingressWeb(random2(10)) # empty web
#' checkLink(web, 1, 2)           # should return TRUE
checkLink <-function (g, f, t) {
  intersect <- function (p0x, p0y, p1x, p1y, p2x, p2y, p3x, p3y) {
    # http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
    s1x <- p1x-p0x
    s1y <- p1y-p0y
    s2x <- p3x - p2x
    s2y <- p3y - p2y
    d   <- (-s2x * s1y + s1x * s2y)
    if (isTRUE(all.equal(d,0))) return(FALSE) # line segments are parallel
    s <- (-s1y * (p0x - p2x) + s1x * (p0y - p2y)) / d;
    t <- ( s2x * (p0y - p2y) - s2y * (p0x - p2x)) / d;
    if ((isTRUE(all.equal(s,0))||isTRUE(all.equal(s,1))) &&
        (isTRUE(all.equal(t,0))||isTRUE(all.equal(t,1)))) return(FALSE)
    if ((s>0) && (s<1) && (t>0) && (t<1)) return(TRUE)
    return(FALSE)
  }
  #
  if (g$link[f,t]) return(FALSE) # link exists
  # print(c(f,t, link[f,t]))
  n <- length(g$x)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if ((g$link[i,j]>0) && intersect(g$x[f], g$y[f], g$x[t], g$y[t], g$x[i], g$y[i], g$x[j], g$y[j])) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}
