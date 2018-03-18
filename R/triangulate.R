#' triangulate
#'
#' Creates a tringulation of the convex hull
#'
#' @param plan ingressWeb: an ingers web object
#' @param base numeric: the number of the base portal for the triangulation
#' @param portals numeric:
#'
#' @return a set or triangles
#' @export
#'
#' @examples
#' # Read portal position from a Excel CSV file
#' path     <- system.file('shiny', 'data', package='ingressWeb')
#' filename <- paste(path, 'Leisepark-170212.csv', sep='/')
#' portals  <- read.csv(filename)
#' # convert latitude and longitude to x-y coordinates using the
#' # Universal Transverse Mercator coordinate system
#' xy     <- ll2xy(portals, utm=33)
#' web    <- ingressWeb(xy)
#' webtri <- make.links(web, triangle=33)
#' plot(webtri)
triangulate <- function (plan, base, portals=NULL) {
  p <- portals(plan)
  if (is.null(portals)) portals <- 1:p
  reverse <- (base<0)
  base    <- abs(base)
  if (!base  %in% portals) stop("Base portal is not portal list")
  index <- (1:p)[chull(plan$x[portals], plan$y[portals])]
  n  <- length(index)
  pb <- which(base==index)
  if (length(pb)) { # base portal is on convex hull
    index <- c(index[-1:-pb], index[-pb:-n])
    t      <- matrix(NA, ncol=3, nrow=n-2)
    for (j in 1:(n-2)) t[j,] <- c(base, index[j], index[j+1])
  } else {
    t      <- matrix(NA, ncol=3, nrow=n)
    t[n,] <- c(base, index[n], index[1])
    for (j in 1:(n-1)) t[j,] <- c(base, index[j], index[j+1])
  }
  if (reverse) t <- matrix(t[,c(1,3,2)], ncol=3)
  t
}
