#' barycentric
#'
#' cmoputes barycentric coordinates for x-y coordinates related to x-y triangle coordinates
#'
#' @param x numeric(n): x coordinates
#' @param y numeric(n): y coordinates
#' @param xtri numeric(3): x coordinates of triangle vertices
#' @param ytri numeric(3): y coordinates of triangle vertices
#'
#' @details Barycentric are useful for different purposes:
#' \enumerate{
#'   \item If all barycentric coordinates are positive then the point is inside the triangle otherwise outside.
#'   \item If two coordinates are zero andd one coordinate is one then the point is one of the vertices of the triangle.
#'   \item For an interior point the barycentric coordinates give the proportion of the area if you subtringulate with the trinangle with this point.
#' }
#' @return numeric(n,3): barycentric coordinates for x-y coordinates
#' @export
#'
#' @examples
#' x <- random2(15)
#' barycentric(x[,1], x[,2], x[13:15,1], x[13:15,2])
barycentric <- function(x, y, xtri, ytri) {
  t      <- (ytri[2]-ytri[3])*(xtri[1]-xtri[3])+(xtri[3]-xtri[2])*(ytri[1]-ytri[3])
  lambda <- cbind((ytri[2]-ytri[3])*(x-xtri[3])+(xtri[3]-xtri[2])*(y-ytri[3]),
              (ytri[3]-ytri[1])*(x-xtri[3])+(xtri[1]-xtri[3])*(y-ytri[3]))/t
  cbind(lambda, 1-rowSums(lambda))
}