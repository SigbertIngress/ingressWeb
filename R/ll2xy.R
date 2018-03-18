#' ll2xy
#'
#' Computes from latitudes and longitudes to x-y coordinates based on the
#' \href{UTM projection}{https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system}.
#' The data frame must contain two columns giving the longitude and latitude of the portals. The following columns will be used:
#' \code{name} (portal name), \code{lat}/\code{latE6} (latitude) and \code{lon}/\code{lng}/\code{lonE6}/\code{lngE6} (longitude).
#' If \code{utm} is not set explicitly then the UTM zone is the zone where majority of the portals is located.
#'
#' @param portals data frame: input data
#' @param utm integer: utm zone to convert the latitude and longitude (default: NULL)
#'
#' @return data frame: with five variables: \code{name} (portal name), \code{x} (x coordinate of portal), \code{y} (y coordinate of portal),
#' \code{lat} (latitude of portal) and \code{lng} (longitude of portal)
#' @import utils sp
#' @export
#'
#' @examples
#' path     <- system.file('shiny', 'data', package='ingressWeb')
#' filename <- paste(path, 'Leisepark-170212.csv', sep='/')
#' portals  <- read.csv(filename)
#' xy       <- ll2xy(portals)
#' web      <- ingressWeb(xy)
#' plot(web)
ll2xy <- function(portals, utm=NULL) {
  if (is.numeric(portals$lonE6)) lng <- portals$lonE6/1e6
  if (is.numeric(portals$lngE6)) lng <- portals$lngE6/1e6
  if (is.numeric(portals$lon))   lng <- portals$lon
  if (is.numeric(portals$lng))   lng <- portals$lng
  if (is.numeric(portals$latE6)) lat <- portals$latE6/1e6
  if (is.numeric(portals$lat))   lat <- portals$lat
  xy <- data.frame(lng=lng, lat=lat)
  coordinates(xy) <- c('lng', 'lat')
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  if (is.null(utm)) {
    utm <- 1+((xy$lng+180)%%360)%/%6
    utm <- as.numeric(names(which.max(table(utm))))
  }
  xy          <- spTransform(xy, CRS(sprintf("+proj=utm +zone=%.0f ellps=WGS84", utm)))
  portals$lng <- lng
  portals$lat <- lat
  portals$x   <- xy@coords[,1]
  portals$y   <- xy@coords[,2]
  portals
}
