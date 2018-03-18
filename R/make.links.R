#' make.links
#'
#' creates a linked web following some linking plan, e.g. cobweb, herring/fish bone, fanfield or maxfield.
#' Except for the maxfield (default) you need to provide further parameters, see Details.
#'

#'
#' @param plan ingressWeb: a ingressWeb with portal positions and no links
#' @param ... further parameter(s) to generate a linked web
#'
#' @details In the Ingress community exists a number of popular linking plans. The \href{https://www.youtube.com/watch?v=FkHtelZxgtg}{videos of Michael Hartley} are very helpful
#' for the herring bone or fish bone, the cobweb, the fanfield and for the \href{http://www.ingress-maxfield.com/}{maxfield}.
#' To realize these linking plans you neet to set further named parameters (see also \code{vignette("ingressWeb")}):
#' \describe{
#'   \item{cobweb}{provide either \code{cobweb=1}, \code{cobweb=2} or \code{cobweb=3} depending at which corner the cobweb should start.}
#'   \item{fishbone}{provide either \code{fishbone=1}, \code{fishbone=2} or \code{fishbone=3} depending at which corner the fish bone web should start.}
#'   \item{fanfield}{provide either \code{fanfield=1}, \code{fanfield=-1}, \code{fanfield=2}, \code{fanfield=-2}, \code{fanfield=3}, \code{fanfield=-3} depending at which corner the fish bone web should start. A negavtive value reverses the order.}
#'   \item{delaunay}{provide \code{delaunay=TRUE} for a \href{https://en.wikipedia.org/wiki/Delaunay_triangulation}{Delaunay} web. This web contains usually one multilayer field.}
#' }
#' @return a linked ingressWeb object
#' @import grDevices geometry
#' @export
#'
#' @examples
#' web <- ingressWeb(random2(15))
#' web <- make.links(web)
#' plot(web, main="Maxarea web")
#' # Fish/herring bone
#' web  <- ingressWeb(fishbone(8))
#' web8 <- make.links(web, fishbone=8)
#' plot(web8, main="Fishbone web")
#' # Cobweb
#' web <- ingressWeb(cobweb(6))
#' web1 <- make.links(web, cobweb=1)
#' plot(web1, main="Cobweb 1")
#' web2 <- make.links(web, cobweb=2)
#' plot(web2, main="Cobweb 2")
#' web3 <- make.links(web, cobweb=3)
#' plot(web3, main="Cobweb 3")
#' # Fanfield
#' web <- ingressWeb(random2(10))
#' web1 <- make.links(web, fanfield=1)
#' plot(web1, main="Fanfield 1")
#' web2 <- make.links(web, fanfield=-1)
#' plot(web2, main="Fanfield -1")
#' web3 <- make.links(web, fanfield=2)
#' plot(web3, main="Fanfield 2")
#' # Delaunay triangulation
#' web <- ingressWeb(random2(10))
#' web1 <- make.links(web, delaunay=TRUE)
make.links <- function(plan, ...) {
  subtriangulate <- function (plan, tri, depth, ...) {
    args     <- list(...)
    plan$link[tri, tri] <- 1
    bary     <- barycentric(plan$x, plan$y, plan$x[tri], plan$y[tri])
    interior <- (rowSums(bary>0)==3)
    if(sum(interior)==0) return(plan)
    noweb   <- TRUE
    # if any interior points available
    if (!is.null(args$cobweb)) {
        noweb <- FALSE
        k     <- 1+(depth+(0:2))%%3
        rc    <- apply(interior*bary, 2, which.max)
        t     <- tri; t[k[1]] <- rc[k[1]]
        plan  <- subtriangulate(plan, t, depth+1, ...)
        t     <- tri; t[k[2]] <- rc[k[1]]
        plan  <- subtriangulate(plan, t, depth+1, ...)
        t     <- tri; t[k[3]] <- rc[k[1]]
        plan  <- subtriangulate(plan, t, depth+1, ...)
      }
    if (!is.null(args$fishbone)) {
      if (!noweb) stop("Can not make two or more webs at once")
      noweb   <- FALSE
      maxcoor <- apply(interior*bary, 2, which.max)
      t <- c(maxcoor[1], tri[2], tri[3])
      plan <- subtriangulate(plan, t, depth+1, ...)
      t <- c(maxcoor[1], tri[1], tri[2])
      plan <- subtriangulate(plan, t, depth+1, ...)
      t <- c(maxcoor[1], tri[1], tri[3])
      plan <- subtriangulate(plan, t, depth+1, ...)
    }
    if (!is.null(args$fanfield)) {
      noweb <- FALSE
      index <- c(tri[2], (1:length(plan$x))[interior], tri[3])
      ni <- length(index)
      x  <- plan$x[index]-plan$x[tri[1]]
      y  <- plan$y[index]-plan$y[tri[1]]
      a  <- atan2(y, x)
      if (diff(range(a))>pi) a <- (a>0)*(a-pi)+(a<0)*(a+pi)
      if (diff(range(a))>pi) stop("polar angles are larger than pi")
      pindex <- index[order(a)]
      if (pindex[1]!=tri[2]) pindex <- rev(pindex)
      plan$link[tri[1],pindex] <- plan$link[pindex,tri[1]] <- 1
      for (i in 2:ni) {
        for (j in 1:(i-1)) {
          if (checkLink(plan, pindex[i], pindex[j]))
            plan$link[pindex[i], pindex[j]] <-  plan$link[pindex[j], pindex[i]] <- 1
        }
      }
    }
    if (noweb) {
      mm <- interior*bary
      rc <- which(mm == max(mm), arr.ind = TRUE)
      t <- c(tri[1], tri[2], rc[1])
      plan <- subtriangulate(plan, t, depth+1, ...)
      t <- c(tri[1], rc[1], tri[3])
      plan <- subtriangulate(plan, t, depth+1, ...)
      t <- c(rc[1], tri[2], tri[3])
      plan <- subtriangulate(plan, t, depth+1, ...)
    }
    return(plan)
  }
  # start of make.links
  if (!('ingressWeb' %in% class(plan))) stop('First parameter is not a ingressWeb')
  args <- list(...)
  index   <- chull(plan$x, plan$y)
  p       <- portals(plan)
  n       <- length(index)
  noweb <- TRUE
  if (!is.null(args$cobweb)) {
    if (!noweb) stop("Can not make two or more webs at once")
    if (!(args$cobweb %in% 1:p)) stop("cobweb parameter value is invalid")
    noweb  <- FALSE
    tri   <- triangulate(plan, args$cobweb)
    for (i in 1:nrow(tri)) plan <- subtriangulate(plan, tri[i,], 0, ...)
  }
  if (!is.null(args$fishbone)) {
    if (!noweb) stop("Can not make two or more webs at once")
    if (!(args$fishbone %in% 1:p)) stop("fishbone parameter value is invalid")
    noweb  <- FALSE
    tri   <- triangulate(plan, args$fishbone)
    for (i in 1:nrow(tri)) plan <- subtriangulate(plan, tri[i,], 0, ...)
  }
  if (!is.null(args$fanfield)) {
    if (!noweb) stop("Can not make two or more webs at once")
    if (!(args$fanfield %in% c(-p:-1,1:p))) stop("fanfield parameter value is invalid")
    noweb <- FALSE
    tri   <- triangulate(plan, args$fanfield)
    for (i in 1:nrow(tri)) plan <- subtriangulate(plan, tri[i,], 0, ...)
  }
  if (!is.null(args$triangle)) {
    if (!noweb) stop("Can not make two or more webs at once")
    noweb  <- FALSE
    tri <- triangulate(plan, args$triangle)
    for (i in 1:nrow(tri)) plan$link[tri[i,], tri[i,]] <- 1
  }
  if (!is.null(args$maxarea)) {
    if (!noweb) stop("Can not make two or more webs at once")
    noweb  <- FALSE
    tri <- triangulate(plan, args$maxarea)
    for (i in 1:nrow(tri)) plan <- subtriangulate(plan, tri[i,], 0, ...)
  }
  if (noweb) {
    tri    <- delaunayn(cbind(plan$x, plan$y))
    for (i in 1:nrow(tri)) plan$link[tri[i,], tri[i,]] <- 1
  }
  return(plan)
}
