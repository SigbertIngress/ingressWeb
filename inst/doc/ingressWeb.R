## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.height = 7, fig.width = 7)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(random2(10))
plot(web)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(random2(10, random=runif))
plot(web)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(cobweb(6))
plot(web)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(cobweb(6, d=0.2))
plot(web)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(fishbone(9))
plot(web)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(fishbone(9, d=0.2))
plot(web)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(random2(10))
fullweb <- make.links(web)
plot(fullweb)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(cobweb(6))
web1 <- make.links(web, cobweb=1)
web2 <- make.links(web, cobweb=2)
web3 <- make.links(web, cobweb=3)
par(mfrow=c(2,2))
plot(web)
plot(web1, main="Cobweb 1")
plot(web2, main="Cobweb 2")
plot(web3, main="Cobweb 3")


## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(fishbone(9))
web1 <- make.links(web, fishbone=1)
web2 <- make.links(web, fishbone=2)
web3 <- make.links(web, fishbone=3)
par(mfrow=c(2,2))
plot(web)
plot(web1, main="Fish bone 1")
plot(web2, main="Fish bone 2")
plot(web3, main="Fish bone 3")

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(random2(10))
web1 <- make.links(web, fanfield=1)
web2 <- make.links(web, fanfield=-1)
web3 <- make.links(web, fanfield=2)
web4 <- make.links(web, fanfield=3)
par(mfrow=c(2,2))
plot(web1, main="Fanfield 1")
plot(web2, main="Fanfield -1")
plot(web3, main="Fanfield 2")
plot(web4, main="Fanfield 3")

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(random2(10))
web1 <- make.links(web, delaunay=TRUE)
plot(web1, main="Delaunay")

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(random2(10))
summary(web)
web2 <- make.links(web)
summary(web2)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web  <- ingressWeb(random2(10))
web2 <- make.links(web)
portals(web2)
links(web2)          # total number of links
l<-links(web2, TRUE) # number of links per portal
table(l)
fields(web2)         # with multilayers
mindunits(web2)      # not rounded up

