## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.height = 7, fig.width = 7)

## ------------------------------------------------------------------------
library(ingressWeb)
# Read portal position from a Excel CSV file
path     <- system.file('shiny', 'data', package='ingressWeb')
filename <- paste(path, 'Leisepark-170212.csv', sep='/')
portals  <- read.csv(filename, stringsAsFactors = FALSE)
# convert latitude and longitude to x-y coordinates using the 
# Universal Transverse Mercator coordinate system 
xy       <- ll2xy(portals, utm=33)
web      <- ingressWeb(xy)
plot(web)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(random2(10))
plot(web)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(random2(10, random=runif))
par(mfrow=c(1,2))
plot(web)
plot(web, text=TRUE, col="white") # show portal index rather than points

## ------------------------------------------------------------------------
library(ingressWeb)
web <- ingressWeb(cobweb(6))
plot(web)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(cobweb(6, d=0.2))
plot(web)

## ------------------------------------------------------------------------
library(ingressWeb)
web <- ingressWeb(fishbone(9))
plot(web)

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(fishbone(9, d=0.2))
plot(web)

## ----message=FALSE-------------------------------------------------------
set.seed(0)
library(ingressWeb)
web  <- ingressWeb(random2(10))
webd <- make.links(web)
plot(webd, main="Delaunay", pos='topright')

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(random2(10))
fullweb <- make.links(web, maxarea=10)
plot(fullweb)

## ------------------------------------------------------------------------
library(ingressWeb)
web <- ingressWeb(fishbone(9))
web9 <- make.links(web, fishbone=9)
par(mfrow=c(2,1))
plot(web, text=TRUE, col="white")
plot(web9, main="Fish bone 9")

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(random2(10))
web1 <- make.links(web, fanfield=10)
web2 <- make.links(web, fanfield=8)
web3 <- make.links(web, fanfield=-8)
par(mfrow=c(2,2))
plot(web, main="Web", text=TRUE, col="white")
plot(web1, main="Fanfield 10")
plot(web2, main="Fanfield 8")
plot(web3, main="Fanfield -8")

## ------------------------------------------------------------------------
library(ingressWeb)
web <- ingressWeb(cobweb(6))
web1 <- make.links(web, cobweb=6)
web2 <- make.links(web, cobweb=12)
web3 <- make.links(web, cobweb=18)
par(mfrow=c(2,2))
plot(web, text=TRUE, col=0)
plot(web1, main="Cobweb 1")
plot(web2, main="Cobweb 2")
plot(web3, main="Cobweb 3")

## ------------------------------------------------------------------------
set.seed(0)
library(ingressWeb)
web <- ingressWeb(cobweb(6, 0.3))
web1 <- make.links(web, cobweb=6)
web2 <- make.links(web, cobweb=12)
web3 <- make.links(web, cobweb=18)
par(mfrow=c(2,2))
plot(web, text=TRUE, col=0)
plot(web1, main="Cobweb 1")
plot(web2, main="Cobweb 2")
plot(web3, main="Cobweb 3")

## ------------------------------------------------------------------------
library(ingressWeb)
# Read portal position from a Excel CSV file
path     <- system.file('shiny', 'data', package='ingressWeb')
filename <- paste(path, 'Leisepark-170212.csv', sep='/')
portals  <- read.csv(filename)
# convert latitude and longitude to x-y coordinates using the 
# Universal Transverse Mercator coordinate system 
xy       <- ll2xy(portals, utm=33)
web      <- ingressWeb(xy)
plot(web, text=TRUE, col="white")

## ------------------------------------------------------------------------
library(ingressWeb)
# Read portal position from a Excel CSV file
path     <- system.file('shiny', 'data', package='ingressWeb')
filename <- paste(path, 'Leisepark-170212.csv', sep='/')
portals  <- read.csv(filename)
# convert latitude and longitude to x-y coordinates using the 
# Universal Transverse Mercator coordinate system 
xy     <- ll2xy(portals, utm=33)
web    <- ingressWeb(xy)
webtri <- make.links(web, triangle=33) 
plot(webtri)

## ------------------------------------------------------------------------
library(ingressWeb)
# Read portal position from a Excel CSV file
path     <- system.file('shiny', 'data', package='ingressWeb')
filename <- paste(path, 'Leisepark-170212.csv', sep='/')
portals  <- read.csv(filename)
# convert latitude and longitude to x-y coordinates using the 
# Universal Transverse Mercator coordinate system 
xy   <- ll2xy(portals, utm=33)
web  <- ingressWeb(xy)
webd <- make.links(web) 
plot(webd, pos="topright")

## ------------------------------------------------------------------------
library(ingressWeb)
# Read portal position from a Excel CSV file
path     <- system.file('shiny', 'data', package='ingressWeb')
filename <- paste(path, 'Leisepark-170212.csv', sep='/')
portals  <- read.csv(filename)
# convert latitude and longitude to x-y coordinates using the 
# Universal Transverse Mercator coordinate system 
xy   <- ll2xy(portals, utm=33)
web  <- ingressWeb(xy)
webb <- make.links(web, fishbone=33) 
webf <- make.links(web, fanfield=33) 
webm <- make.links(web, maxarea=33) 
par(mfrow=c(2,2))
plot(web, main="No links")
plot(webb, main="Fish bone")
plot(webf, main="Fanfield")
plot(webm, main="Maxarea")
# In Berlin you have approx. 0.021 MU/m^2 field
summary(webb, density=0.021)
summary(webf, density=0.021)
summary(webm, density=0.021)

## ------------------------------------------------------------------------
# Read portal position from a Excel CSV file
path     <- system.file('shiny', 'data', package='ingressWeb')
filename <- paste(path, 'Leisepark-170212.csv', sep='/')
portals  <- read.csv(filename)
# convert latitude and longitude to x-y coordinates using the 
# Universal Transverse Mercator coordinate system 
xy   <- ll2xy(portals, utm=33)
web  <- ingressWeb(xy)
webc <- make.links(web, cobweb=33) 
plot(webc)
# In Berlin you have approx. 0.021 MU/m^2 field
summary(webc, density=0.021)

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
web2 <- make.links(web, maxarea=10)
portals(web2)
links(web2)          # total number of links
l<-links(web2, TRUE) # number of links per portal
table(l)
fields(web2)         # with multilayers
mindunits(web2)      # not rounded up
accesspoints(web2)   # 

## ----eval=FALSE----------------------------------------------------------
#  install.packages(c('shiny', 'shinydashboard', 'shinyFiles'))
#  run

## ----eval=FALSE----------------------------------------------------------
#  library(ingressWeb)
#  shinyWeb()

## ----eval=FALSE----------------------------------------------------------
#   system.file("shiny", "data", package = "ingressWeb")

