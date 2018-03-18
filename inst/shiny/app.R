library(shiny)
library(shinydashboard)
library(shinyFiles)
library(sp)
library(ingressWeb)

ui <- dashboardPage(
  dashboardHeader(title = "ingressWeb"),
  dashboardSidebar(
    tags$head(tags$style('.marginLeft{margin-left:15px;}')),
    sidebarMenu(
      menuItem("Load/create portals", startExpanded = TRUE,
               selectInput('portals', 'Web type',
                           c(File='File', Random='Random', Cobweb='Cobweb', Fishbone='Fishbone')
                           ),
               conditionalPanel(
                 condition = "input.portals == 'File'",
                 shinyFilesButton('files', 'File select', 'Please select a file', FALSE, class="marginLeft")
               ),
               conditionalPanel(
                 condition = "input.portals == 'Random'",
                 sliderInput('nrandom', 'Number of portals', min=5, max=50, value=9)
               ),
               conditionalPanel(
                 condition = "input.portals == 'Cobweb'",
                 sliderInput('ncobweb', 'Portals per leg', min=2, max=15, value=6),
                 sliderInput('dcobweb', 'Distortion', min=0, max=2, step=0.02, value=0)
               ),
               conditionalPanel(
                 condition = "input.portals == 'Fishbone'",
                 sliderInput('nfishbone', 'Number of portals', min=4, max=40, value=9),
                 sliderInput('dfishbone', 'Distortion', min=0, max=1, step=0.01, value=0)
               )
      ),
      menuItem("Create links & fields",
               selectInput('web', 'Link plan',
                           c(None="None", Maxarea='Maxarea', Fishbone='Fishbone', Fanfield='Fanfield', Delaunay='Delaunay',
                             Cobweb='Cobweb', Triangulation="Triangulation")
               ),
               uiOutput("uiPortalSpinner"),
               conditionalPanel(
                 condition = "input.web == 'Fanfield'",
                 checkboxInput('rfanfield', label=NULL)
               ),
               sliderInput('mu', 'MU density (in kMU/km^2)', min=0, max=4, step=0.01, value=2.1)
      )
    )
  ),
  dashboardBody(
    box(title = "Plot", status = "primary", plotOutput("plot"), width=12, solidHeader = TRUE),
    box(title = "Summary", status = "info", verbatimTextOutput("summary"), width=12,  solidHeader = TRUE)
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(web=NULL, plan=NULL, spinner = 1)

  shinyFileChoose(input, 'files', roots=c(wd='data'), filetypes=c('csv'))

  output$uiPortalSpinner <- renderUI ({
    list(
      div(style="display:inline-block;text-align: center;",
          actionButton("left", label='', icon = icon("triangle-left", lib = "glyphicon"), style='padding-left:4px;padding-right:4px;')),
      div(style="display:inline-block;width:118px;text-align: center;", textOutput('spinner')),
      div(style="display:inline-block;text-align: center;",
          actionButton("right", label='', icon = icon("triangle-right", lib = "glyphicon"), style='padding-left:4px;padding-right:4px;'))
      )
  })

  output$spinner <- renderText({
    portal <- ''
    if (!is.null(rv$web)) {
      portal <- if (is.null(rv$web$shortname)) rv$web$name[rv$spinner] else rv$web$shortname[rv$spinner]
    }
    portal
  })

  observeEvent(input$left, {
    if (!is.null(input$left))  {
      isolate(rv$spinner <- 1+((rv$spinner-2)%%length(rv$web$name)))
    }
  })

  observeEvent(input$right, {
    if (!is.null(input$right)) {
      isolate(rv$spinner <- 1+((rv$spinner)%%length(rv$web$name)))
    }
  })

  observe({
    if (!is.null(input$files)) {
   #   browser()
      filename  <- paste0('data/', input$files$files[[1]][[2]])
      portals   <- read.csv(filename, stringsAsFactors = FALSE)
      rv$web    <- ingressWeb(ll2xy(portals))
      rv$spinner <- 1
    }
  })

  observe({
    if ((input$portals=='Random') && !is.null(input$nrandom)) rv$web <- rv$plan <- ingressWeb(random2(input$nrandom)); rv$spinner <- 1
    if ((input$portals=='Cobweb') && !is.null(input$ncobweb)) rv$web <- rv$plan <- ingressWeb(cobweb(input$ncobweb, input$dcobweb)); rv$spinner <- 1
    if ((input$portals=='Fishbone') && !is.null(input$nfishbone)) rv$web <- rv$plan <- ingressWeb(fishbone(input$nfishbone, input$dfishbone)); rv$spinner <- 1
  })

  observe({
    if (input$web=='None')          rv$plan <- rv$web
    if (input$web=='Maxarea')       rv$plan <- make.links(rv$web, maxarea=rv$spinner)
    if (input$web=='Cobweb')        rv$plan <- make.links(rv$web, cobweb=rv$spinner)
    if (input$web=='Fanfield')      rv$plan <- make.links(rv$web, fanfield=if(rfanfield) -rv$spinner else rv$spinner)
    if (input$web=='Fishbone')      rv$plan <- make.links(rv$web, fishbone=rv$spinner)
    if (input$web=='Triangulation') rv$plan <- make.links(rv$web, triangle=rv$spinner)
    if (input$web=='Delaunay')      rv$plan <- make.links(rv$web)
  })

  output$plot <- renderPlot({
    if (!is.null(rv$plan)) {
      col             <- rep("black", length(rv$plan$name))
      col[rv$spinner] <- "red"
      plot(rv$plan, main=isolate(input$portals), col=col, axes=TRUE)
    }
  })

  output$summary <- renderPrint({
    if (!is.null(rv$plan)) {
      dens <- if (input$mu>0) input$mu/1000 else 1
      summary(rv$plan, dens)
    }
  })
}

shinyApp(ui, server)
