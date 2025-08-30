# source("helpers.R")
# UI portion
# @param id can be set when the module is loaded in ui.R
levins_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  c <- 0.4 # colonisation rate
  e <- 0.1 # extiniction rate
  Prop <- 1:100/100 # proportion of occupied patches
  # put your ui elements for inputs and output inside the content section
  tags$section(class=ns("content"),
               div(class="row justify-content-center",
                   div(class="col-xs-12 col-sm-10 col-lg-4",
                       if(!is.null(h2)){
                         div(class="row justify-content-center",
                             div(class="col-xs-12 col-sm-10 col-lg-12",
                                 h2(class=ns("title"), h2)
                             )
                         )
                       },
                       if(!is.null(intro)){
                         div(class="row justify-content-center justify-content-lg-start",
                             div(class="col-xs-12",
                                 div(class="alert alert-info", role="alert", intro)
                             )
                         ) 
                       },
                       # Inputs
                       sliderInput(inputId=ns("c"), label="Per-patch probability of colonization (c)", width="100%", value=c, min=.01, max=3, step=.01, ticks=T, animate = animationOptions(interval = 100, loop = T, playButton = "PLAY", pauseButton = "PAUSE")),
                       sliderInput(inputId=ns("e"), label="Per-patch probability of extinction (e)", width="100%", value=e, min=.01, max=1, step=.01, ticks=T, animate = animationOptions(interval = 100, loop = T, playButton = "PLAY", pauseButton = "PAUSE"))
                   ),
                   div(class="col-xs-12 col-sm-10 col-lg-8",
                       # output areas
                       tabsetPanel(type="tabs", id="output",
                                   tabPanel("Plots",
                                            plotOutput(ns("Plot"))
                                   ),
                                   tabPanel("Data", class="text-center",
                                            downloadButton(outputId=ns("CSV"), label="Download CSV"),
                                            tableOutput(ns("Data"))
                                   )
                       )
                   )
               )
  )
}
# Server function (output rendering)
levins <- function(input, output, session){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  levins.data <- reactive({
    validate(
      need(input$c, "Colonisation rate required"),
      need(input$e, "Extinction rate required")
    )
    # cat(file=stderr(), "building levins.data \n")
    # run practical specific funtion using input data here
    levins_model(input$c, input$e)
  })
  # Render Reactive projected population dynamics against time
  output$Plot <- renderPlot({
    data <- levins.data()
    Plot <-
      plotstyle()
    par(mar = c(5, 5, 1, 5))
    plot(data$Prop, data$cProp, xlab="Fraction of sites occupied (P)", ylab="", col=brand.primary, type="l", xlim=range(0, 1), lwd = 2)
    mtext("Colonization rate (c*P*(1-P))", 2, col=brand.primary, line=3, las=3)
    axis(4)
    mtext("Extinction rate (e*P)", 4, col=brand.tertiary, line=3, las=3)
    lines(data$Prop, data$eProp, col=brand.tertiary, lty = 2, lwd = 2)
    if(data$Pstar>0){
      abline(v=data$Pstar, col=brand.secondary, lty=3, lwd = 2)
      mtext("Pstar (1-e/c)", 3, col=brand.secondary, at=data$Pstar)
    }
  })
  output$Data <- renderTable({
    data <- levins.data()
    Data <- data$matrix
  },
  striped=TRUE,
  rownames=FALSE
  #digits=data$digits
  )
  output$CSV <- downloadHandler(
    filename = function() { "levins-meta-populations.csv" },
    content = function(file) {
      data <- levins.data()
      write.table(data$matrix, file, sep=",", row.names=FALSE)
    }
  )
}