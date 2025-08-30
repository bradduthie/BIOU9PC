# UI portion
# @param id can be set when the module is loaded in ui.R
poisson_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  lambda <- 2.4
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
        sliderInput(inputId=ns("lambda"), label="Mean rate (lambda)", width="100%", value=lambda, min=0.1, max=10, step=.01, ticks=FALSE, animate = animationOptions(playButton = 'PLAY', pauseButton = 'PAUSE', interval = 100))
      ),
      div(class="col-xs-12 col-sm-10 col-lg-8",
        # output areas
        plotOutput(ns("Plot"))
      )
    )
  )
}
# Server function (output rendering)
poisson <- function(input, output, session){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  poisson.data <- reactive({
    validate(
      need(input$lambda, "Mean rate required.")
    )
    # cat(file=stderr(), "building levins.data \n")
    # run practical specific funtion using input data here
    poisson_distribution(input$lambda)
  })
  # Render Reactive functions
  output$Plot <- renderPlot({
    data <- poisson.data()
    Plot <-
        plotstyle()
        hist(data$randP, breaks = (0:30)-0.5, main = NULL, xlab = 'Expected distribution of values, given mean rate', freq = F)
#        points(data$xpos, data$density, cex = 2) 
        abline(v= data$lambda, col = 'red', lwd = 3)
  })
}