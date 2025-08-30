# UI portion
# @param id can be set when the module is loaded in ui.R
binomial_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  size <- 10 # Number of trials. 
  p <- 0.07 # Probability of success (ie, probability of death)
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
        sliderInput(inputId=ns("p"), label="Probability of success (ie, per-capita probability of mortality)", width="100%", value=p, min=0.01, max=1, step=.01, ticks=FALSE, animate = animationOptions(playButton = 'PLAY', pauseButton = 'PAUSE', interval = 100)),
        numericInput(inputId=ns("size"), label="Number of trials (ie, number of individuals)", width="100%", value=size, min=1, max=NA)
      ),
      div(class="col-xs-12 col-sm-10 col-lg-8",
        # output areas
        plotOutput(ns("Plot"))
      )
    )
  )
}
# Server function (output rendering)
binomial <- function(input, output, session){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  binomial.data <- reactive({
    validate(
      need(input$size, "Number of trials required."),
      need(input$size <= 1000, "Number of trials cannot exceed 1000"),
      need(input$p, "Probability of success required.")
    )
    # cat(file=stderr(), "building levins.data \n")
    # run practical specific funtion using input data here
    binomial_distribution(input$size, input$p)
  })
  # Render Reactive functions
  output$Plot <- renderPlot({
    data <- binomial.data()
    Plot <-
      plotstyle()
      hist(data$randB, breaks = (0:(data$size+1))-0.5, freq = F, main = NULL, xlab = 'Expected number of events')
    #  lines(data$xpos, data$density, type = "h", lwd = 6) 
      abline(v= data$p*data$size, col = 'red', lwd = 3)
  })
}