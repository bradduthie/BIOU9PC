# UI portion
# @param id can be set when the module is loaded in ui.R
standard_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  mean <- 32
  sd <- 10
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
        sliderInput(inputId=ns("mean"), label="Mean", width="100%", value=mean, min=-100, max = 100, animate = animationOptions(playButton = 'PLAY', pauseButton = 'PAUSE', interval = 100)),
        sliderInput(inputId=ns("sd"), label="Standard deviation", width="100%", value=sd, min=0, max = 100, animate = animationOptions(playButton = 'PLAY', pauseButton = 'PAUSE', interval = 100))
      ),
      div(class="col-xs-12 col-sm-10 col-lg-8",
        # output areas
        plotOutput(ns("Plot"))
      )
    )
  )
}
# Server function (output rendering)
standard <- function(input, output, session){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  standard.data <- reactive({
    validate(
      need(input$mean, "Mean required."),
      need(input$sd, "Standard deviation required.")
    )
    # cat(file=stderr(), "building levins.data \n")
    # run practical specific funtion using input data here
    standard_distribution(input$mean, input$sd)
  })
  # Render Reactive functions
  output$Plot <- renderPlot({
    data <- standard.data()
    Plot <-
      plotstyle()
      hist(data$randN, breaks = 100, probability = T, main = NULL, xlab = "Expected distribution of values, given mean and standard deviation")
      lines(data$xpos, data$density)
      abline(v= data$mean, col = 'red', lwd = 3)
      segments(data$mean-data$sd, max(data$density)/2, data$mean+data$sd, max(data$density)/2, col = 'red', lwd = 3)
  })
}