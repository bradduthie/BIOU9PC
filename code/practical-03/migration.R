# source("helpers.R")
# UI portion
# @param id can be set when the module is loaded in ui.R
migration_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  N.patch <- 5
  # max.time <- 70
  m <- .1 # frequency of migration.
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
        sliderInput(inputId=ns("N.patch"), label="Number of patches", width="100%", value=N.patch, min=2, max = 10, step = 1),
        # numericInput(inputId=ns("max.time"), label="Max Time", width="100%", value=max.time),
        sliderInput(inputId=ns("m"), label="Frequency of migration", width="100%", value=m, min=0, max=1, step=.01, ticks=T)
      ),
      div(class="col-xs-12 col-sm-10 col-lg-8",
        plotOutput(ns("Plot"))  
      )
    )
  )
}

# Server function (output rendering)
migration <- function(input, output, session){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  migration.data <- reactive({
    validate(
      need(input$N.patch >= 2, "A number of patches > 1 is needed to assess migration between patches."),
      need(input$m, "Migration rate required.")
    )
    # cat(file=stderr(), "building migration.data \n")
    # run practical specific funtion using input data here
    migration_probability(input$N.patch, input$m)
  })
  # Render Reactive projected population dynamics against time
  output$Plot <- renderPlot({
    data <- migration.data()
    Plot <-
      plotstyle()
      par(mfrow = c(1, 1), pty = "m")
      image(x=1:data$N.patch, y=1:data$N.patch, 1-(data$M), xlab="from patch", ylab="to patch", axes=F, main="Probability of migration", ylim=c(2+data$N.patch,0), asp=1)
      axis(1, at = 1:data$N.patch)
      axis(2, at = 1:data$N.patch)
      text(x = rep(1:data$N.patch, each=data$N.patch), y=rep(1:data$N.patch, times=data$N.patch), sprintf("%1.2f", data$M))
      text(data$N.patch+1, 1:data$N.patch, sprintf("%1.2f", rowSums(data$M)-diag(data$M)), font=2)
      text(1:data$N.patch, data$N.patch+1, sprintf("%1.2f", colSums(data$M)-diag(data$M)), font=2)
  })
  output$Data <- renderTable({
    data <- migration.data()
    Data <- data$matrix
  },
  striped=TRUE,
  rownames=TRUE
  #digits=data$digits
  )
  output$CSV <- downloadHandler(
    filename = function() { "migration-probability.csv" },
    content = function(file) {
      data <- migration.data()
      write.table(data$file, file, sep=",", row.names=FALSE, col.names=TRUE)
    }
  )
}