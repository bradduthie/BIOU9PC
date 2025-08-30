# UI portion
# @param id can be set when the module is loaded in ui.R
projection_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  N.patch  <- 5
  max.time <- 70
  m        <- 0  # frequency of migration.
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
        sliderInput(inputId=ns("r"), label="Intrinsic growth rate (r)", width="100%", value=0.05, min= -0.1, max=0.1, ticks=T),
        sliderInput(inputId=ns("r_sd"), label="Variation in intrinsic growth rate (r)", width="100%", value=0, min=0, max=0.05, ticks=T),
        sliderInput(inputId=ns("K"), label="Carrying capacity (K)", width="100%", value=300, min=0, max=1000, ticks=T),
        sliderInput(inputId=ns("K_sd"), label="Variation in carrying capacity", width="100%", value=0, min=0, max=1000, ticks=T),
        sliderInput(inputId=ns("max.time"), label="Max Time", width="100%", value=max.time, min = 10, max = 1000),
        sliderInput(inputId=ns("m"), label="Frequency of migration", width="100%", value=m, min=0, max=0.5, step = 0.01, ticks=T)
      ),
      div(class="col-xs-12 col-sm-10 col-lg-8",
        # output areas
          tabsetPanel(type="tabs", id="output",
            tabPanel("Plots",
              h3("Sub-population trajectories"),
              plotOutput(ns("Plot.trajectories")),
              h3("Sub-population dynamics on one set of axes"),
              plotOutput(ns("Plot.combined")),
              h3("Number of emigrants"),
              plotOutput(ns("Plot.emigrants")),
              h3("Number of immigrants"),
              plotOutput(ns("Plot.immigrants")),
              h3("Net migration"),
              plotOutput(ns("Plot.migration"))
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
projection <- function(input, output, session){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  projection.data <- reactive({
    validate(
      need(input$N.patch > 1, "A number of patches > 1 is needed for migration to occur."),
      need(input$max.time > 1, "Max time must be > 1 to project into the future."),
      need(input$m, "Migration rate required.")
    )
    # cat(file=stderr(), "building migration.data \n")
    # run practical specific funtion using input data here
    patch_projection(input$N.patch, input$max.time, input$m, input$r, input$r_sd, input$K, input$K_sd)
  })
  # Render Reactive projected population dynamics against time
  output$Plot.trajectories <- renderPlot({
    data <- projection.data()
    Plot.trajectories <-
      plotstyle()
      par(mfrow = c(1, data$N.patch), mar = c(5, 1, 0, 0), oma = c(0, 4, 0, 0), xpd = NA)
      for(i in 1:data$N.patch){
        # Set up the base graphic
        plot(1:data$max.time, data$N[i,], col = i, xlab = "", ylab = "", yaxt = "n", ylim = c(min(data$N, data$K)*0.9, max(data$N, data$K)*1.1), xlim = c(0, data$max.time), type = 'l') 
        ifelse(i == 1, axis(2), axis(2, labels = NA))
        # Annotate the figure
        mtext(sprintf("%1.0f\nr = %1.3f\nK = %1.0f", i, data$r[i], data$K[i]), font = 1, cex = 1.2, line = -6, adj = 0.05) 
        segments(1, data$K[i], data$max.time, data$K[i], lty = 2, col = i)
        abline(v = data$max.time/2, col = 'gray', lty = 3)
        # if sub-population extinction occurs, this marks it on the figure
        if(any(data$N[i,] < 1)){points(which(data$N[i,] < 1)[1], 0, col = 'red', pch = 1, cex = 5, lwd = 2)}
      }
      title(ylab = "Sub-population size", outer = T, cex.lab= 2, line = 2)
      title(xlab = "Timesteps", outer = T, cex.lab= 2, line = -2)
  })
  output$Plot.combined <- renderPlot({
    data <- projection.data()
    Plot.combined <- 
      plotstyle()
      par(mfrow = c(1, 1))
      matplot(x = 1:data$max.time, y = t(data$N), type = "l", lty = 1, ylab = "Sub-population size", xlab = "Timestep")
      abline(h = data$K, lty = 2, col = 1:data$N.patch, lwd = 1)
      abline(v = data$max.time/2, col = 'gray', lty = 3)
      # if sub-population extinction occurs, this marks it on the figure
      points(apply(data$N, 1, function(x){ifelse(any(x<1), which(x < 1)[1], NA)}), rep(0, data$N.patch), col = 1:data$N.patch, pch = 1, lwd = 2, cex = 5)
      legend("topright", col = 1:data$N.patch, lwd = 2, legend = 1:data$N.patch, title = "Sub-population", bty = 'n', inset = 0.1)
  })
  output$Plot.emigrants <- renderPlot({
      data <- projection.data()
      Plot.emigrants <- 
        plotstyle()
        matplot(x = 1:data$max.time, y = t(data$emigrants), type = "l", lty = 1, ylab = "N. emigrants", xlim = c(0, data$max.time), xlab = "Timestep")
        legend("left", col = 1:data$N.patch, lwd = 2, legend = 1:data$N.patch, title = "Sub-population", bty = 'n', inset = 0.1)
  })
  output$Plot.immigrants <- renderPlot({
      data <- projection.data()
      Plot.immigrants <- 
        plotstyle()
        matplot(x = 1:data$max.time, y = t(data$immigrants), type = "l", lty = 1, ylab = "N. immigrants", xlim = c(0, data$max.time),xlab = "Timestep")
        legend("left", col = 1:data$N.patch, lwd = 2, legend = 1:data$N.patch, title = "Sub-population", bty = 'n', inset = 0.1)
  })
  output$Plot.migration <- renderPlot({
      data <- projection.data()
      Plot.migration <- 
        plotstyle()
        matplot(x = 1:data$max.time, y = t(data$emigrants) - t(data$immigrants), type = "l", lty = 1, ylab = "Net migration gain (or loss)", xlim = c(0, data$max.time),xlab = "Timestep")
        abline(h = 0, col = 'gray', lty = 3)
        legend("left", col = 1:data$N.patch, lwd = 2, legend = 1:data$N.patch, title = "Sub-population", bty = 'n', inset = 0.1)
  })
  output$Data <- renderTable({
    data <- projection.data()
    Data <- t(data$matrix)
  },
  striped=TRUE,
  rownames=FALSE,
  colnames = F
  #digits=data$digits
  )
  output$CSV <- downloadHandler(
    filename = function() { "patch-population-projection.csv" },
    content = function(file) {
      data <- projection.data()
      write.table(t(data$matrix), file, sep=",", row.names=T, col.names=F)
    }
  )
}