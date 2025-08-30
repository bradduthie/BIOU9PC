# UI portion
# @param id can be set when the module is loaded in ui.R
demographics_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  max.time <- 50 # How many timesteps to run the simulation
  b <- 0.10 # The mean number of offspring produced by each member of the population
  d <- 0.09 # The mean probability of death for every member of the population
  N0 <- 200 # Initial population sizes
  trials <- 10 # How many replicate populations to generate
  # standardD <- 76 # Standard distribution
  legend <- "topright"
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
                                 div(class="alert alert-info", role="alert", intro),
                                 div(class="alert alert-info", role="alert", "In this model, deaths occur first, so the only organisms that breed are those that survive.  N[t+1] <- N[t] + B - D, where D is a random draw from a binomial distribution with probability d and size N[t], and B is a random draw from a Poisson distribution with lambda = b*N[t])")
                             )
                         ) 
                       },
                       # Inputs
                       tags$table(
                         tags$tr(
                           tags$td(width = "50%",
                                   sliderInput(ns("b"), "Per-capita probability of birth (b)", width="100%", value=b, min=0.01, max=1, step=.01)
                           ),
                           tags$td(width = "50%",
                                   sliderInput(ns("N0"), "Initial population size", width="100%", value=N0, min=1, max=1000)
                           )
                         ),
                         tags$tr(
                           tags$td(
                             sliderInput(ns("d"), "Per-capita probability of death (d)", width="100%", value=d, min=0.01, max=1, step=0.01)
                           ),
                           tags$td(
                             sliderInput(ns("max.time"), "Max time", width="100%", value=max.time, min=1, max=1000, step=1)
                           )
                         ),
                         tags$tr(
                           tags$td(
                             sliderInput(ns("trials"), "Trials (How many replicate populations to generate?)", width="100%", value=trials, min=1, max=100, step=1)
                           ),
                           tags$td(
                             selectInput(ns("legend"), "Legend position", c("topleft", "topright", "bottomleft", "bottomright"), selected=legend, width="100%")
                           )
                         ),
                         tags$tr(
                           tags$td(
                             checkboxGroupInput(ns("show"), "What to show?", choices = c("Deterministic" = 'det', "Replicates" = "reps", "95% CI" = "CI95","50% CI" = "CI50"), selected = "reps")
                           ),
                           tags$td()
                         )
                       ) # end table. 
                   ),
                   div(class="col-xs-12 col-sm-10 col-lg-8",
                       # output areas
                       tabsetPanel(type="tabs", id="output",
                                   tabPanel("Plots",
                                            h3("Population sizes"),
                                            plotOutput(ns("popsizePlot"))
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
demographics <- function(input, output, session){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  demographics.data <- reactive({
    validate(
      need(input$max.time > 1, "Max time > 1 required to make predictions into the future."),
      need(input$b > 0, "Birth rate > 0 required."),
      need(input$d > 0, "Death rate > 0 required."),
      need(input$N0 > 0, "Initial population sizes > 0 required."),
      need(input$trials > 0, "Number of replicate populations required.")
    )
    # cat(file=stderr(), "building levins.data \n")
    # run practical specific funtion using input data here
    demographic_stochasticity(input$max.time, input$b, input$d, input$N0, input$trials)
  })
  legend.pos <- reactive({
    validate(
      need(input$legend, "Legend position required.")
    )
    position_legend(input$legend)
  })
  # Render Reactive functions
  # Plot all the predicted population sizes from all of the trials
  output$popsizePlot <- renderPlot({
    data <- demographics.data()
    legend <- legend.pos()
    popsizePlot <-
    plotstyle()
    matplot(x = 1:data$max.time, y = t(data$N), type = "n",  ylab = "Population size (N)", xlab = "Timesteps")
    
    if("reps" %in% input$show){
      points(x = apply(data$N, 1, function(x){ifelse(any(x == 0), which(x == 0)[1]-1, NA)}), y = rep(0, input$trials), pch = 1, lwd = 2, cex = 2, col = rainbow(input$trials)) # this indicates when each replicae populatin goes extnict. 
      data$N[data$N == 0] <- NA
      matlines(x = 1:data$max.time, y = t(data$N), type = "l", lty = 1, col = rainbow(input$trials))
    }
    if("det" %in% input$show){
      if(any(data$Ndet == 0)){points(which(data$Ndet == 0)[1]-1, 0, pch = 16, cex = 2)}
      data$Ndet[data$Ndet == 0] <- NA
      lines(x = 1:data$max.time, data$Ndet, lwd = 3, lty = 2)
      legend(legend$pos, lty = 2, legend = c("Deterministic"), bty = 'n', inset = 0.05, cex = 1.5, lwd = 2,  border = NA)
    }
    if("CI95" %in% input$show){
      polygon(c(1:data$max.time, data$max.time:1), c(data$CI[1,], rev(data$CI[5,])), col = "#00000010", border = NA)
    }
    if("CI50" %in% input$show){
      polygon(c(1:data$max.time, data$max.time:1), c(data$CI[2,], rev(data$CI[4,])), col = "#00000030", border = NA)
    }
  })
  # Plot the mean population size, plus the 50 and 95% confidence intervals around the mean

  # Show how the variation in population size among replicate trials increases through time. 
  # output$variationPlot <- renderPlot({
  #   data <- demographics.data()
  #   variationPlot <-
  #     plotstyle()
  #     plot(x = 1:data$max.time, y = data$variation.y, xlab = "Timestep", ylab = "Standard deviation of population size", type = "l")
  # })
  output$Data <- renderTable({
    data <- demographics.data()
    Data <- data$matrix
  },
  striped=TRUE,
  rownames=FALSE
  #digits=data$digits
  )
  output$CSV <- downloadHandler(
    filename = function() { "demographic-stochasticity.csv" },
    content = function(file) {
      data <- demographics.data()
      write.table(data$matrix, file, sep=",", row.names=FALSE)
    }
  )
}