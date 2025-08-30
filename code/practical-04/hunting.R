# UI portion
# @param id can be set when the module is loaded in ui.R
hunting_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set default Parameters to govern population dynamics.
  b  <- 0.15 # Mean per-capita probability fecundity
  d  <- 0.14 # Mean per-capita probabiliy of mortality
  sd <- 0    # Standard deviation for estimating the population size. Smaller values mean that human observers more accurately estimate the population size.
  K        <- 2000 # Carrying capacity
  N0       <-  800 # Starting population size
  max.time <-   50 # Number of years the model is run
  trials   <-   10 # how many replicate populatinos to simulate?
  # Parameters that govern the hunting regimes
  h           <-   0 # Constant harvest number
  prop        <-   0 # Proportional harvest rate
  thresh      <-2000 # Harvest threshold
  prop_thresh <-   0 # What proportion to harvest, assuming the threshold is exceeded
  flex        <-   0.01 # Flexibility parameter for adaptive harvest
  span        <-   2    # How many years over which to assess change in population size for adaptive harvest? 
  h_initial   <-   0 #initial constant rate to hunt before adaptation begins
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
                       h4("Population dynamics:"),
                       tags$table(
                         tags$tr(
                           tags$td(width = "60%",
                            sliderInput(ns("b"), "Per-capita probability of birth (b)", width="100%", value=b, min=0.01, max=1, step=.01)),
                           tags$td(width = "40%",
                      numericInput(ns("N0"), "Initial population size", width="100%", value=N0, min=1))
                         ),
                         tags$tr(
                           tags$td(
                             sliderInput(ns("d"), "Per-capita probability of death (d)", width="100%", value=d, min=0.01, max=1, step=0.01)),
                           tags$td(
                             numericInput(ns("max.time"), "Max time", width="100%", value=max.time, min=1, max=1000, step=1))
                         ),
                         tags$tr(
                           tags$td(
                             sliderInput(ns("K"), "Carrying Capacity (K)", width="100%", value=K, min=0, max=20000, ticks=FALSE)),
                           tags$td(
                             numericInput(ns("trials"), "Trials (How many replicate populations to generate?)", width="100%", value=trials, min=1, max=1000))
                         ),
                         tags$tr(
                           tags$td(
                             sliderInput(ns("sd"), "Uncertainty in population size estimate", width="100%", value=sd, min=0, max=0.2, ticks = T, step = 0.01)), # even a tiny bit of noise really clouds the estimates of population sizes.
                           tags$td(
                             checkboxGroupInput(ns("show"), "What to show?", choices = c("95% CI" = "CI95","50% CI" = "CI50"), selected = "CI95")
                           )
                         )
                      ),
                      hr(),
                      h4("Hunting regimes:"),
                      radioButtons(
                        ns("regime"), "Hunting regime:", 
                        c("Control (no hunting)" = "control",
                          "Constant" = "constant",
                          "Proportional" = "prop",
                          "Threshold proportinal" = "thresh",
                          "Adaptive" = "adapt")
                        ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] == 'constant'", ns("regime")),
                        sliderInput(ns("h"), "Contant harvest number", width="100%", value=h, min=0, max=500)
                        ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] == 'prop'", ns("regime")),
                        sliderInput(ns("prop"), "Proportional harvest rate", width="100%", value=prop, min=0.0, max=1, step=.01, ticks=FALSE)
                        ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] == 'thresh'", ns("regime")),
                        tags$table(
                          tags$tr(
                            tags$td(
                              sliderInput(ns("prop_thresh"), "Proportional harvest rate", width="100%", value=prop, min=0.0, max=1, step=.01, ticks=FALSE)
                              ),
                            tags$td(
                              sliderInput(ns("thresh"), "Harvest threshold", width="100%", value=thresh, min=0, max=10000)
                              )
                            )
                          )
                        ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] == 'adapt'", ns("regime")),
                        tags$table(
                          tags$tr(
                            tags$td(
                              sliderInput(ns("flex"), "Sensitivity to recent change in population size", width="100%", value= flex, min=0.00, max=3, step=.01, ticks=FALSE)
                              ),
                            tags$td(
                              sliderInput(ns("span"), "Span (over how many years to assess change in population size)", width="100%", value = span, min=2, max=10, step=1, ticks=FALSE)
                              )
                          ),
                          tags$tr(
                            tags$td(
                              sliderInput(ns("h_initial"), "Initial constant harvest number (How many individuals to harvest before adaptation begins?)", width="100%", value = h_initial, min=0, max=500, step=1, ticks=FALSE)
                              ),
                            tags$td()
                            )
                          )
                        )
                      ),
                   div(class="col-xs-12 col-sm-10 col-lg-8",
                       # output areas
                       tabsetPanel(type="tabs", id="output",
                                   tabPanel("Plots",
                                            h3("Population size"),

                                            plotOutput(ns("Plot1")),
                                            h3("Hunting yield"),
                                            plotOutput(ns("Plot2"))
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
hunting <- function(input, output, session){
  # get the session name-space set by ui id
  ns = session$ns

  # Trigger reactive calculation functions
  Ncontrol.data <- reactive({
    validate(
      need(input$max.time > 1 & input$max.time <= 1000, "Max time must be between 1 and 1000."),
      need(input$trials > 0 & input$trials <= 1000, "Number of replicate populations  must be between 1 and 1000."),
      need(input$N0 > 0, "Initial populations sizes > 0 required."),
      need(input$b > 0, "Birth rate required"),
      need(input$d > 0, "Death rate required"),
      need(input$K, "Carrying Capacity required."),
      need(input$sd, "Observation uncertainty required.")
    )
    #    cat(file=stderr(), "Finding Ncontrol...\n")
    find_Ncontrol(input$max.time, input$N0, input$b, input$d, input$K, input$sd, input$trials)
  })
  Nconst.data <- reactive({
    validate(
      need(input$max.time > 1 & input$max.time <= 1000, "Max time must be between 1 and 1000."),
      need(input$trials > 0 & input$trials <= 1000, "Number of replicate populations  must be between 1 and 1000."),

      need(input$N0 > 0, "Initial populations sizes > 0 required."),
      need(input$b > 0, "Birth rate required."),
      need(input$d > 0, "Death rate required."),
      need(input$h, "Constant harvest number required."),
      need(input$K, "Carrying Capacity required."),
      need(input$sd, "Observation uncertainty required.")
    )
    #    cat(file=stderr(), "Finding Nconst...\n")
    find_Nconst(input$max.time, input$N0, input$h, input$b, input$d, input$K, input$sd, input$trials)
  })
  Nprop.data <- reactive({
    validate(
      need(input$max.time > 1 & input$max.time <= 1000, "Max time must be between 1 and 1000."),
      need(input$trials > 0 & input$trials <= 1000, "Number of replicate populations  must be between 1 and 1000."),
      need(input$N0 > 0, "Initial populations sizes > 0 required."),
      need(input$b > 0, "Birth rate required"),
      need(input$d > 0, "Death rate required"),
      need(input$K, "Carrying capacity required."),
      need(input$sd, "Observation uncertainty required."),
      need(input$prop, "Proportional harvest rate required.")
    )
    #    cat(file=stderr(), "Finding Nprop...\n")
    find_Nprop(input$max.time, input$N0, input$b, input$d, input$K, input$sd, input$prop, input$trials)
  })
  Nthres.data <- reactive({
    validate(
      need(input$max.time > 1 & input$max.time <= 1000, "Max time must be between 1 and 1000."),
      need(input$trials > 0 & input$trials <= 1000, "Number of replicate populations  must be between 1 and 1000."),
      need(input$N0 > 0, "Initial populations sizes > 0 required."),
      need(input$b > 0, "Birth rate required"),
      need(input$d > 0, "Death rate required"),
      need(input$K, "Carrying capacity required."),
      need(input$sd, "Standard distrbution required."),
      need(input$prop_thresh, "Proportional harvest rate required."),
      need(input$thresh, "Harvest threshold required.")
    )
    #    cat(file=stderr(), "Finding Nthres...\n")
    find_Nthres(input$max.time, input$N0,  input$b, input$d, input$K, input$sd, input$prop_thresh, input$thresh, input$trials)
  })
  Nadapt.data <- reactive({
    validate(
      need(input$max.time > 1 & input$max.time <= 1000, "Max time must be between 1 and 1000."),
      need(input$trials > 0 & input$trials <= 1000, "Number of replicate populations  must be between 1 and 1000."),
      need(input$N0 > 0, "Initial populations sizes > 0 required."),
      need(input$b > 0, "Birth rate required"),
      need(input$d > 0, "Death rate required"),
      need(input$h, "Constant harvest number required."),
      need(input$K, "Carrying Capacity required."),
      need(input$sd, "Standard distribution required."),
      need(input$flex, "Flexibility value required."),
      need(input$span, "Span value required.")
    )
    #    cat(file=stderr(), "Finding Nadapt...\n")
    find_Nadapt(input$max.time, input$N0, input$b, input$d, input$K, input$sd, input$flex, input$h_initial, input$span, input$trials)
  })
  # Render Reactive functions
  # Plot all the predicted population sizes from all of the trials
  output$Plot1 <- renderPlot({
    dat <- switch(input$regime,
                  control  = Ncontrol.data(),
                  constant = Nconst.data(),
                  prop     = Nprop.data(),
                  thresh   = Nthres.data(),
                  adapt    = Nadapt.data(),
                  Ncontrol.data())
    plotstyle()
    matplot(x = 1:input$max.time, y = dat$N, type = "l", lty = 1, col = rainbow(input$trials), ylab = "Population size (N)", xlab = "Timestep", ylim = range(dat$N, 0, na.rm = T))
    if(input$regime == 'thresh'){abline(h = input$thresh, lty = 2, lwd = 2)}
    abline(h = input$K, col = 'gray', lty = 2)
    points(x = apply(dat$N, 2, function(x){ifelse(any(x == 0), which(x == 0)[1]-1, NA)}), y = rep(0, input$trials), pch = 1, lwd = 2, cex = 2, col = rainbow(input$trials)) # this adds points to indicate when each replicate population goes extinct.
    if("CI95" %in% input$show){
      polygon(c(1:input$max.time, input$max.time:1), c(dat$CI[1,], rev(dat$CI[5,])), col = "#00000010", border = NA)
    }
    if("CI50" %in% input$show){
      polygon(c(1:input$max.time, input$max.time:1), c(dat$CI[2,], rev(dat$CI[4,])), col = "#00000030", border = NA)
    }
    mtext(side = 3, adj = 0.95, sprintf("Mean population size: %1.0f", mean(dat$N, na.rm = T)), line = -2)
    mtext(side = 3, adj = 0.95, sprintf("Mean minimum population size: %1.2f", mean(apply(dat$N, 2, min, na.rm = T))), line = -3)
    mtext(side = 3, adj = 0.95, sprintf("Percentage of replicate populations that went extinct: %1.2f", sum(apply(dat$N, 2, function(x){any(is.na(x)|x == 0)}))/ncol(dat$N)), line = -4)
  })
  output$Plot2 <- renderPlot({
    dat <- switch(input$regime,
                  control  = Ncontrol.data(),
                  constant = Nconst.data(),
                  prop     = Nprop.data(),
                  thresh   = Nthres.data(),
                  adapt    = Nadapt.data(),
                  Ncontrol.data())
      plotstyle()
      if(all(dat$H[!is.na(dat$H)] == 0)){
        matplot(x = 1:input$max.time, y = dat$H, type = "n", xlab = "", ylab = "")
        mtext('No hunting was performed', font = 2, line = -3, cex = 1.3)
      } else {
        matplot(x = 1:input$max.time, y = dat$H, type = "l", xlab = "Timestep", ylab = "Hunting yield", col = rainbow(input$trials), lty = 1)
        mtext(side = 3, adj = 0.95, sprintf("Mean yield: %1.2f", mean(dat$H, na.rm = T)), line = -2)
        mtext(side = 3, adj = 0.95, sprintf("Standard deviation of yield: %1.2f", sd(dat$H, na.rm = T)), line = -3)
        mtext(side = 3, adj = 0.95, sprintf("Mean annual variation in yield: %1.2f", mean(apply(dat$H, 2, sd, na.rm = T))), line = -4)
      }
  })
  output$Data <- renderTable({
    dat <- switch(input$regime,
                  control  = Ncontrol.data(),
                  constant = Nconst.data(),
                  prop     = Nprop.data(),
                  thresh   = Nthres.data(),
                  adapt    = Nadapt.data(),
                  Ncontrol.data())
    Data <- data.frame(timestep = 1:input$max.time, N = dat$N, H = dat$H)
  },
  striped=TRUE,
  rownames=FALSE,
  digits=0
  )
  output$CSV <- downloadHandler(
    filename = function() { "popsize-hunting-yield.csv" },
    content = function(file) {
      dat <- switch(input$regime,
                    control  = Ncontrol.data(),
                    constant = Nconst.data(),
                    prop     = Nprop.data(),
                    thresh   = Nthres.data(),
                    adapt    = Nadapt.data(),
                    Ncontrol.data())
      Data <- data.frame(timestep = 1:input$max.time, dat$N, dat$H)
      write.csv(Data, file, row.names=FALSE)
    }
  )

}