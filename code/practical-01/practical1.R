# UI portion
# @param id can be set when the module is loaded in ui.R
practical1_UI <- function(id, h2=NULL, intro=NULL) {
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  if(id=="exponential"){
    size <- 100
    rate <- 1.1
    steps <- 20
    K <- 999999999
    infK <- NULL
  }
  if(id=="logistic"){
    size <- 15
    rate <- 0.1
    steps <- 100
    K <- 500
    infK <- FALSE
  }
  if(id=="MSY"){
    size <- 15
    rate <- 0.1
    steps <- 20
    K <- 100
    infK <- FALSE
  }
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
        # conditions for panels are JS expressions - NOT R expressions
        conditionalPanel(
          condition="input.tabs != 'MSY'",
          numericInput(inputId=ns("size"), label="Population size (N)", 
                       width="100%", value=size, min=1),
          sliderInput(inputId=ns("steps"), label="Max Time (t)", width="100%", 
                      value=steps, min=1, max=1000)
        ),
        sliderInput(inputId=ns("rate"), label="Growth rate (r)", width="100%", 
                    value=rate, min=0, max=3, step=0.01, ticks=FALSE, 
                    animate = animationOptions(interval = 100, loop = F, 
                                               playButton = "PLAY", 
                                               pauseButton = "PAUSE")),
        conditionalPanel(
          condition="input.tabs != 'Exponential'",
          sliderInput(inputId=ns("K"), label="Carrying Capacity (K)", width="100%", value=K, min=0, max=5000, ticks=T, animate = animationOptions(interval = 100, loop = F, playButton = "PLAY", pauseButton = "PAUSE"))
        ),
        checkboxInput(inputId=ns("numbers"), label="Predict whole numbers?", value=FALSE, width="100%"),
        conditionalPanel(
          condition="input.tabs == 'Logistic'",
          checkboxInput(inputId=ns("infK"), label="Set infinite Carrying Capacity?", value=FALSE, width="100%")
        ),
        conditionalPanel(
          condition="input.tabs == 'Exponential'",
          checkboxInput(inputId=ns("log"), label="Log-transform Y axis?", value=FALSE, width="100%")
        )
        
      ),
      div(class="col-xs-12 col-sm-10 col-lg-8",
        if(id=="exponential"){
          tabsetPanel(type="tabs", id="exp-discOutput",
            tabPanel("Discrete Plot", plotOutput(ns("discPlot"))),
            tabPanel("Discrete Data", class="text-center",
              downloadButton(outputId=ns("discCSV"), label="Download CSV"), 
              tableOutput(ns("discData"))
            )
          )
        },
        if(id=="exponential") hr(),
        if(id=="exponential"){
          tabsetPanel(type="tabs", id="exp-contOutput",
            tabPanel("Continuous Plot", plotOutput(ns("contPlot"))),
            tabPanel("Continuous Data", class="text-center",
              downloadButton(outputId=ns("contCSV"), label="Download CSV"), 
              tableOutput(ns("contData"))
            )
          )
        },
        if(id=="logistic"){
          tabsetPanel(type="tabs", id="logOutput",
            tabPanel("Plot", 
               plotOutput(ns("logPlot"))
            ),
            tabPanel("Data", class="text-center",
              downloadButton(outputId=ns("logCSV"), label="Download CSV"), 
              tableOutput(ns("logData"))
            )
          )
        },
        if(id=="MSY"){
          tabsetPanel(type="tabs", id="msyOutput",
            tabPanel("Plot",
              plotOutput(ns("msyPlot"))
            ),
            tabPanel("Data", class="text-center",
              downloadButton(outputId=ns("msyCSV"), label="Download CSV"),
              tableOutput(ns("msyData"))
            )
          )
        }
      )
    )
  )
}
# colour
brand.primary = '#006635'
brand.secondary = '#6E2C6B'
# plot style
plotstyle <- function(){
  par(
    pch=18, 
    bty="L", 
    # pty="s", 
    cex=1.2, 
    lwd=2, 
    las=1, 
    family="sans", 
    font.axis=1, 
    mar=c(5,8,2,2), 
    mgp=c(3, 1, 0),
    tck=-0.015
  )
}
# Exponential Growth Calculations
exponential_growth <- function(N, r, max.time, round.num){
  N2 <- N
  # Fiddle with N to plot discrete for each time step
  # (each result uses population estimate from previous time step)
  if(max.time>1000)max.time <- 1000
  for(t in 1:(max.time-1)){
    N[t+1] <- r*N[t]
  }
  # Fiddle with N2 to plot continuous for each time step
  # (each result based off initial population size)
  N2 <- N2[1]*exp(log(r)*1:max.time)
  
  # prepare table output
  table.digits <- 3
  # Check for whole numbers request
  if(isTRUE(round.num)){
    N <- round(N)
    N2 <- round(N2)
    table.digits <- 0
  }
  steps <- c(1:max.time)
  steps.matrix <- matrix(steps, nrow=max.time, ncol=1)
  # build discrete tables
  disc.matrix <- matrix(N, nrow=max.time, ncol=1)
  file.disc.matrix <- cbind(steps.matrix, disc.matrix)
  colnames(disc.matrix) <- c("N")
  colnames(file.disc.matrix) <- c("Time", "N")
  rownames(disc.matrix) <- paste("Time ", steps)
  # build continuous tables
  cont.matrix <- matrix(N2, nrow=max.time, ncol=1)
  file.cont.matrix <- cbind(steps.matrix, cont.matrix)
  colnames(cont.matrix) <- c("N")
  colnames(file.cont.matrix) <- c("Time", "N")
  rownames(cont.matrix) <- paste("Time ", steps)
  # return for output rendering
  return(list(
    "N"=N,
    "N2"=N2,
    "steps"=steps,
    "digits"=table.digits,
    "dmatrix"=disc.matrix,
    "dfile"=file.disc.matrix,
    "cmatrix"=cont.matrix,
    "cfile"=file.cont.matrix
  ))
}
# Logistic Growth Calculations
logistic_growth <- function(N, r, max.time, K, infK, round.num){
  # set vectors
  if(isTRUE(infK)){
    K <- Inf
  }
  if(max.time>1000)max.time <- 1000
  
  for(t in 1:(max.time-1)) {
    N[t+1] <- N[t]+ r*N[t]*(1-N[t]/K)
  }
  # prepare table output
  table.digits <- 3
  # Check for whole numbers request
  if(isTRUE(round.num)){
    N <- round(N)
    table.digits <- 0
  }
  # create data
  steps <- c(1:max.time)
  steps.matrix <- matrix(steps, nrow=max.time, ncol=1)
  log.matrix <- matrix(N, nrow=max.time, ncol=1)
  file.log.matrix <- cbind(steps.matrix, log.matrix)
  colnames(log.matrix) <- c("N")
  colnames(file.log.matrix) <- c("Time", "N")
  rownames(log.matrix) <- paste("Time ", steps)
  # return output
  return(list(
    "N"=N,
    "K"=K,
    "steps"=steps,
    "matrix"=log.matrix,
    "file"=file.log.matrix
  ))
}
# Max Sustainable Yield Calculations
msy <- function(r, K, round.num){
  # set vectors
  N <- 1:(K*1.2)
  PGR <- N*r*(1-N/K)
  # Check for whole numbers request
  if(isTRUE(round.num)){
    PGR <- round(PGR)
    table.digits <- 0
  }
  
  # Maximal population growth rate
  max.rate <- max(PGR)
  # Optimal population size for maximal growth rate
  max.size <- N[which.max(PGR)]
  # create data
  n.matrix <- matrix(N, nrow=K*1.2, ncol=1)
  pgr.matrix <- matrix(PGR, nrow=K*1.2, ncol=1)
  msy.matrix <- cbind(n.matrix, pgr.matrix)
  colnames(msy.matrix) <- c("N", "Growth Rate")
  # return output
  return(list(
    "N"=N,
    "K"=K, 
    "PGR"=PGR,
    "yline"=max.rate,
    "xline"=max.size,
    "matrix"=msy.matrix,
    "file"=msy.matrix
  ))
}

# Server function (output rendering)
practical1 <- function(input, output, session) {
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  exp.data <- reactive({
    exponential_growth(input$size, input$rate, input$steps, input$numbers) 
  })
  log.data <- reactive({
    logistic_growth(input$size, input$rate, input$steps, input$K, input$infK, input$numbers) 
  })
  msy.data <- reactive({
    msy(input$rate, input$K, input$numbers) 
  })
  # Render Reactive Exponential output
  output$discPlot <- renderPlot({
    data <- exp.data()
    discPlot <- 
      plotstyle()
      plot(data$steps, data$N, xlab="Time step", ylab="Population size (N)", col=brand.primary, log = ifelse(input$log == T, 'y', ''))
  })
  output$discData <- renderTable({
    data <- exp.data()
    discData <- data$dmatrix
    },
    striped=TRUE,
    rownames=TRUE
    #digits=data$digits
  )
  output$discCSV <- downloadHandler(
    filename = function() { "exponential-discrete-growth.csv" },
    content = function(file) {
      data <- exp.data()
      write.table(data$dfile, file, sep=",", row.names=FALSE)
    }
  )
  output$contPlot <- renderPlot({
    data <- exp.data()
    contPlot <- 
      plotstyle()
      plot(data$steps, data$N2, xlab='Time step', ylab="Population size (N)", type="l", col=brand.primary, log = ifelse(input$log == T, 'y', ''))
  })
  output$contData <- renderTable({
    data <- exp.data()
    contData <- data$cmatrix
    },
    striped=TRUE,
    rownames=TRUE
    # digits= if (isTRUE(input$numbers)) 0 else 2
  )
  output$contCSV <- downloadHandler(
    filename = function() { "exponential-continuous-growth.csv" },
    content = function(file) {
      data <- exp.data()
      write.table(data$cfile, file, sep=",", row.names=FALSE)
    }
  )
  # Render Logistic output
  output$logPlot <- renderPlot({
    data <- log.data()
    logPlot <- 
      plotstyle()
      plot(data$steps, data$N, type="l", xlab="Time step", ylab="Population size (N)", col=brand.primary)
      if (data$K != Inf){
        abline(h=data$K, untf=FALSE, col=brand.secondary, lty="dashed")
      }
      # ggplot()
      # geom_hline(aes(yintercept=K))
      # geom_text(aes(0, K, label=K, vjust=-1))
  })
  output$logData <- renderTable({
    data <- log.data()
    logData <- data$matrix
    },
    striped=TRUE,
    rownames=TRUE
    # digits= if (isTRUE(input$numbers)) 0 else 2
  )
  output$logCSV <- downloadHandler(
    filename = function() { "logistic-growth.csv" },
    content = function(file) {
      data <- log.data()
      write.table(data$file, file, sep=",", row.names=FALSE)
    }
  )
  # Render MSY output
  output$msyPlot <- renderPlot({
    data <- msy.data()
    msyPlot <- 
      plotstyle()
      plot(
        data$N, data$PGR, 
        ylab=expression("Change in population size"~~(dN/dt)), 
        xlab="Population size (N)", 
        type="l",
        col=brand.primary
      )
      # if (data$K != Inf){
        abline(h=data$yline, untf=FALSE, col=brand.secondary, lty="dashed")
        abline(v=data$xline, untf=FALSE, col=brand.secondary, lty="dotted")
        abline(h=0, col = brand.secondary, lty = 3)
      # }
  })
  output$msyData <- renderTable({
    data <- msy.data()
    msyData <- data$matrix
    },
    striped=TRUE,
    rownames=FALSE
    # colnames= if (isTRUE(input$infK)) FALSE else TRUE
    # digits= if (isTRUE(input$numbers)) 0 else 2
  )
  output$msyCSV <- downloadHandler(
    filename = function() { "max-sustainable-yield.csv" },
    content = function(file) {
      data <- msy.data()
      write.table(data$file, file, sep=",", row.names=FALSE)
    }
  )
}