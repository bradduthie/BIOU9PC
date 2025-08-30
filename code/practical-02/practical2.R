# UI portion
# @param id can be set when the module is loaded in ui.R
practical2_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  steps <- 20
  stages <- 3
  p <- c(20,20,20)
  m <- c(
    0.0, 1.1, 4.5,
    0.3, 0.0, 0.0,
    0.0, 0.5, 0.1)
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
                       tags$label("Survivial Probability Matrix"),
                       div(class="row",
                           div(class="col-xs-4",
                               div(class="bg-primary panel-body",
                                   span(class="text-primary", "Class 1"),
                                   numericInput(inputId=ns("m1"), label="", width="100%", value=m[1], min = 0, step = 0.01),
                                   numericInput(inputId=ns("m4"), label="", width="100%", value=m[4], min = 0, step = 0.01),
                                   numericInput(inputId=ns("m7"), label="", width="100%", value=m[7], min = 0, step = 0.01)
                               )
                           ),
                           div(class="col-xs-4",
                               div(class="bg-secondary panel-body",
                                   span(class="text-secondary", "Class 2"),
                                   numericInput(inputId=ns("m2"), label="", width="100%", value=m[2], min = 0, step = 0.01),
                                   numericInput(inputId=ns("m5"), label="", width="100%", value=m[5], min = 0, step = 0.01),
                                   numericInput(inputId=ns("m8"), label="", width="100%", value=m[8], min = 0, step = 0.01)
                               )
                           ),
                           div(class="col-xs-4",
                               div(class="bg-tertiary panel-body",
                                   span(class="text-tertiary", "Class 3"),
                                   numericInput(inputId=ns("m3"), label="", width="100%", value=m[3], min = 0, step = 0.01),
                                   numericInput(inputId=ns("m6"), label="", width="100%", value=m[6], min = 0, step = 0.01),
                                   numericInput(inputId=ns("m9"), label="", width="100%", value=m[9], min = 0, step = 0.01)
                               )
                           )
                       ),
                       tags$label("Population sizes"),
                       div(class="row",
                           div(class="col-xs-4",
                               div(class="bg-primary panel-body",
                                   span(class="text-primary", "Class 1"),
                                   numericInput(inputId=ns("p1"), label="", width="100%", value=p[1], min = 0)
                               )
                           ),
                           div(class="col-xs-4",
                               div(class="bg-secondary panel-body",
                                   span(class="text-secondary", "Class 2"),
                                   numericInput(inputId=ns("p2"), label="", width="100%", value=p[2], min = 0)
                               )
                           ),
                           div(class="col-xs-4",
                               div(class="bg-tertiary panel-body",
                                   span(class="text-tertiary", "Class 3"),
                                   numericInput(inputId=ns("p3"), label="", width="100%", value=p[3], min = 0)
                               )
                           )
                       ),
                       sliderInput(inputId=ns("steps"), label="Max Time", width="100%", value=steps, min=2, max=200)
                       # checkboxInput(inputId=ns("numbers"), label="Predict whole numbers?", value=TRUE, width="100%")
                   ),
                   div(class="col-xs-12 col-sm-10 col-lg-8",
                       # output areas
                       tabsetPanel(type="tabs", id="output",
                                   tabPanel("Plots",
                                            plotOutput(ns("NPlot")),
                                            plotOutput(ns("dN_dtPlot")),
                                            plotOutput(ns("NtotPlot")),
                                            plotOutput(ns("NpercPlot"))
                                   ),
                                   tabPanel("Data", class="text-center",
                                            downloadButton(outputId=ns("AllCSV"), label="Download CSV"),
                                            tableOutput(ns("AllData"))
                                   )
                       )
                   )
               )
  )
}
# colour
brand.primary = "#006635"
brand.secondary = "#6E2C6B"
brand.tertiary = "#ed8231"
bar.colours = c(brand.primary, brand.secondary, brand.tertiary)
bar.density=c(10,20,30)
bar.angle=c(45,325,45)
# plot style
plotstyle <- function(){
  par(
    pch=18, 
    bty="L", 
    # pty="s", 
    cex=1.2, 
    lwd=1, 
    las=1, 
    family="sans", 
    font.axis=1, 
    mar=c(5,8,2,2), 
    mgp=c(3, 1, 0),
    tck=-0.015
  )
}
# matrix multiplication
# (the operator %*% performs matrix multiplication)
matrix_multiplication <- function(m, N0, max.time, stages){
  survival.chances <- matrix(m, byrow=TRUE, nrow=3)
  if(max.time < 2){
    # cat(file=stderr(), "predicting 1 step only \n")
    # predict overall change for 1 step
    N <- survival.chances %*% N0
    return(list(
      "N"=N,
      "steps"=max.time
    ))
  } else {
    # cat(file=stderr(), "multiple time steps recognised \n")
    N <- matrix(rep(NA, stages*max.time), nrow=stages) # NA: Not Available / missing values
    N[,1] <- N0
    # predict overall change for all steps
    # (loop cycles max.time -1, performing calculation for all future steps above time step 1)
    for(t in 1:(max.time - 1)){ 
      N[,t+1] <- survival.chances %*% N[,t]
    }
    steps <- c(1:max.time)
    steps.matrix <- matrix(steps, nrow=max.time, ncol=1)
    colnames(steps.matrix) <- c("Time") 
    N.matrix <- matrix(N, nrow=max.time, ncol=3, byrow = T)
    colnames(N.matrix) <- paste("N Class ", 1:stages)
    # total population size at each time
    Ntot <- colSums(N)
    Ntot.matrix <- matrix(Ntot, nrow=max.time, ncol=1)
    colnames(Ntot.matrix) <- c("N Total")
    # fraction of population in each stage at each time
    Nperc <- sweep(N, 2, Ntot, "/")
    Nperc.matrix <- matrix(Nperc, nrow=max.time, ncol=3, byrow = T)
    colnames(Nperc.matrix) <- paste("Nperc Class ", 1:stages)
    modal.matrix <- cbind(N.matrix, Nperc.matrix, Ntot.matrix)
    rownames(modal.matrix) <- paste("Time ", steps)
    modal.file <- cbind(steps.matrix, N.matrix, Nperc.matrix, Ntot.matrix)
    
    # observed growth rate
    dN_dt <- c(NA, Ntot[-1]/Ntot[-max.time])
    # asymptotic (ie, long-run expectation) population growth rate
    lamb <- lambda(survival.chances)
    # asymptotic (ie, long-run expectation) stage distribution
    stable.dist <- stable.stage(survival.chances)
    
    # return values to output
    return(list(
      steps=max.time,
      stages=stages,
      N=N,
      Ntot=Ntot,
      Nperc=Nperc,
      dN_dt=dN_dt,
      lamb=lamb,
      stable=stable.dist,
      file=modal.file,
      matrix=modal.matrix
    ))
  }
}

# Server function (output rendering)
practical2 <- function(input, output, session){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  modal.data <- reactive({
    validate(
      need(input$m1, "Matrix values must be >= 0 to create plots"),
      need(input$m2, "Matrix values must be >= 0 to create plots"),
      need(input$m3, "Matrix values must be >= 0 to create plots"),
      need(input$m4, "Matrix values must be >= 0 to create plots"),
      need(input$m5, "Matrix values must be >= 0 to create plots"),
      need(input$m6, "Matrix values must be >= 0 to create plots"),
      need(input$m7, "Matrix values must be >= 0 to create plots"),
      need(input$m8, "Matrix values must be >= 0 to create plots"),
      need(input$m9, "Matrix values must be >= 0 to create plots"),
      need(input$p1, "Population sizes must be >= 0 to create plots"),
      need(input$p2, "Population sizes must be >= 0 to create plots"),
      need(input$p3, "Population sizes must be >= 0 to create plots"),
      need(input$steps > 1, "Max Time needs a number > 1 to create plots")
    )
    survival.chances <- c(input$m1, input$m2, input$m3, input$m4, input$m5, input$m6, input$m7, input$m8, input$m9) 
    N0 <- c(input$p1, input$p2, input$p3)
    # cat(file=stderr(), "building modal.data \n")
    matrix_multiplication(survival.chances, N0, input$steps, 3)
  })
  # Render Reactive projected population dynamics against time
  output$NPlot <- renderPlot({
    dat <- modal.data()
    if(dat$steps >= 2){
      NPlot <-
        plotstyle()
      # cat(file=stderr(), "rendering NPlot \n")
      matplot(t(dat$N), xlab="Time step", ylab="Individuals (N)", col = bar.colours, lty = 1, type = 'b')
      #legend("bottomright", legend=1:dat$stages, title="Class", inset=0.1, col=bar.colours, bg="white", bty="o", lty = 1)
    } else {
      # cat(file=stderr(), "not enough steps to render NPlot \n")
    }
  })
  # Render Reactive change in population size against time
  output$dN_dtPlot <- renderPlot({
    dat <- modal.data()
    if(dat$steps >= 2){
      dN_dtPlot <-
        plotstyle()
      # cat(file=stderr(), "rendering NpercPlot \n")
      plot(dat$dN_dt, xlab="Time step", ylab="Change in population size (dN/dt)", type = 'b')
      mtext(bquote(lambda: .(round(dat$lamb, 3))), side = 3, adj = 1, line = -2)
      abline(h = dat$lamb, col = 'red', lty = 2)
    } else {
      # cat(file=stderr(), "not enough steps to render dN_dtPlot \n")
    }
  })
  
  
  # Render Reactive proportion of individuals in each stage against time
  output$NpercPlot <- renderPlot({
    dat <- modal.data()
    if(dat$steps >= 2){
      NpercPlot <-
        plotstyle()
      # cat(file=stderr(), "rendering NpercPlot \n")
      barplot(dat$Nperc, xlab="Time step", ylab="Proportion of individuals (Nperc)", space=0, names.arg=1:dat$steps, border=bar.colours, col=bar.colours)
      mtext(paste("Stable stage distribution: ", paste(100*round(dat$stable, 2), collapse = " ")), side = 3, adj = 1, line = 0)
      legend("bottomright", legend=dat$stages:1, title="Class", inset=0.1, border=rev(bar.colours), fill=rev(bar.colours), bty="o")
    } else {
      # cat(file=stderr(), "not enough steps to render NpercPlot \n")
    }
  })
  # Render Reactive total population size against time
  output$NtotPlot <- renderPlot({
    dat <- modal.data()
    if(dat$steps >= 2){
      NtotPlot <-
        plotstyle()
      # cat(file=stderr(), "rendering NtotPlot \n")
      plot(1:dat$steps, dat$Ntot, xlab="Time steps", ylab="Total population size (Ntot)", type="b")
    } else {
      # cat(file=stderr(), "not enough steps to render NtotPlot \n")
    }
  })
  output$AllData <- renderTable({
    dat <- modal.data()
    AllData <- dat$matrix
  },
  striped=TRUE,
  rownames=TRUE
  #digits=dat$digits
  )
  output$AllCSV <- downloadHandler(
    filename = function() { "matrix-population-models.csv" },
    content = function(file) {
      dat <- modal.data()
      write.table(dat$file, file, sep=",", row.names=FALSE)
    }
  )
}