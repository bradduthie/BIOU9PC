model3 <- function(input, output, session, params){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  monod.data <- reactive({
    validate(
      need(input$r1   >= 0, "Population growth rate 1 needs to be ≥ 0"),
      need(input$r2   >= 0, "Population growth rate 1 needs to be ≥ 0"),
      need(input$Ku1  >= 0, "Resource concentration at which effective growth rate = r/2 1 needs to be ≥ 0"),
      need(input$Ku2  >= 0, "Resource concentration at which effective growth rate = r/2 2 needs to be ≥ 0"),
      need(input$D    >= 0, "Flow rate needs to be ≥ 0"),
      need(input$R0   >= 0, "Resource concentration into the system needs to be ≥ 0"),
      need(input$R01  >= 0, "Initial Resource concentration  needs to be ≥ 0"),
      need(input$N01  >= 0, "Initial consumer population size 1 needs to be ≥ 0"),
      need(input$N02  >= 0, "Initial consumer population size 2 needs to be ≥ 0")
    )
    #    browser()
    # run practical specific funtion using input data here
    monodcomp2  <- calcMONODcomp2(r1 = input$r1, r2 = input$r2, Ku1 = input$Ku1, Ku2 = input$Ku2, D = input$D, R0 = input$R0, yini = c(Consumer1 = input$N01, Consumer2 = input$N02, Resource = input$R01), times = params$times)
  })
  # Render population trajectories over time
  output$trajMONODPlot2 <- renderPlot({
    #    cat(file=stderr(), "rendering plot 1 trajectories...\n") 
    trajPlot <-
      # loading_progress("Rendering plot...")
      plotstyle()
    plotMONODcomp2(monod.data(), input$log)
  })
}