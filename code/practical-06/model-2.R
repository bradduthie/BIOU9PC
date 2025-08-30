model2 <- function(input, output, session, params){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  monod.data <- reactive({
    validate(
      need(input$r   >= 0, "Population growth rate needs to be ≥ 0"),
      need(input$Ku  >= 0, "Resource concentration at which effective growth rate = r/2 needs to be ≥ 0"),
      need(input$D   >= 0, "Flow rate needs to be ≥ 0"),
      need(input$R0  >= 0, "Resource concentration into the system needs to be ≥ 0"),
      need(input$R01 >= 0, "Initial Resource concentration  needs to be ≥ 0"),
      need(input$N01 >= 0, "Initial consumer population size needs to be ≥ 0")
      
    )
    #    browser()
    # run practical specific funtion using input data here
    monodcomp   <- calcMONODcomp(r = input$r, Ku = input$Ku, D = input$D, R0 = input$R0,  yini = c(Consumer = input$N01, Resource = input$R01), times = params$times)
  })
  # Render population trajectories over time
  output$trajMONODPlot <- renderPlot({
#    cat(file=stderr(), "rendering plot 1 trajectories...\n") 
    trajPlot <-
      # loading_progress("Rendering plot...")
      plotstyle()
    plotMONODtraj(monod.data(), params$max.time, input$log)
  })
  # Render phase-plane diagram
  output$planeMONODPlot <- renderPlot({
#    cat(file=stderr(), "rendering plot 2 phase-plane...\n") 
    planePlot <-
      #      loading_progress("Rendering plot...")
      plotstyle()
    plotMONODpp(monod.data(), params$max.time)
    #       plotLVcomp("phase-plane", LVcomp.data(), params$max.time, input)
  })
}