# Server function (output rendering)
model5 <- function(input, output, session, params){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  predation.data <- reactive({
    validate(
      need(input$a > 0, "Rate of ingestion needs to be > 0"),
      need(input$b > 0, "Assimilation efficiency needs to be > 0"),
      need(input$d > 0, "Mortality rate of predator needs to be > 0"),
      need(input$r > 0, "Potential population growth rate of prey needs to be > 0"),
      need(input$beta   >= 0, "Predator mutual interference needs to be ≥ 0"),
      need(input$N_init > 0, "Prey population size needs to be > 0"),
      need(input$P_init > 0, "Predator population size needs to be > 0")
      
    )
    # run practical specific funtion using input data here
    calc5(a = input$a, b = input$b, d = input$d, r = input$r, beta = input$beta, yini = c(N=input$N_init, P=input$P_init), times = params$times)
  })
  # Render population trajectories over time
  output$trajPlot <- renderPlot({
    cat(file=stderr(), "rendering plot 1 trajectories...\n") 
    trajPlot <-
      # loading_progress("Rendering plot...")
      plotstyle()
    plot1("trajectories", predation.data(), params$max.time)
  })
  # Render phase-plane diagram
  output$planePlot <- renderPlot({
    cat(file=stderr(), "rendering plot 1 phase-plane...\n") 
    planePlot <-
      # loading_progress("Rendering plot...")
      plotstyle()
    plot1("phase-plane", predation.data(), params$max.time)
  })
}