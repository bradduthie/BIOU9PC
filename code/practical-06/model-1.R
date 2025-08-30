# Server function (output rendering)
model1 <- function(input, output, session, params){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  LVcomp.data <- reactive({
    validate(
      need(input$K1 > 0, "Carrying capacity 1 needs to be > 0"),
      need(input$K2 > 0, "Carrying capacity 2 needs to be > 0"),
      need(input$r1 > 0, "Population growth rate 1 needs to be > 0"),
      need(input$r2 > 0, "Population growth rate 2 needs to be > 0"),
      need(input$a12 >= -1 & input$a12 <=2, "Effect of Species 1 on 2 needs to be between -1 and 2"),
      need(input$a21 >= -1 & input$a21 <=2, "Effect of Species 2 on 1 needs to be between -1 and 2"),
      need(input$N01 > 0, "Prey population size needs to be > 0"),
      need(input$N02 > 0, "Predator population size needs to be > 0")
    )
    # run practical specific function using input data here
    LVcomp   <- calcLVcomp(input$a12, input$a21, input$K1, input$K2, input$r1, input$r2, yini = c(N1 = input$N01, N2 = input$N02), params$times)
  })
  # Render population trajectories over time
  output$trajPlot <- renderPlot({
#    cat(file=stderr(), "rendering plot 1 trajectories...\n") 
    trajPlot <-
      # loading_progress("Rendering plot...")
      plotstyle()
      plotLVtraj(LVcomp.data(), params$max.time)
    })
  # Render phase-plane diagram
   output$planePlot <- renderPlot({
#     cat(file=stderr(), "rendering plot 1 phase-plane...\n") 
     planePlot <-
       # loading_progress("Rendering plot...")
       plotstyle()
       plotLVpp(LVcomp.data(), params$max.time, K1 = input$K1, K2= input$K2, a12 = input$a12, a21 = input$a21)
   })
}