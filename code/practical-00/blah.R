
# Server function (output rendering)
blah <- function(input, output, session){
  # get the session name-space set by ui id
  ns = session$ns
  # Trigger reactive calculation functions
  blah.data <- reactive({
    validate(
      # need(input$c, "Colonisation rate required")
    )
    # cat(file=stderr(), "building levins.data \n")
    # run practical specific funtion using input data here
  })
  # Render Reactive projected population dynamics against time
  # output$Plot <- renderPlot({
  #   data <- blah.data()
  #   Plot <-
  #       plotstyle()
  # })
  # output$Data <- renderTable({
  #   data <- blah.data()
  #   Data <- data$matrix
  # },
  # striped=TRUE,
  # rownames=FALSE
  # #digits=data$digits
  # )
  # output$CSV <- downloadHandler(
  #   filename = function() { "blah.csv" },
  #   content = function(file) {
  #     data <- blah.data()
  #     write.table(data$matrix, file, sep=",", row.names=FALSE)
  #   }
  # )
}