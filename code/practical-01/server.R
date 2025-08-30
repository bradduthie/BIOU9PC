# server code run once per user session
shinyServer(function(input, output, session) {
  # load module with server function name and UI id
  callModule(practical1, "exponential")
  callModule(practical1, "logistic")
  callModule(practical1, "MSY")
})