source("helpers.R")
source("blah.R")
# server code run once per user session
shinyServer(function(input, output, session){
  # load module with server function name and UI id
  callModule(blah, "blah")
})