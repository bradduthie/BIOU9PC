source("helpers.R")
source("foodwebs.R")
# server code run once per user session
shinyServer(function(input, output, session){
  # load module with server function name and UI id
  callModule(foodwebs, "foodwebs")
})