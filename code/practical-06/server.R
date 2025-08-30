# modules
source("helpers.R")
source("competition helper functions.R")
source("model-1.R")
source("model-2.R")
source("model-3.R")
# Default params for all modules
setup <- function(){
  max.time <- 200
  params <- list(
    max.time = max.time,
    times    = 1:max.time
  )
  return(params)
}
# server code run once per user session
shinyServer(function(input, output, session){
  # load module with server function name and UI id
  params <- setup()
  # params <- callModule(setup, "setup")
  callModule(model1, "model-1", params)
  callModule(model2, "model-2", params)
  callModule(model3, "model-3", params)
})