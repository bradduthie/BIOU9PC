source("helpers.R")
# server code run once per user session
shinyServer(function(input, output, session){
  # load module with server function name and UI id
  callModule(poisson, "poisson")
  callModule(binomial, "binomial")
  callModule(standard, "standard")
  callModule(demographics, "demographics")
  callModule(hunting, "hunting")
})