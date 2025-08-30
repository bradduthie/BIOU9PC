source("helpers.R")
# server code run once per user session
shinyServer(function(input, output, session){
  # load module with server function name and UI id
  callModule(levins, "levins")
  callModule(migration, "migration")
  callModule(projection, "projection")
})