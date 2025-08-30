library(shiny)
library(popbio)
library("deSolve");
# modules keep things tidy and reduce server load
source("practical2.R")
 
# UI code called once per R session (least often)
shinyUI(
  bootstrapPage(
    # These are reloaded with updated Bootstrap scripts as one minified script (main.js)
    tags$head(
      tags$meta(name="viewport", content="width=device-width, initial-scale=1")
    ),
    div(class="container-fluid",
        tags$header(class="app-header",
                    div(class="row justify-content-center",
                        div(class="col-xs-12 col-sm-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
                            h1(class="app-title text-center", "Practical 2: Matrix population models"),
                            p(class="alert alert-warning", "Right click and 'Save Image As' to download the graph, or use the 'Download CSV' button to export table data."
                            )
                        )
                    )
        ),
        practical2_UI("dynamics", intro="The aim of this week’s computer lab is to understand, construct and use matrix models to predict the dynamics of age - or stage - structured populations.")
    ),
    tags$script(src="shared/main.js"), # scripts better loaded outside of head
    title = "BIOU9PC Practical 2: Matrix population models - Population and Community Ecology",
    theme = "shared/main.css" # our custom bootstrap styles
  )
)  