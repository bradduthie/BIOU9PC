library(shiny)
library("deSolve");
# UI settings
title <- "BIOU9PC Practical 6: Competition - Population and Community Ecology"
h1    <- "Practical 6: Competition"
# modules keep things tidy and reduce server load
source("ui/lotkavol.R")
# UI code called once per R session (least often)
shinyUI(
  bootstrapPage(
    # These are reloaded with updated Bootstrap scripts as one minified script (main.js)
    tags$head(
      tags$meta(name="viewport", content="width=device-width, initial-scale=1")
    ),
    tags$head(
      tags$meta(name="viewport", content="width=device-width, initial-scale=1")
    ),
    div(class="container-fluid",
      tags$header(class="app-header",
        div(class="row justify-content-center",
          div(class="col-xs-12 col-sm-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
            h1(class="app-title text-center", h1)
            )
        )
      ),
      tabsetPanel(type="tabs", id="tabs",
        tabPanel("Lotka-Volterra Competition", model1_UI("model-1", intro="The purpose of this practical is for you to interact with the simplest Lotka-Volterra competition model.")),
        tabPanel("One-species Monod model", model2_UI("model-2", intro="The purpose of this practical is for you to interact with a Monod competition model.")),
        tabPanel("Two-species Monod model", model3_UI("model-3", intro="The purpose of this practical is for you to interact with a two-species Monod competition model."))
      )
    ),
    tags$script(src="shared/main.js"), # scripts better loaded outside of head
    title = title,
    theme = "shared/main.css" # our custom bootstrap styles
  )
)  