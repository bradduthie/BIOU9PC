library(shiny)
library(igraph)
library("deSolve");
# modules keep things tidy and reduce server load
source("ui/input.R")
source("ui/output.R")
title <- "BIOU9PC Practical 7: Food Webs - Population and Community Ecology"
h1 <- "Practical 7: Food Webs"
handout.path <- "docs/biou9pc-07.pdf"
alert <- "Determine the relative stability of ecological communities."
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
            h1(class="app-title text-center", h1)
          )
        )
      ),
      tabsetPanel(type="tabs", id="tabs",
        # in this practical all the tabs use the same id "foodwebs" so that the output knows what inputs to use.
        tabPanel("Build the food web", input_UI("foodwebs", intro="")),
      tabPanel("Examine the output", output_UI("foodwebs"))
      )
    ),
    tags$script(src="shared/main.js"), # scripts better loaded outside of head
    title = title,
    theme = "shared/main.css" # our custom bootstrap styles
  )
)  