library(shiny)
# modules keep things tidy and reduce server load
source("ui/blah.R")
title <- "BIOU9PC Practical: - Population and Community Ecology"
h1 <- "Practical:"
handout.path <- "docs/biou9pc-.pdf"
alert <- "Right click and 'Save Image As' to download the graph, or use the 'Download CSV' button to export table data."
# UI code called once per R session (least often)
shinyUI(
  bootstrapPage(
    # These are reloaded with updated Bootstrap scripts as one minified script (main.js)
    suppressDependencies(
      "bootstrap",
      "font-awesome",
      "html5shiv",
      "respond"
    ),
    tags$head(
      tags$meta(name="viewport", content="width=device-width, initial-scale=1")
    ),
    div(class="container-fluid",
      tags$header(class="app-header",
        div(class="row justify-content-center",
          div(class="col-xs-12 col-sm-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
            h1(class="app-title text-center", h1),
            p(class="alert alert-warning", alert,
              if(file.exists(paste("www", handout.path, sep="/")))
                tags$a(class="btn btn-info pull-right", target="_blank", href=handout.path, "View Handout")
            )
          )
        )
      ),
      tabsetPanel(type="tabs", id="tabs",
                  tabPanel("Blah", blah_UI("blah", intro=""))
      )
    ),
    tags$script(src="shared/main.js"), # scripts better loaded outside of head
    title = title,
    theme = "shared/main.css" # our custom bootstrap styles
  )
)  