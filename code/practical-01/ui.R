library(shiny)
library("deSolve");
# modules keep things tidy and reduce server load
source("practical1.R")

# UI code called once per R session (least often)
ui <- fluidPage(
  # These are reloaded with updated Bootstrap scripts as one minified script (main.js)
  
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1")
  ),
  div(class="container-fluid",
      tags$header(class="app-header",
                  div(class="row justify-content-center",
                      div(class="col-xs-12 col-sm-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
                          h1(class="app-title text-center", "Practical 1: Population Growth & Regulation"),
                          p(class="alert alert-warning", "Right click and 'Save Image As' to download the graph, or use the 'Download CSV' button to export table data."
                            #              if(file.exists('www/docs/practical1.pdf'))
                            #                tags$a(class="btn btn-info pull-right", target="_blank", href="docs/practical1.pdf", "View Handout")
                          )
                      )
                  )
      ),
      tabsetPanel(type="tabs", id="tabs",
                  tabPanel("Exponential", practical1_UI("exponential", intro="The exponential model can be run in discrete time or continuous time. In discrete time, it is written as Nt+1 = r·Nt. In continuous time, it is written as Nt = N0·e^(rt). See practical handout for details")),
                  tabPanel("Logistic", practical1_UI("logistic", intro="The logistic model can be written in discrete or continuous time. We will use discrete time. We modify the discrete-time exponential model to incorporate a carrying capacity (K). The rate of population growth slows to 0 when the population size is close to K. It is written as Nt+1 = Nt+Nt·r(1-Nt/K)")),
                  tabPanel("MSY", practical1_UI("MSY", intro="The maximum sustainable yield is an important quantity for resource managers. It is defined as the maximal number of organisms that can be harvested per time step without driving the population to extinction. We calculate it by first determining the rate of population growth (dN/dt) as a function of the carrying capacity and population size. (This is simply the derivative of the logistic function). We then determine where dN/dt is maximal. This relationship can be expressed as dN/dt = N·r(1-N/K)"))
      )
  ),
  tags$script(src="shared/main.js"), # scripts better loaded outside of head
  title = "BIOU9PC Practical 1: Population Growth & Regulation- Population and Community Ecology",
  #theme = "shared/main.css" # our custom bootstrap styles
)