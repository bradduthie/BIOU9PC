library(shiny)
# UI settings
title <- "BIOU9PC Practical 5: Predation - Population and Community Ecology"
h1 <- "Practical 5: Predation"
handout.path <- "docs/biou9pc-05.pdf"
alert <- "Right click and 'Save Image As' to download plots."
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
            h1(class="app-title text-center", h1),
            p(class="alert alert-warning", alert)
          )
        )
      ),
      tabsetPanel(type="tabs", id="tabs",
        tabPanel("1. Simplest model", value = 'model-1', model_UI("model-1", intro="This model presents the simplest possible Lotka-Volterra predator-prey model. This is a continuous-time model in which the change in population size of the predator is affected by the number of prey, and the change in population size of the prey is affected by the number of predators. It is put into effect as a pair of coupled ordinary differential equations.")),
        tabPanel("2. With carrying capacity", value = 'model-2', model_UI("model-2",  intro="This model adds a carrying capacity for prey to the previous model. Thus, the change in population size of the predator is affected by the number of prey, but now the change in population size of the prey is affected both by the number of predators and the number of prey. In other words, they experience predation as well as density dependence.")),
        tabPanel("3. With refuges", value = 'model-4', model_UI("model-4", intro="In this model, we add refuges to the simplest model. Thus, as the prey population size declines, it's harder for predators to locate and consume the very last prey.")),
        tabPanel("4. With carrying capacity and Allee effect", value = 'model-3', model_UI("model-3", intro="In this model, we add more realism to the model with carrying capacity by including an Allee effect to the prey population. In other words, there is a minimal population size, below which the population growth rate of the prey decreases. One way this could occur is if it becomes more difficult to find mates at small population sizes."))
        # tabPanel("5. Competition between predators", value = 'model-5', model_UI("model-5", intro="In this model, We add competiton between predators to the simplest model. ."))
        
      )
    ),
    tags$script(src="shared/main.js"), # scripts better loaded outside of head
    title = title,
    theme = "shared/main.css" # our custom bootstrap styles
  )
)  