library(shiny)
library("deSolve");
# modules keep things tidy and reduce server load
source("levins.R")
source("migration.R")
source("projection.R")

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
            h1(class="app-title text-center", "Practical 3: Meta-populations"),
            p(class="alert alert-warning", "The aim of this week’s lab practical is to use and understand simple meta-population models.
Right click and 'Save Image As' to download graphs and use the 'Download CSV' button to export table data.")
          )
        )
      ),
      tabsetPanel(type="tabs", id="tabs",
        tabPanel("Levins' Model", levins_UI("levins", intro="We start with the meta-population model by Levins (1969). It predicts the fraction of occupied patches in a meta-population, given c, the per-patch probability of colonisation and e, the per-patch probability of extinction. The colonization rate is given by c*P*(1-P), where P is the fraction of patches that are occupied. It is maximised when 1/2 of the patches are occupied (WHY?). The extinction rate is given by e*P, which increases linearly with the number of occupied patches (WHY?). Pstar gives the equilibrium fraction of occupied patches. ")),
        tabPanel("Migration", migration_UI("migration", intro="Levin’s model assumes that the interlinked sub-populations grow logistically, but it elides the details of their population sizes, focusing only on the colonization and extinction of patches. Here, we examine the migration between a set of populations that are equally spaced along a line. Assuming random disersal of migrants, the frequency of migration between two patches is inversely dependent on the distance that separates them. The aim of this part of the practical is to understand the effects of emigration and immigration.")),
        tabPanel("Dynamics in a Meta-population", projection_UI("projection", intro="Levin’s model assumes that the interlinked sub-populations grow logistically, but it elides the details of their population sizes, focusing only on the colonization and extinction of patches. Here, expand on his model to examine sub-population dynamics in a set of patches linked by migration. In each patch, the sub-population grows logistically, but they can vary in carrying capacity (K) and intrinsic population growth rate (r). The main aim of this part of the practical is to understand the nature of source-sink dynamics."))
      )
    ),
    tags$script(src="shared/main.js"), # scripts better loaded outside of head
    title = "BIOU9PC Practical 3: Meta-populations - Population and Community Ecology",
    theme = "shared/main.css" # our custom bootstrap styles
  )
)  