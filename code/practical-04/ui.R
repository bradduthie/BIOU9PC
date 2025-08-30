library(shiny)
library("deSolve");
# modules keep things tidy and reduce server load
source("poisson.R")
source("binomial.R")
source("standard.R")
source("demographics.R")
source("hunting.R")
# settings
title <- "BIOU9PC Practical 4: Stochasticity & Hunting - Population and Community Ecology"
h1 <- "Practical 4: Stochasticity & Hunting"
handout.path <- "docs/biou9pc-04.pdf"
alert <- "Right click and 'Save Image As' to download the graph, or use the 'Download CSV' button to export table data."
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
        tabPanel("Statistical Distributions", 
          poisson_UI("poisson", h2="Poisson Distribution", intro="The Poisson distribution expresses the probability of a given number of events that occur in a fixed interval of time or space, assuming that these events occur independently and with a mean rate. So, it's good for birth rates. It has only one parameter, lambda."),
          hr(),
          binomial_UI("binomial", h2="Binomial Distribution", intro="The binomial distribution gives the number of successes in a sequence of independent yes/no trials, each of which yields success with probability p. It's therefore useful for modeling mortality, since death can happen only once to each organism. For these purposes, we consider death as a success. Note that when the number of trials is large, the binomial distribution closely resembles the normal distribution. "),
          hr(),
          standard_UI("standard", h2="Normal Distribution", intro="The normal distribution is one of the most useful probability distributions, because the averages of random variables independently drawn from independent distributions converge to the normal. In other words, they become normally distributed when the number of random variables is sufficiently large. For our purposes, it's useful because most measurement errors are small, with only a few large ones. So it's useful for expressing random variation in continuous variables, such as population size or individual size.")
        ),
        tabPanel("Demographics", 
          demographics_UI("demographics", h2="Demographic Stochasticity", intro="Using the fundamental equation of population dynamics, we assume a closed system, so there is no migration. There is also no density-dependence, so the underlying dynamics are exponential growth. We create trials (replicate populations), each with initial population sizes.")
        ),
        tabPanel("Scenario analysis", 
          hunting_UI("hunting", h2="Stochastic population models")
        )
      )
    ),
    tags$script(src="shared/main.js"), # scripts better loaded outside of head
    title = title,
    theme = "shared/main.css" # our custom bootstrap styles
  )
)  