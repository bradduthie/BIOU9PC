# UI portion
# @param id can be set when the module is loaded in ui.R
model_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  a <- 0.012 # /day  m, rate of ingestion
  b <- 0.25 # assimilation efficiency
  d <- 0.1 # /day, mortality rate of predator
  r <- 0.2 # /day, potential population growth rate of prey
  N_init <- 30
  P_init <- 20
  K   <- 10000 # Carrying capacity
  f   <- 0 # Minimum sustainable population size for Prey
  Ref <- 0 # refuges for prey
#  beta <- 0 # # competition between predators. interesting, but not implememnted
  
    # put your ui elements for inputs and output inside the content section
  tags$section(class=ns("content"),
               div(class="row justify-content-center",
                   div(class="col-xs-12 col-sm-10 col-lg-4",
                       if(!is.null(h2)){
                         div(class="row justify-content-center",
                             div(class="col-xs-12 col-sm-10 col-lg-12", h2(class=ns("title"), h2))
                         )
                       },
                       if(!is.null(intro)){
                         div(class="row justify-content-center justify-content-lg-start",
                             div(class="col-xs-12",
                                 div(class="alert alert-info", role="alert", intro)
                             )
                         ) 
                       },
                       # Inputs
                       tags$table(
                         tags$tr(
                           tags$td(
                             sliderInput(inputId=ns("a"), label="Rate of ingestion (a)", width="100%", value=a, min=0.001, max=1, step=0.001, ticks=FALSE)      
                           ),
                           tags$td(
                             sliderInput(inputId=ns("b"), label="Assimilation efficiency (b)", width="100%", value=b, min=0.01, max=2, step=0.01, ticks=FALSE)      
                           )
                         ),
                         tags$tr(
                           tags$td(
                             sliderInput(inputId=ns("d"), label="Mortality rate of predator (d)", width="100%", value=d, min=0.01, max=2, step=0.01, ticks=FALSE)
                           ),
                           tags$td(
                             sliderInput(inputId=ns("r"), label="Intrinsic population growth rate of prey (r)", width="100%", value=r, min=0.01, max=2, step=0.01, ticks=FALSE) 
                           )
                         ),
                         tags$tr(
                           tags$td(
                             numericInput(inputId=ns("N_init"), label="Prey population size (N0)", width="100%", value=N_init, min=1, max=NA) 
                           ),
                           tags$td(
                             numericInput(inputId=ns("P_init"), label="Predator population size (P0)", width="100%", value=P_init, min=1, max=NA)
                           )
                         )
                       ),
                       ### Gotta get these conditional panels working right!
                       conditionalPanel(
                         condition="input.tabs == 'model-2' || input.tabs == 'model-3'",
                         sliderInput(inputId=ns("K"), label="Carrying Capacity (K)", width="100%", value=K, min=0, max=10000, ticks=FALSE)
                       ),
                       conditionalPanel(
                         condition= "input.tabs == 'model-4'",
                         numericInput(inputId=ns("Ref"), label="Size of prey refuges (Ref)", width="100%", value=f, min=0, max=NA)
                       ),
                       conditionalPanel(
                         condition= "input.tabs == 'model-3'",
                         numericInput(inputId=ns("f"), label="Minimum sustainable population size for prey (f)", width="100%", value=f, min=0, max=NA)
                       )
                       # conditionalPanel(
                       #   condition= "input.tabs == 'model-5'",
                       #   numericInput(inputId=ns("beta"), label="competitoin between predators (beta)", width="100%", value=beta, min=0, max=NA)
                       # )
                       ### Need to add an addition conditional panel here to set the refuge size for model 4. 
                   ),
                   
                   div(class="col-xs-12 col-sm-10 col-lg-8",
                       # output areas
                       plotOutput(ns("trajPlot")),
                       plotOutput(ns("planePlot"))
                   )
               )
  )
}