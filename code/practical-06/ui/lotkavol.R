# UI portion
# @param id can be set when the module is loaded in ui.R
model1_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  K1  <- 200   # Carrying Capacity Species 1
  K2  <- 190   # Carrying Capacity Species 2
  r1  <- 0.15   # Maximum potential population growth rate Species 1
  r2  <- 0.15  # Maximum potential population growth rate Species 1
  a12 <- 0.0  # Effect of Species 1 on Species 2
  a21 <- 0.0  # Effect of Species 2 on Species 1
  N01 <- 60   # initial populaion size Species 1
  N02 <- 60   # initial populaion size Species 2
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
                           tags$td(sliderInput(inputId=ns("r1"), label="Intrinsic population growth rate Species 1 (R1)", width="100%", value=r1, min=0, max=3, step = 0.01, ticks=FALSE)),
                           tags$td(sliderInput(inputId=ns("r2"), label="Intrinsic population growth rate Species 2 (R2)", width="100%", value=r2, min=0, max=2, step = 0.01, ticks=FALSE))
                         ),
                         tags$tr(
                           tags$td(sliderInput(inputId=ns("K1"), label="Carrying Capacity Species 1 (K1)", width="100%", value=K1, min=0, max=2000, step=1, ticks=FALSE)),
                           tags$td(sliderInput(inputId=ns("K2"), label="Carrying Capacity Species 2 (K2)", width="100%", value=K2, min=0, max=2000, step=1, ticks=FALSE))
                         ),
                         tags$tr(
                           tags$td(sliderInput(inputId=ns("a12"), label="Effect of Species 1 on Species 2 (a12)", width="100%", value=a12, min=-1, max=2, step = 0.01,  ticks=FALSE)),
                           tags$td(sliderInput(inputId=ns("a21"), label="Effect of Species 2 on Species 1 (a21)", width="100%", value=a21, min=-1, max=2, step = 0.01, ticks=FALSE))
                         ),
                         tags$tr(
                           tags$td(
                             tags$hr(),
                             sliderInput(inputId=ns("N01"), label="Initial population size Species 1", width="100%", value=N01, min=0, max=3000, step=1, ticks=FALSE)
                           ),
                           tags$td(
                             tags$hr(),
                             sliderInput(inputId=ns("N02"), label="Initial population size Species 2", width="100%", value=N02, min=0, max=3000, step= 1, ticks=FALSE)
                           )
                         )
                       )
                   ),
                   div(class="col-xs-12 col-sm-10 col-lg-8",
                       # output areas
                       plotOutput(ns("trajPlot")),
                       plotOutput(ns("planePlot"))
                   )
               )
  )
}

model2_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  R0   <- 2.0   # Concentration of resources entering into the system
  D    <- 0.15   # Flow rate through the system
  r    <- 0.30  # Maximal per-capita increase of the consumer population
  Ku   <- 1.8  # The concentration of Resource that allows consumer population growth to be r/2
  N01    <- 1     # initial consumer concentration in tank
  R01    <- 0.5     # initial resource concentration in tank
  
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
                       sliderInput(inputId=ns("R0"), label="Resource concentration entering the system(R0)", width="100%", value=R0, min=0, max=10, step = 0.01, ticks=FALSE),
                       sliderInput(inputId=ns("D"), label="Flow rate though the system (D)", width="100%", value=D, min=0, max=0.3, ticks=FALSE),
                       tags$hr(),
                       sliderInput(inputId=ns("r"), label="Maximum potential population growth rate (r)", width="100%", value=r, min=0, max=3, step = 0.01,ticks=FALSE),
                       sliderInput(inputId=ns("Ku"), label="Resource concentration at which effective growth rate = r/2", width="100%", value=Ku, min=0, max=10, step = 0.01,ticks=FALSE),
                       tags$hr(),
                       sliderInput(inputId=ns("N01"), label="Initial consumer concentration", width="100%", value=N01, min=0, max=10, step = 0.01, ticks=FALSE),
                       sliderInput(inputId=ns("R01"), label="Initial resource concentration", width="100%", value=R01, min=0, max=10, step = 0.01, ticks=FALSE),
                       checkboxInput(inputId=ns("log"), label="Log Y axis?", value=T)
                   ),
                   div(class="col-xs-12 col-sm-10 col-lg-8",
                       # output areas
                       plotOutput(ns("trajMONODPlot")),
                       plotOutput(ns("planeMONODPlot"))
                   )
               )
  )
}

model3_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  R0   <- 2.0   # Concentration of resources entering into the system
  D    <- 0.15   # Flow rate through the system
  
  r1    <- 0.30  # Maximal per-capita increase of the consumer population
  r2    <- 0.30  # Maximal per-capita increase of the consumer population
  Ku1   <- 1.8  # The concentration of Resource that allows consumer population growth to be r/2
  Ku2   <- 1.8  # The concentration of Resource that allows consumer population growth to be r/2
  N01   <- 1     # initial consumer 1 concentration
  N02   <- 1     # initial consumer 2 concentration
  R01   <- 0.5   # initial resource concentration
  
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
                       sliderInput(inputId=ns("R0"), label="Resource concentration entering the system(R0)", width="100%", value=R0, min=0, max=10, step = 0.01, ticks=FALSE),
                       sliderInput(inputId=ns("D"), label="Flow rate through the system (D)", width="100%", value=D, min=0, max=0.3, ticks=FALSE),
                       tags$hr(),
                       tags$table(
                         tags$tr(
                           tags$td(sliderInput(inputId=ns("r1"), label="Maximum potential population growth rate (r1)", width="100%", value=r1, min=0, max=3, step = 0.01,ticks=FALSE)),
                           tags$td(sliderInput(inputId=ns("r2"), label="Maximum potential population growth rate (r2)", width="100%", value=r2, min=0, max=3, step = 0.01,ticks=FALSE))
                         ),
                         tags$tr(
                           tags$td(sliderInput(inputId=ns("Ku1"), label="Resource concentration at which growth = r/2 (Ku1)", width="100%", value=Ku1, min=0, max=10, step = 0.01,ticks=FALSE)),
                           tags$td(sliderInput(inputId=ns("Ku2"), label="Resource concentration at which growth = r/2 (Ku2)", width="100%", value=Ku2, min=0, max=10, step = 0.01,ticks=FALSE))
                         ),
                         tags$tr(
                           tags$td(
                             tags$hr(),
                             sliderInput(inputId=ns("N01"), label="Initial consumer concentration 1", width="100%", value=N01, min=0, max=10, step = 0.01, ticks=FALSE)),
                           tags$td(
                             tags$hr(),
                             sliderInput(inputId=ns("N02"), label="Initial consumer concentration 2", width="100%", value=N02, min=0, max=10, step = 0.01, ticks=FALSE))
                         )
                       ),
                       
                       sliderInput(inputId=ns("R01"), label="Initial resource concentration", width="100%", value=R01, min=0, max=10, step = 0.01, ticks=FALSE),
                       checkboxInput(inputId=ns("log"), label="Log Y axis?", value=T)
                   ),
                   div(class="col-xs-12 col-sm-10 col-lg-8",
                       # output areas
                       plotOutput(ns("trajMONODPlot2"))
                   )
               )
  )
}