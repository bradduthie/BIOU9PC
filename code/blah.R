# UI portion
# @param id can be set when the module is loaded in ui.R
blah_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # set defaults
  # put your ui elements for inputs and output inside the content section
  tags$section(class=ns("content"),
               div(class="row justify-content-center",
                   div(class="col-xs-12 col-sm-10 col-lg-4",
                       if(!is.null(h2)){
                         div(class="row justify-content-center",
                             div(class="col-xs-12 col-sm-10 col-lg-12",
                                 h2(class=ns("title"), h2)
                             )
                         )
                       },
                       if(!is.null(intro)){
                         div(class="row justify-content-center justify-content-lg-start",
                             div(class="col-xs-12",
                                 div(class="alert alert-info", role="alert", intro)
                             )
                         ) 
                       }
                       # Inputs
                   ),
                   div(class="col-xs-12 col-sm-10 col-lg-8",
                       # output areas
                       tabsetPanel(type="tabs", id="output",
                                   tabPanel("Plots"
                                            # plotOutput(ns("Plot"))
                                   ),
                                   tabPanel("Data", class="text-center"
                                            # downloadButton(outputId=ns("CSV"), label="Download CSV"),
                                            # tableOutput(ns("Data"))
                                   )
                       )
                   )
               )
  )
}