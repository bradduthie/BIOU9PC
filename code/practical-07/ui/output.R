#' @title Community stability UI
#' @description Output area for out render function from foodwebs() in foodwebs.R via server.R. Also provides output areas for data table rendering and CSV downloads.
#' @param id id passed from ui.R, used as prefix for ids and classes using ns()
#' @param h2 Heading passed form ui.R
#' @param intro Intro passed form ui.R, appears in green alert box.
output_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # put your ui elements for inputs and output inside the content section
  tags$section(class=ns("content"),
    div(class="row",
      div(class="col-lg-6 col-lg-offset-3", id=ns("output"),
        if(!is.null(h2)){ h2(class=ns("title"), h2) },
        if(!is.null(intro)){ div(class="alert alert-info", role="alert", intro) }
      )
    ),
    div(class="row",
      div(class="col-lg-4",
        h3("Community matrix"),
        downloadButton(ns("matrix.csv"), label="Download matrix CSV"),
        tableOutput(ns("matrix"))
      ),
      div(class="col-lg-8",
        h3("Community diagram"),
        plotOutput(ns("diagram"))
      )
    ),
    div(class="row",
      div(class="col-lg-4",
        h3("Community stability"),
        tableOutput(ns("summary")),
        downloadButton(ns("summary.csv"), label="Download summary CSV"),
        br(),br(),
        tableOutput(ns("extinctions")),
        downloadButton(ns("extinctions.csv"), label="Download extinctions CSV"),
        br(),br()
      ),
      div(class="col-lg-8",
        tabsetPanel(
          type="tabs", id="hist",
          tabPanel(
            "Prediction Plot",
            plotOutput(ns("stability"))
          ),
          tabPanel(
            "Prediction Data", 
            class="text-center", 
            downloadButton(ns("stability.csv"), label="Download CSV"),
            tableOutput(ns("stabilitydata"))
          )
        )
      )
    )
  )
}