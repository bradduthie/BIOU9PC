# set defaults
N     <- 3 # Species in web on load
N.min <- 2 # Min species for web to be possible
N.max <-25 # Max species in web

#' @title Input UI
#' @description Matrix builder that is then used to create table, diagram and plotter output.
#' @param id id passed form ui.R, used as prefix for all ids within using ns()
#' @param h2 Heading passed form ui.R
#' @param intro Intro passed form ui.R, appears in green alert box.
input_UI <- function(id, h2=NULL, intro=NULL){
  if(is.null(id)){return("no id set in ui.R")}
  # set the id as name-space for ui elements
  ns <- NS(id)
  # put your ui elements for inputs and output inside the content section
  tags$section(class=ns("content"),
    div(class="row",
      div(class="col-xs-12 col-lg-6",
        if(!is.null(h2)){
          h2(class=ns("title"), h2)
        },
        div(class="alert alert-info", role="alert",
            tags$p("Determine how many species there will be in your ecological community, and resize the community matrix to suit. Provide names for your species, and indicate their trophic levels. Then specify the food web by indicating the type of interaction that occurs between each pair of species. The rules for these interactions are detailed in the practical handout. Briefly stated, they are:"),
            tags$p(style='text-indent: 1em;', tags$span(style='font-weight: bold;', 'C'), "should only appear on the diagonal (from top-left to bottom right)."),
            tags$p(style='text-indent: 1em;', "Any", tags$span(style='color:orange; font-weight: bold;', 'c'), "below the diagonal should be paired with a corresponding", tags$span(style='color:orange; font-weight: bold;', 'c'), "above it, as interspecific competition always includes two species."),
            tags$p(style='text-indent: 1em;', tags$span(style='color:green; font-weight: bold;', 'P'), "can only appear below the diagonal, and must be matched with a", tags$span(style='color:red; font-weight: bold;', 'p'), ", which can only appear above the diagonal, as every predator needs a prey."),
            tags$p(style='text-indent: 1em;', "Any ", tags$span(style='color:blue; font-weight: bold;', 'm'), "appearing below the diagonal should be paired with a corresponding", tags$span(style='color:blue; font-weight: bold;', 'm'), "above it, as mutualism also always includes two species."))),
      div(class="col-xs-12 col-lg-3",
        h3("Food web options:"),
        sliderInput(inputId=ns("N"), label="Number of species:", width="100%", value=N, min=2, max=N.max, step=1, ticks=F)
      ),
      div(class="col-xs-12 col-lg-3",
        h3("Interaction key:"),
        tags$ul(
          class = ns("interaction-key"),
          tags$li(strong("C"), " = Intra-specific competition"),
          tags$li(strong("c"), " = Inter-specific competition"),
          tags$li(strong("P"), " = Predator"), 
          tags$li(strong("p"), " = Prey"),
          tags$li(strong("m"), " = Mutualist")
        )
      )
    ),
    div(class="row",
      div(class="col-xs-12", id=ns("matrix"),
        div(
          class = ns("row"),
          div(class = ns("col"), h4(class = "mat-header", "Species name")),
          div(class = ns("col"), h4(class = "mat-header", "Trophic level")),
          div(class = ns("col"), h4(class = "mat-header", "Community matrix->"))
        ),
        lapply(1:N.max, build_rows, id, N)
      )
    )
  )
}
#' @title Build matrix rows
#' @description Begins each row with numbered species name and tropic level inputs. Then triggers the conditional wrappers for repeated matrix inputs to the full size of N max. Deigned to be run from for loop or apply function to reiterate a number of times.
#' @param i current iteration of function number, symbolises species/row number
#' @param id namespace passed in by initial call to input_UI() in ui.R keep all the ids properly named using ns()
#' @param N reactive to N slider input, so trophic choices never go above size of matrix selected.
build_rows <- function(i, id, N){
  ns <- NS(id)
  if(i <= N.min){
    div(
      class = ns("row"),
      id = ns(paste0("row-N",i)),
      build_species_input(i, id),
      build_trophic_input(i, id, N),
      # fills the first N.min rows with conditional wrappers after N.min columns
      lapply(1:N.max, build_interaction_wrappers, id, i)
    )
  } else {
    conditionalPanel(
      condition = sprintf("input['%s'] > %i", ns("N"), i-1),
      div(
        class = ns("row"),
        id = ns(paste0("row-N",i)),
        build_species_input(i, id),
        build_trophic_input(i, id, N),
        # fills all the following rows with conditional wrappers after N.min columns
        lapply(1:N.max, build_interaction_wrappers, id, i)
      )
    )
  }
}
#' @title Wraps matrix inputs in conditional panels
#' @description Make sure columns containing matrix inputs above N.min will be hidden and reactive to N slider.
#' @param i current iteration of function number, symbolises species/column number
#' @param id namespace passed in by initial call to input_UI() in ui.R keep all the ids properly named using ns()
#' @param irow current row number, used with i do determin species interaction this input defines.
build_interaction_wrappers <- function(i, id, irow){
  ns <- NS(id)
  if(i <= N.min){
    build_interaction_input(i, id, irow)
  } else {
    conditionalPanel(
      condition = sprintf("input['%s'] > %i", ns("N"), i-1),
      build_interaction_input(i, id, irow)
    )
  }
}
#' @title Species Input
#' @description Text input which defaults to species/row number. Used to add species names to community diagrams.
#' @param i species/row number
#' @param id namespace passed in by initial call to input_UI() in ui.R keep all the ids properly named using ns()
build_species_input <- function(i, id){
  ns <- NS(id)
  div(
    class = ns("species-col"),
    id = ns(paste0("species-col-N",i)),
    title = paste("Species", i, sep = " "),
    textInput(
      ns(paste("species", i, sep = "-")), 
      label = NULL, 
      width = "100%",
      value = i,
      placeholder = i
    ))
}
#' @title Trophic levels input
#' @description Dropdown inputs for setting trophic levels of each species. Used to arrange spcies in the community diagram.
#' @param i species/row number
#' @param id namespace passed in by initial call to input_UI() in ui.R keep all the ids properly named using ns()
#' @param N reactive to N slider input, so choices never go above size of matrix selected.
build_trophic_input <- function(i, id, N){
  ns <- NS(id)
  div(
    class = ns("trophic-col"),
    id = ns(paste0("trophic-col-N", i)),
    title = paste("Trophic Level for species", i, sep = " "),
    selectInput(
      ns(paste("trophic", i, sep = "-")),
      label = NULL, 
      choices = 1:N, 
      selected = 0,
      multiple = F, 
      selectize = T, 
      width = "100%"
    ))
}
#' @title Species Interaction Input
#' @description Dropdown inputs for interaction codes which will be evaluated into numerical data for output. Designed to be run repeatedly from for loop or apply function. Uses both row and column numbers to determine the species interaction this input defines.
#' @param i species/column number
#' @param id namespace passed in by initial call to input_UI() in ui.R keep all the ids properly named using ns()
#' @param irow species/row number
build_interaction_input <- function(i, id, irow){
  ns <- NS(id)
  div(
    class = if(i==irow) ns("diagonal-col") else ns("col"),
    id = ns(paste0("col-N",i)),
    title = paste(i, "vs", irow, sep = " "),
    selectInput(
      ns(paste("N", i, irow, sep = "-")), 
      label = NULL, 
      # make sure intra-specific competiton, "C", is only available on the diagonal
      choices = if(i==irow) c(0,"C") else c(0,"c","P","p","m"), 
      selected = 0, 
      multiple = F, 
      selectize = T, 
      width = "100%"
    ))
}
