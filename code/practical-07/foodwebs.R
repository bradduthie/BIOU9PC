# set defaults
reps <- 1000 # how many replicate communities to evaluate? 

#' @title Foodwebs server function
#' @description reactive functions and output rendering
#' @param input all the input data
#' @param ouput all the avilable output objects
#' @param session module info - more relevant if reused as multiple ui tabs
foodwebs <- function(input, output, session){
  # get the session name-space set by ui id
  ns = session$ns
  # look for changes to apply to matrix
  foodwebs.matrix.vals <- reactive({
    return(list_matrix_vals(input))
  })
  foodwebs.matrix <- reactive({
    validate(
      need(input$N, "Number of species required.")
    )
    mat <- array(NA, dim = c(input$N, input$N, 100))
    for(i in 1:100){
      mat[,,i] <- build_matrix(list_matrix_vals(input), input$N, names = get_meta(input, "species"))
    }
    mean_mat <- apply(mat, 1:2, mean)
    rownames(mean_mat) <- colnames(mean_mat) <- get_meta(input, "species")
#    browser()
    return(mean_mat)
  })
  # render matrix changes using reactive data
  output$matrix <- renderTable({
    cat(file=stderr(), "attempting matrix render...\n")
    matrix <- foodwebs.matrix()
#    mat <- array()
#    for(i in 1:100){
#    mat[[i]] <- foodwebs.matrix()
#    }
  },
  striped = T,
  rownames = T,
  colnames = T,
  spacing = 'xs',
  digits = 2
  )
  output$matrix.csv <- downloadHandler(
    filename = function() { "evaluated-matrix.csv" },
    content = function(file) {
      write.table(
        foodwebs.matrix(), 
        file, 
        sep = ",", 
        row.names = F, 
        col.names = T
      )
    }
  )
  # look for changes to apply to community diagram
  foodwebs.diagram <- reactive({
    validate(
      need(input$N, "Number of species required.")
    )
    create_community_diagram(
      list_matrix_vals(input), 
      input$N,
      names = get_meta(input, "species"), 
      levels = get_meta(input, "trophic")
    )
  })
  # render community diagram with reactive data
  output$diagram <- renderPlot({
    cat(file=stderr(), "attempting diagram render...\n")
    diagram <- foodwebs.diagram()
  })
  # look for changes to apply to trophic level inputs
  get.N <- reactive({
    validate(
      need(input$N, "Number of species required.")
    )
    return(input$N)
  })
  get.trophic <- reactive({
    return(get_meta(input, "trophic"))
  })
  get.species <- reactive({
    return(get_meta(input, "species"))
  })
  # update trophic inputs with new choices from reactive data
  observe({
    N <- get.N()
    selections <- get.trophic()
    for(i in 1:N){
      # needs to pass tropic input to selected to retain input memory as well expanding choicesa
      updateSelectInput(session, paste("trophic", i, sep = "-"), choices = 1:N, selected = selections[i])
    }
  })
  # look for changes to apply to stability testing
  get.predictions <- reactive({
    validate(
      need(input$N, "Number of species required.")
    )
    vals <- list_matrix_vals(input)
    test_community_stability(vals, input$N, names = get_meta(input, "species"))
  })
  # render stability summary table
  output$summary <- renderTable({
    cat(file=stderr(), "attempting summary table render...\n")
    stability <- results_summary(get.predictions())
    }, 
    rownames = T, 
    digits = 2
  )
  output$summary.csv <- downloadHandler(
    filename = function() { "prediction-summary.csv" },
    content = function(file) {
      write.table(
        results_summary(get.predictions()), 
        file, 
        sep = ",", 
        row.names = T, 
        col.names = F
      )
    }
  )
  # render extinctions summary table
  output$extinctions <- renderTable({
    cat(file=stderr(), "attempting extinctions table render...\n")
    stability <- extinctions_summary(get.predictions(), names = get.species())
    },
    rownames = T, 
    digits = 1
  )
  output$extinctions.csv <- downloadHandler(
    filename = function() { "predicted-species-extinctions.csv" },
    content = function(file) {
      write.table(
        extinctions_summary(get.predictions(), names = get.species()), 
        file, 
        sep = ",", 
        row.names = F, 
        col.names = T
      )
    }
  )
  # render stability testing histogram
  output$stability <- renderPlot({
    cat(file=stderr(), "attempting stability plot render...\n")
    stability <- plot_community_stability(
      get.predictions(), 
      foodwebs.matrix.vals()
    )
  })
  output$stabilitydata <- renderTable({
    cat(file=stderr(), "attempting stability table render...\n")
    stability <- get.predictions()
  #  browser()
    wd <- ifelse(stability$which_dies > 0, T, F)
    colnames(wd) <- paste(colnames(wd), 'dies')
    stabilitydata <- cbind(stability$out, wd)
  },
  rownames = F, 
  digits = 2
  )
  output$stability.csv <- downloadHandler(
    filename = function() { "stability-plot-data.csv" },
    content = function(file) {
      stability <- get.predictions()
#      browser()
      wd <- ifelse(stability$which_dies > 0, T, F)
      colnames(wd) <- paste(colnames(wd), 'dies')
      write.table(
        cbind(stability$out, wd),
        file, 
        sep = ",", 
        row.names = F, 
        col.names = T
      )
    }
  )
}

#' @title List input values as expressions
#' @description Equates each input value to an expression and organises in a list format which can be transfromed into a matrix
#' @param input All the input data from the session.
#' @return a list object, fileld with expressions for each filled input matched by id
list_matrix_vals <- function(input){
#  cat(file=stderr(), "building matrix...\n")
  N <- input$N
  vals <- list()
  # by row...
  for(i in 1:N){
    # and then by column...
    for(j in 1:N){
      # create a named value to match row and col class then check for exisiting input data to match
      # expression statements applied depending on input content, parsed later with eval() so we can 
      # re-evaluate the matirx many times for extinction predictions
      vals[[i + (j-1)*N]] <- switch(input[[paste("N", i, j, sep = "-")]], 
                                    C = expression(-1),
                                    c = expression(runif(1,  -1,  0)),
                                    p = expression(runif(1, -10,  0)),
                                    P = expression(runif(1,   0 , 0.1)),
                                    m = expression(runif(1,   0 , 0.1)),
                                        expression(0))
    }
  }
  # mat <- matrix(lapply(vals, eval), ncol = N, byrow = T)
  return(vals)
}

#' @title Build matrix from list of expressions
#' @description Applies eval() to all vals to run expressions while building matrix of size N
#' @param vals takes output from list_matrix_vals(), or any list of expressions.
#' @param N The dimensions of the matrix to create
#' @param names character defines the colnames and rownames to return. Should be the species names provided by the user. 
#' @param abs logical (default FALSE) defines whether matrix values should be absolute.
#' @return Matrix of size N containing evaluated expressions from supplied list vals.
build_matrix <- function(vals, N, names, abs = F){
  if(abs == T){
    M <- matrix(sapply(lapply(vals, eval), abs), ncol = N, byrow = T)
  } else {
    M <- matrix(sapply(vals, eval), ncol = N, byrow = T)
  }
  rownames(M) <- colnames(M) <- names
  return(M)
}
#' @description retrieves only the input data for the meta data, ie: trophic levels or species names.
#' @param type accepts "trophic" or "species"
get_meta <- function(input, type = trophic){
  N <- input$N
  vals <- list()
  for(i in 1:N){
    vals[[i]] <- input[[paste(type, i, sep = "-")]]
  }
  return(unlist(vals))
}

#' @title Create community foodweb diagram
#' @description Plots a diagram representing the interactions among in the organisms in the ecological community specified by model and N.
create_community_diagram <- function(vals, N, labels = F, levels = NULL, names){
  # set variables
  trophic.levels = if(!is.null(levels)) as.numeric(levels) else 1:N 
  M <- build_matrix(vals, N, names) # with negative vals for line colours
  W <- unlist(build_matrix(vals, N, names, abs = T)) # with absolute vals for line width
  g <- graph.adjacency(M, weighted = T)
  E(g)$color <- ifelse(M[M != 0] < 0, "red", "forestgreen")
  E(g)$width <- 2
  # organise trophic levels
  sp.per.trophic.level <- table(trophic.levels)
  Xpos <-	unlist(sapply(sp.per.trophic.level, FUN = function(x){
      if(x == 1){
        mean(range(sp.per.trophic.level))
      } else{
        seq(1, x, by = 1)
      }
  }))
  # style
  par(
    las = 1, 
    bty = "n", 
    mfrow = c(1, 1)
  )
  # draw diagram
  plot(
    g, 
    edge.loop.angle = pi/2, 
    vertex.size = 10, 
    vertex.color = "white", 
    vertex.shape = "rectangle",
    vertex.width = 20,
    layout = matrix(c(Xpos, trophic.levels), nrow = N),
    edge.curved = 0.4, 
    edge.arrow.size = 1, 
#    edge.label = if(labels == T) sprintf("%1.2f", M[M!= 0]), 
    vertex.label = names,
    asp = 0.36,
    margin = 0
  )
}

#' @title Test community stability
#' @description re-evaluates the matrix repeatedly - default 1000 times - to find which extinctions are likely.
#' @param vals accepts list of expressions to evaluate, especially output from list_matrix_vals()
#' @param N number of species in web, should be reactive to N slider.
#' @param names names of species in web, should be supplied by user.
#' @param n.runs number of times to evaluate the matrix.
#' @return list object $out contains all the data, $which_dies contains predictions
test_community_stability <- function(vals, N, names, n.runs = reps) {
  M <- build_matrix(vals, N, names)
  out <- data.frame(runs = 1:n.runs, return_time = NA, stable = NA)
  which_dies <- matrix(0, ncol = N, nrow = n.runs)
  colnames(which_dies) = names
  for (run in 1:n.runs) {
    M <- build_matrix(vals, N, names)
    eigens <- Re(eigen(M)$values)
    neg.eigens <- eigens[eigens < 0]
    out$stable[run] <- length(neg.eigens) == N
    out$return_time[run] <- -1/max(neg.eigens)
    temp <- eigens == max(eigens) & eigens >= 0
    if(sum(temp) > 0){which_dies[run,] <- temp/sum(temp)}
  }
#    browser()
  return(list(
    out = out, 
    which_dies = which_dies
  ))
}

#' @title Plot community stability
#' @description Plots histogram of community stability predictions.
#' @param test output from test_community_stability()
#' @param vals accepts list of expressions to evaluate, especially output from list_matrix_vals()
#' @param n.runs number of times to evaluate the matrix.
#' @return list object $out contains all the data, $which_dies contains predictions
plot_community_stability <- function(test, vals, n.runs = reps) {
  op <- par(las = 1, bty = "n", xpd = F, tcl = 0.2)
  stable   <- test$out[test$out$stable == T, ]
#browser()
    if(nrow(stable)>0){
  hist(
    log10(stable$return_time),
    breaks = max(10, nrow(stable)/10), 
    main = "", 
    xlab = "Return time", 
    border = NA, 
    col = "gray",
    xaxt = 'n'
  )
  abline(v = mean(log10(stable$return_time)), lwd = 2, col = 'red')
  abline(v = quantile(log10(stable$return_time), c(0.25, 0.5, 0.75)), lwd = 2, lty = c(2, 1, 2), col = 'black')
  axis(1, 
    at     =     0:max(round(log10(stable$return_time))), 
    labels = 10^(0:max(round(log10(stable$return_time)))))
  } else {
    plot.new()
    mtext(paste("0 of", n.runs, "runs were stable, so no histogram of return types can be generated."), col = 'red', cex = 1.2)
  }
  par(op)
}

#' @title Results summary
#' @description Summarises the stability of the food web
#' @param test output from test_community_stability()
#' @return data.frame containing a summary of foodweb stability
results_summary <- function(test){
  input <- test$out
  stable   <- input[input$stable == T, ]
  unstable <- input[input$stable == F, ]
  summ <- data.frame(
    value = c(quantile(stable$return_time, 0.25), 
              median(stable$return_time), 
              quantile(stable$return_time, 0.75),
              length(stable$return_time[stable$return_time > 100])/nrow(stable) *100,
              length(unstable$return_time)/nrow(input) *100
              ))
  rownames(summ) <- c("25th percentile of return time", "Median return time", "75th percentile of return time", "Percent of return time > 100", "Percent of runs in which extinction(s) occurred")
  return(summ)
}

#' @title Extinctions summary
#' @description Summarises the extinctions that occur in the food web
#' @param test output from test_community_stability()
#' @return data.frame containing a summary of foodweb stability
extinctions_summary <- function(test, names){
  wd  <- data.frame(`Percentage of runs in which extinction occurred` = 100*colSums(test$which_dies)/nrow(test$which_dies))
  rownames(wd) <- names
  return(wd)
}
