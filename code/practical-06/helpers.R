# colour
brand.primary = "#006635"
brand.secondary = "#6E2C6B"
brand.tertiary = "#ed8231"
# plot style
plotstyle <- function(){
  par(
    pch = 18, 
    bty = "n", 
    pty = "m", 
    cex = 1.2, 
    lwd = 2, 
    las = 1, 
    family = "sans", 
    font.axis = 1, 
    mar = c(5,8,2,2), 
    mgp = c(3,1,0),
    tck = -0.015
  )
}
# legend position
position_legend <- function(pos = "topright"){
  return(list(
    "pos" = pos
  ))
}
# set negative values, NaNs and NAs to zero
fix_numbers <- function(v){
  v[is.na(v)] <- 0
  v[is.nan(v)] <- 0
  v[v < 0] <- 0
  return(v)
}
# Progress messages
loading_progress <- function(message = "Loading...", steps = 10){
  withProgress(message=message, value = 0, {
    # Number of times we'll go through the loop
    n <- steps
    dat <- data.frame(1:steps)
    for (i in 1:n) {
      # Each time through the loop, add another row of data. This is
      # a stand-in for a long-running computation.
      dat <- cbind(dat, rnorm(1:steps))
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Doing part", i))
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
    }
  })
}