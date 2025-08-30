# practical specific function/s

# colour
brand.primary = "#006635"
brand.secondary = "#6E2C6B"
brand.tertiary = "#ed8231"
# plot style
plotstyle <- function(){
  par(
    pch = 18, 
    bty = "L", 
    pty = "m", 
    cex = 1.2, 
    lwd = 1, 
    las = 1, 
    family = "sans", 
    font.axis = 1, 
    mar = c(5,8,2,2), 
    mgp = c(3, 1, 0),
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