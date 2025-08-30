run_model <-
function(plot_frequency = 100){
  initial_setup() # ESTABLISH initial distribution of prey and predators
  for (t in 2:max.time) {
    if (plot_frequency != 0 && t%%plot_frequency == 0) {
      plotNPpos(t)
    }
    # Plot current positions. If model is running slowly, increase interval, or do not specify an interval. Or, to make the model run much faster, comment out this line. THis will disable plotting altogether, but greatly accelerate the speed at which the model runs
  #	text(Re(Npos), Im(Npos), 1:length(Npos), pos = 4)
  #	points(Npos[which_dodge], col = 'green')
  #	points(Npos[closest_prey], col = 'blue', cex = 2)
  Nmover(t)                                        # This Function causes the prey to move
  Pmover(t)                                        # This Function causes the predators to move
  Pkiller(t)                                       # Mortality to predators
  if (out$P[t] <= 0) {cat("\n\n*** Predator extinct ***\n\n"); return(out)} # Stop if predator goes extinct
  Nkiller(t)                                       # Mortality to prey
  if (out$N[t] <= 0) {cat("\n\n*** Prey extinct ***\n\n");	 return(out)} # Stop if prey goes extinct
  Nreproducer(t)                                   # Prey reproduction - density dependent
  Preproducer(t)                                   # Predator reproduction
  if(t %% 100 == 0)cat(paste0(t, " "))
  }
  if(t %% 100 == 0)cat("\n")

  out <- out[!is.na(out$N) & !is.na(out$P), ]  # cut down the output file, if one population went extinct.
  return(out)
}
