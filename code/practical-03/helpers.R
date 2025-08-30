# colour
brand.primary = "#006635"
brand.secondary = "#6E2C6B"
brand.tertiary = "#ed8231"
# plot style
plotstyle <- function(){
  par(
    pch = 18, 
    cex = 1.2, 
    lwd = 1, 
    las = 1, 
    family = "sans", 
    font.axis = 1, 
    tck = -0.015,
    mfrow = c(1, 1),
    bty = "n",
    pty = "m"
  )
}
# practical specific function/s
levins_model <-  function(c, e){
  Prop <- 1:100/100
  cProp <- c*Prop*(1-Prop)
  eProp <- e*Prop
  Pstar <- 1-e/c
  
  Prop.matrix <- matrix(Prop, nrow=100, ncol=1)
  colnames(Prop.matrix) <- "Proportion of sites occupied"
  cProp.matrix <- matrix(cProp, nrow=100, ncol=1)
  colnames(cProp.matrix) <- "Colonization rate (c)"
  eProp.matrix <- matrix(eProp, nrow=100, ncol=1)
  colnames(eProp.matrix) <- "Extinction rate (e)"
  matrix <- cbind(Prop.matrix, cProp.matrix, eProp.matrix)
  
  return(list(
    "Prop" = Prop,
    "cProp" = cProp,
    "eProp" = eProp,
    "Pstar" = Pstar,
    "matrix" = matrix
  ))
}

migration_probability <- function(N.patch, m){
  # Set up migration.
  # Assume that patches are arranged linearly.
  # The frequency of migration between two patches is inversely dependent on the distance between them.
  M <- m/(1+abs(outer(1:N.patch, 1:N.patch, "-")))
  
  matrix <- M
  rownames(matrix) <- 1:N.patch
  colnames(matrix) <- 1:N.patch
  file <- cbind(matrix(1:N.patch, ncol=1, nrow=N.patch), M)
  colnames(file) <- 0:N.patch
  
  return(list(
    "N.patch" = N.patch,
    "M" = M,
    "matrix" = matrix,
    "file" = file
  ))
}

patch_projection <- function(N.patch, max.time, m, r, r_sd, K, K_sd){
  K <- (seq(max(1, K-K_sd), K+K_sd, length = N.patch)) # Carrying Capacity
  r <- (seq(r-r_sd, r+r_sd, length = N.patch)) # Intrinsic Growth Rate. Normally distributed, meaning that some will be negative
  # initial population sizes for each patch. 
  # (Random numbers, uniformly distributed between K/2 and 3*K/2)
  N0 <- round(runif(N.patch, K/2, 3*K/2))
  # Set up migration.
  # Assume that patches are arranged linearly.
  # The frequency of migration between two patches is inversely dependent on the distance between them.
  m <- ifelse(m==0, 0.0001, m) # don't let m = 0, it causes errors. 
  M <- m/(1+abs(outer(1:N.patch, 1:N.patch, "-")))
  # Set up a place to hold population projections, as well as the number of migrants
  N <- matrix(0, nrow = N.patch, ncol = max.time)
  immigrants <- emigrants <- matrix(NA, nrow = N.patch, ncol = max.time)
  N[,1] <- N0 # Initialize population sizes
  
  # Run the model
  # error-checking to make sure that the input data is sett up correctly. 
  # (This give NO output unless an error is detected.)
  stopifnot(all.equal(N.patch, length(r), length(K), length(N0), nrow(M), ncol(M))) 
  for(t in 1:(max.time-1)){
    # each sub-population grows according to a logistic growth model
    N[,t+1] <- (N[,t]+N[,t]*r*(1-(N[,t]/K)))
    N[,t+1] <- ifelse(N[,t+1] < 1, 0, N[,t+1]) # if the population has gone extinct, hold it at 0. 
    # Start migration 1/2 way through model run.
    if(t>=max.time/2){
      # how many individuals leave each population?
      emigrants[,t+1] <- (colSums(M)*N[,t+1])    
      # Where do they go?
      immigrants[,t+1] <- ((M/rowSums(M)) %*% emigrants[,t+1]) 
      # Transfer the migrants
      N[,t+1] <- N[,t+1] - emigrants[,t+1] + immigrants[,t+1] 
    }
  }
  migration <- immigrants - emigrants 
  row.names <- c(
    paste0("N-",    1:N.patch),
    paste0("Em-",   1:N.patch),
    paste0("Imm-",  1:N.patch),
    paste0("Migr-", 1:N.patch)
  )
  matrix <- round(rbind(N, emigrants, immigrants, migration), 2)
  matrix <- cbind(row.names, matrix)
  colnames(matrix) <- c("Time", 1:max.time)

  return(list(
    "N.patch" = N.patch,
    "max.time" = max.time,
    "N" = N,
    "K" = K,
    "r" = r,
    "emigrants"  = emigrants,
    "immigrants" = immigrants,
    "matrix"     = matrix
  ))
}