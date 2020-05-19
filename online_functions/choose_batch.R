function(XY, entropy, candidate, rho, nu, batch_size) {
  # XY : candidate locations where the batch is choosen from
  # entropy: entropy computed at the locations XY
  # candidate: Logical whether the location is a candidate for selection
  # rho, nu: Matern covariance parameters of the model
  # batch_size: number of observations in the batch
  candidate_idx <- which(candidate==TRUE)
  XY <- XY[candidate==TRUE,]
  entropy <- entropy[candidate==TRUE]
  XY_ix <- seq(entropy) # Index of potential locations
  ix_max <- which.max(entropy)  # Location with highest entropy
  XY_ix_lo <- XY_ix[-c(ix_max)] # Locations after removing max entropy
  ix_batch <- c(ix_max) # Batch of locations to survey next
  step <- 2
  while (length(ix_batch) < batch_size) {
    tradeoff <- c()
    for (ix_ in XY_ix_lo) {
      new_batch <- c(ix_batch, ix_)
      new_XY <- XY[new_batch, ]
      Kxx <- spaMM::MaternCorr(d = as.matrix(proxy::dist(new_XY)), rho = rho, nu = nu)
      hlogD <- .5 * determinant(Kxx, logarithm = TRUE)$modulus
      tradeoff <- c(tradeoff, entropy[ix_] + sqrt(log(step)) * hlogD)
    }
    relative_max <- which.max(tradeoff)  # Location with highest entropy
    ix_batch <- c(ix_batch, XY_ix_lo[relative_max])
    XY_ix_lo <- XY_ix_lo[-c(relative_max)]
    step <- step + 1
  }
  
  # If we want as output the indices of locations in XY return this
  return(candidate_idx[ix_batch])
  
}