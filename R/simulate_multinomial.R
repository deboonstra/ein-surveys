# The function of this script file is to create an R function to generate
# multionomial data.
simulate_multinomial <- function(n, size, prob) {
  # checking parameter types ####
  if (n %% floor(n) != 0) {
    stop("n must be an integer specifying number of simulations.")
  }
  if (all(size %% floor(size) != 0)) {
    stop("size must be integer valued specifying the sample size.")
  }
  if (!(all(prob >= 0) || all(prob <= 1))) {
    stop("probs must contain numeric values between 0 and 1.")
  }

  # Generating samples based on sample sizes ####

  ## Observational matrix of counts ####
  counts <- stats::rmultinom(n = n, size = size, prob = prob)

  ## Estimated probabilities ####
  p_hat <- counts / size

  ## Standard errors of probabilities
  se <- sqrt((p_hat * (1 - p_hat)) / size)

  ## Adding names to outputs ####
  row.names(counts) <- paste0("catg", seq(1, nrow(counts), by = 1))
  if (ncol(counts) > 1) {
    colnames(counts) <- paste0("responses", seq(1, ncol(counts), by = 1))
  } else {
    colnames(counts) <- "responses"
  }
  row.names(p_hat) <- row.names(counts)
  row.names(se) <- row.names(counts)
  colnames(p_hat) <- colnames(counts)
  colnames(se) <- colnames(counts)

  ## Real-world data ####
  dat <- data.frame(matrix(data = NA, nrow = size, ncol = n))
  colnames(dat) <- colnames(counts)
  for (j in seq_len(ncol(dat))) {
    dat[, j] <- factor(
      x = rep(x = row.names(counts), times = counts[, j]),
      levels = row.names(counts), labels = row.names(counts)
    )
  }

  return(list(data = dat, counts = counts, p_hat = p_hat, se = se))
}