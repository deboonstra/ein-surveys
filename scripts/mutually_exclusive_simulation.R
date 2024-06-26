# The function of this script file is to run a simulation exploring saturation
# point for a mutually exclusive question.

# There are three settings to investigate in these simulations.
# 1. We achieve saturation with our first collection of data.
# 2. We need a second collection of data to achieve saturation; however, there
# IS NO response bias. Thus, the collected data can be seen as one data
# collection mechanism.
# 3. We need a second collection of data to achieve saturation; however, there
# response bias IS PRESENT. Thus, the calculation of standard errors must
# account for the different data collections.

# Loading libraries and functions
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating output directory ####
sub_dir <- "./outputs/mutually-exclusive-simulation/"
if (!dir.exists(sub_dir)) {
  dir.create(sub_dir)
}

# Simulations ####

## Simulation parameters ####

### Number of simulations
n_sims <- 1L

### Sample size
size1 <- 50L
size2 <- 50L

### Generating probabilities for first data collection
prob1 <- rep(x = 0.2, times = 5)

### Generating probabilities for second data collection, where there is NO
## response bias
prob2 <- rep(x = 0.2, times = 5)

### Generating probabilities for second data collection, where there IS
## response bias present
prob2_rb <- c(0.3, 0.3, 0.2, 0.1, 0.1)

### Significance level for test of independence ####
alpha <- 0.05

## Simulation 1 ####
set.seed(1)
cat("\nSimulation 1\n")
sim1 <- simulate_multinomial(n = n_sims, size = size1 + size2, prob = prob1)

### Review simulation results ####

### Counts ####
cat("\nCounts:\n")
counts1 <- matrix(
  data = t(sim1$counts),
  nrow = 1, ncol = nrow(sim1$counts), byrow = TRUE,
  dimnames = list("", row.names(sim1$counts))
)
print(counts1)

### Sample proportions ####
cat("\nSample proportions:\n")
props1 <- matrix(
  data = t(sim1$p_hat),
  nrow = 1, ncol = nrow(sim1$p_hat), byrow = TRUE,
  dimnames = list("", row.names(sim1$p_hat))
)
print(props1)

### Standard errors ####
cat("\nStandard errors:\n")
ses1 <- matrix(
  data = t(round(sim1$se, digits = 3)),
  nrow = 1, ncol = nrow(sim1$se), byrow = TRUE,
  dimnames = list("", row.names(sim1$se))
)
print(ses1)

## Simulation 2 ####
set.seed(2)
cat("\n\nSimulation 2\n")

### Wave 1 of data collection ####
sim2_wave1 <- simulate_multinomial(n = n_sims, size = size1, prob = prob1)

### Wave 2 of data collection ####
sim2_wave2 <- simulate_multinomial(n = n_sims, size = size2, prob = prob2)

### Checking for independence #####
cat("\nCounts by wave:\n")
counts2_wave <- matrix(
  data = c(sim2_wave1$counts, sim2_wave2$counts),
  nrow = 2, ncol = nrow(sim2_wave1$counts), byrow = TRUE,
  dimnames = list(
    c("Wave: 1", "Wave: 2"),
    row.names(sim2_wave1$counts)
  )
)
print(counts2_wave)

cat("\nSample proportions by wave:\n")
props2_wave <- matrix(
  data = round(c(sim2_wave1$p_hat, sim2_wave2$p_hat), digits = 3),
  nrow = 2, ncol = nrow(sim2_wave1$p_hat), byrow = TRUE,
  dimnames = list(
    c("Wave: 1", "Wave: 2"),
    row.names(sim2_wave1$p_hat)
  )
)
print(props2_wave)

cat("\nStandard errors by wave:\n")
ses2_wave <- matrix(
  data = round(c(sim2_wave1$se, sim2_wave2$se), digits = 3),
  nrow = 2, ncol = nrow(sim2_wave1$se), byrow = TRUE,
  dimnames = list(
    c("Wave: 1", "Wave: 2"),
    row.names(sim2_wave1$se)
  )
)
print(ses2_wave)

cat("\nChecking for independence\n")
wave2 <- c(
  rep(x = 1, times = nrow(sim2_wave1$data)),
  rep(x = 2, times = nrow(sim2_wave2$data))
)
responses2 <- c(
  sim2_wave1$data$responses,
  sim2_wave2$data$responses
)
test2 <- stats::chisq.test(x = wave2, y = responses2, correct = FALSE)
print(test2)
if (test2$p.value > alpha) {
  cat("Conclusion: Collapse to one sample without adjusting wave. \n")
} else {
  cat("Conclusion: Collapse to one sample while adjusting wave. \n")
}

### Collapsing simulations to one data collection if independence present ####

#### Initializing final simulation collection ####
sim2 <- vector(mode = "list", length = length(sim2_wave1))
names(sim2) <- names(sim2_wave1)

#### Combining real-world data ####
sim2$data <- data.frame(wave = wave2, responses = responses2)

#### Combining multinomial counts ####
sim2$counts <- matrix(
  data = NA,
  nrow = nrow(sim2_wave1$counts),
  ncol = ncol(sim2_wave1$counts),
  dimnames = list(
    row.names(sim2_wave1$counts),
    colnames(sim2_wave1$counts)
  )
)

for (j in seq_len(ncol(sim2$counts))) {
  sim2$counts[, j] <- sim2_wave1$counts[, j] + sim2_wave2$counts[, j]
}

cat("\nCounts after collapsing:\n")
counts2 <- matrix(
  data = t(sim2$counts),
  nrow = 1, ncol = nrow(sim2$counts), byrow = TRUE,
  dimnames = list("", row.names(sim2$counts))
)
print(counts2)

#### Calculating the sample proportions for the collective data ####
sim2$p_hat <- matrix(
  data = NA,
  nrow = nrow(sim2_wave1$p_hat),
  ncol = ncol(sim2_wave1$p_hat),
  dimnames = list(
    row.names(sim2_wave1$p_hat),
    colnames(sim2_wave1$p_hat)
  )
)
# collective sample size
size2_total <- size1 + size2
sim2$p_hat <- sim2$counts / size2_total

cat("\nSample proportions after collapsing:\n")
props2 <- matrix(
  data = round(t(sim2$p_hat), digits = 3),
  nrow = 1, ncol = nrow(sim2$p_hat), byrow = TRUE,
  dimnames = list("", row.names(sim2$p_hat))
)
print(props2)

#### Calculating the standard errors for the collective data ####
sim2$se <- matrix(
  data = NA,
  nrow = nrow(sim2_wave1$se),
  ncol = ncol(sim2_wave1$se),
  dimnames = list(
    row.names(sim2_wave1$se),
    colnames(sim2_wave1$se)
  )
)
sim2$se <- sqrt((sim2$p_hat * (1 - sim2$p_hat)) / size2_total)

cat("\nStandard errors after collapsing:\n")
ses2 <- matrix(
  data = round(t(sim2$se), digits = 3),
  nrow = 1, ncol = nrow(sim2$se), byrow = TRUE,
  dimnames = list("", row.names(sim2$se))
)
print(ses2)

## Simulation 3 ####
set.seed(3)
cat("\n\nSimulation 3\n")

### Wave 1 of data collection ####
sim3_wave1 <- simulate_multinomial(n = n_sims, size = size1, prob = prob1)

### Wave 2 of data collection ####
sim3_wave2 <- simulate_multinomial(n = n_sims, size = size2, prob = prob2_rb)

### Checking for independence #####
cat("\nCounts by wave:\n")
counts3_wave <- matrix(
  data = c(sim3_wave1$counts, sim3_wave2$counts),
  nrow = 2, ncol = nrow(sim3_wave1$counts), byrow = TRUE,
  dimnames = list(
    c("Wave: 1", "Wave: 2"),
    row.names(sim3_wave1$counts)
  )
)
print(counts3_wave)

cat("\nSample proportions by wave:\n")
props3_wave <- matrix(
  data = round(c(sim3_wave1$p_hat, sim3_wave2$p_hat), digits = 3),
  nrow = 2, ncol = nrow(sim3_wave1$p_hat), byrow = TRUE,
  dimnames = list(
    c("Wave: 1", "Wave: 2"),
    row.names(sim3_wave1$p_hat)
  )
)
print(props3_wave)

cat("\nStandard errors by wave:\n")
ses3_wave <- matrix(
  data = round(c(sim3_wave1$se, sim3_wave2$se), digits = 3),
  nrow = 2, ncol = nrow(sim3_wave1$se), byrow = TRUE,
  dimnames = list(
    c("Wave: 1", "Wave: 2"),
    row.names(sim3_wave1$se)
  )
)
print(ses3_wave)

cat("\nChecking for independence\n")
wave3 <- c(
  rep(x = 1, times = nrow(sim3_wave1$data)),
  rep(x = 3, times = nrow(sim3_wave2$data))
)
responses3 <- c(
  sim3_wave1$data$responses,
  sim3_wave2$data$responses
)
test3 <- stats::chisq.test(x = wave3, y = responses3, correct = FALSE)
print(test3)
if (test3$p.value > alpha) {
  cat("Conclusion: Collapse to one sample without adjusting wave. \n")
} else {
  cat("Conclusion: Collapse to one sample while adjusting wave. \n")
}

### Collapsing simulations to one data collection while adjusting for wave ####

#### Initializing final simulation collection ####
sim3 <- vector(mode = "list", length = length(sim3_wave1))
names(sim3) <- names(sim3_wave1)

#### Combining real-world data ####
sim3$data <- data.frame(wave = wave3, responses = responses3)

#### Combining multinomial counts ####
sim3$counts <- matrix(
  data = NA,
  nrow = nrow(sim3_wave1$counts),
  ncol = ncol(sim3_wave1$counts),
  dimnames = list(
    row.names(sim3_wave1$counts),
    colnames(sim3_wave1$counts)
  )
)

for (j in seq_len(ncol(sim3$counts))) {
  sim3$counts[, j] <- sim3_wave1$counts[, j] + sim3_wave2$counts[, j]
}

cat("\nCounts after collapsing:\n")
counts3 <- matrix(
  data = t(sim3$counts),
  nrow = 1, ncol = nrow(sim3$counts), byrow = TRUE,
  dimnames = list("", row.names(sim3$counts))
)
print(counts3)

#### Calculating the sample proportions for the collective data ####
sim3$p_hat <- matrix(
  data = NA,
  nrow = nrow(sim3_wave1$p_hat),
  ncol = ncol(sim3_wave1$p_hat),
  dimnames = list(
    row.names(sim3_wave1$p_hat),
    colnames(sim3_wave1$p_hat)
  )
)
# collective sample size
size3_total <- size1 + size2
sim3$p_hat <- sim3$counts / size3_total

cat("\nSample proportions after collapsing:\n")
props3 <- matrix(
  data = round(t(sim3$p_hat), digits = 3),
  nrow = 1, ncol = nrow(sim3$p_hat), byrow = TRUE,
  dimnames = list("", row.names(sim3$p_hat))
)
print(props3)

#### Calculating the standard errors for the collective data ####
sim3$se <- matrix(
  data = NA,
  nrow = nrow(sim3_wave1$se),
  ncol = ncol(sim3_wave1$se),
  dimnames = list(
    row.names(sim3_wave1$se),
    colnames(sim3_wave1$se)
  )
)
w1 <- size1 / (size1 + size2)
w2 <- size2 / (size1 + size2)
sim3$se <- sqrt((w1^2 * sim3_wave1$se^2) + (w2^2 * sim3_wave2$se^2))

cat("\nStandard errors after collapsing:\n")
ses3 <- matrix(
  data = round(t(sim3$se), digits = 3),
  nrow = 1, ncol = nrow(sim3$se), byrow = TRUE,
  dimnames = list("", row.names(sim3$se))
)
print(ses3)

# Exporting simulation results ####
save(
  n_sims, size1, size2, prob1, prob2, prob2_rb, alpha,
  sim1, sim2, sim2_wave1, sim2_wave2, sim3, sim3_wave1, sim3_wave2,
  file = paste0(sub_dir, "mutually_exclusive_simulation.rda")
)