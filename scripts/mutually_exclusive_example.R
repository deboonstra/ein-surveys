# The function of this script file is implement the saturation point method on
# one of the EIN survey questions to provide an example of this method. This
# example will focus on a question that has mutually exclusive responses.

# At this moment, this example is based on the Difficult Diagnoses survey. More
# specifically, question 2 of that survey.

# Loading libraries and functions
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating output directory ####
sub_dir <- "./outputs/mutually-exclusive-example/"
if (!dir.exists(sub_dir)) {
  dir.create(sub_dir)
}

# Importing data ####
diff_diag <- readRDS(file = "./data/difficult_diagnoses.rds")

# Implementing saturation point method ####

## Examining the first wave of responses ####

### Subsetting the data to only include the first wave of responses ####
wave1_data <- subset(x = diff_diag, subset = wave == 1, select = c(wave, q2))

### Examining sample size ####
size1 <- nrow(wave1_data)
cat("Sample size: ", size1, "\n")

### Examining number of responses ####
counts1 <- as.integer(stats::ftable(x = wave1_data$q2))
names(counts1) <- levels(wave1_data$q2)
print(counts1)

### Examining sample proportions ####
props1 <- counts1 / size1
print(props1)

### Examining standard errors ####
ses1 <- sqrt((props1 * (1 - props1)) / size1)
print(ses1)

## Examining the first and second waves of responses

### Subsetting the data to include the first and second waves of respones ####
wave2_data <- subset(
  x = diff_diag,
  subset = wave %in% c(1, 2) & !is.na(q2),
  select = c(wave, q2)
)

### Ordering data ####
wave2_data <- wave2_data[order(wave2_data$wave, wave2_data$q2), ]

#### Examining sample size ####
size2 <- as.integer(c(stats::ftable(x = wave2_data$wave), nrow(wave2_data)))
names(size2) <- c(unique(wave2_data$wave), "Total")
print(size2)

### Examining number of responses ####
counts2 <- matrix(
  data = NA,
  nrow = length(size2), ncol = length(unique(wave2_data$q2)),
  dimnames = list(
    names(size2),
    as.character(unique(wave2_data$q2))
  )
)
counts2[1, ] <- as.integer(
  stats::ftable(x = wave2_data$q2[wave2_data$wave == 1])
)
counts2[2, ] <- as.integer(
  stats::ftable(x = wave2_data$q2[wave2_data$wave == 2])
)
counts2[3, ] <- as.integer(stats::ftable(x = wave2_data$q2))
print(counts2)

### Examining sample proportions ####
props2 <- matrix(
  data = NA,
  nrow = length(size2), ncol = length(unique(wave2_data$q2)),
  dimnames = list(
    names(size2),
    as.character(unique(wave2_data$q2))
  )
)
props2[1, ] <- counts2[1, ] / size2[1]
props2[2, ] <- counts2[2, ] / size2[2]
props2[3, ] <- counts2[3, ] / size2[3]
print(props2)

### Checking for independence before calculating standard errors ####
test2 <- stats::chisq.test(
  x = wave2_data$wave,
  y = wave2_data$q2,
  correct = FALSE
)
print(test2)

### Examining standard errors ####
ses2 <- matrix(
  data = NA,
  nrow = length(size2), ncol = length(unique(wave2_data$q2)),
  dimnames = list(
    names(size2),
    as.character(unique(wave2_data$q2))
  )
)
ses2[1, ] <- sqrt((props2[1, ] * (1 - props2[1, ])) / size2[1])
ses2[2, ] <- sqrt((props2[2, ] * (1 - props2[2, ])) / size2[2])
ses2[3, ] <- sqrt((props2[3, ] * (1 - props2[3, ])) / size2[3])
print(ses2)

### Examining heterogeneity index ####
hetero2 <- colMeans(
  rbind(
    abs(props2[1, ] - props2[3, ]),
    abs(props2[2, ] - props2[3, ])
  )
)
print(hetero2)

## Examining all three waves of responses ####

### Subsetting the data to include the first and second waves of respones ####
wave3_data <- subset(
  x = diff_diag,
  subset = !is.na(q2),
  select = c(wave, q2)
)

### Ordering data ####
wave3_data <- wave3_data[order(wave3_data$wave, wave3_data$q2), ]

#### Examining sample size ####
size3 <- as.integer(c(stats::ftable(x = wave3_data$wave), nrow(wave3_data)))
names(size3) <- c(unique(wave3_data$wave), "Total")
print(size3)

### Examining number of responses ####
counts3 <- matrix(
  data = NA,
  nrow = length(size3), ncol = length(unique(wave3_data$q2)),
  dimnames = list(
    names(size3),
    as.character(unique(wave3_data$q2))
  )
)
counts3[1, ] <- as.integer(
  stats::ftable(x = wave3_data$q2[wave3_data$wave == 1])
)
counts3[2, ] <- as.integer(
  stats::ftable(x = wave3_data$q2[wave3_data$wave == 2])
)
counts3[3, ] <- as.integer(
  stats::ftable(x = wave3_data$q2[wave3_data$wave == 3])
)
counts3[4, ] <- as.integer(stats::ftable(x = wave3_data$q2))
print(counts3)

### Examining sample proportions ####
props3 <- matrix(
  data = NA,
  nrow = length(size3), ncol = length(unique(wave3_data$q2)),
  dimnames = list(
    names(size3),
    as.character(unique(wave3_data$q2))
  )
)
props3[1, ] <- counts3[1, ] / size3[1]
props3[2, ] <- counts3[2, ] / size3[2]
props3[3, ] <- counts3[3, ] / size3[3]
props3[4, ] <- counts3[4, ] / size3[4]
print(props3)

### Checking for independence before calculating standard errors ####
test3 <- stats::chisq.test(
  x = wave3_data$wave,
  y = wave3_data$q2,
  correct = FALSE
)
print(test3)

### Examining standard errors ####
ses3 <- matrix(
  data = NA,
  nrow = length(size3), ncol = length(unique(wave3_data$q2)),
  dimnames = list(
    names(size3),
    as.character(unique(wave3_data$q2))
  )
)
ses3[1, ] <- sqrt((props3[1, ] * (1 - props3[1, ])) / size3[1])
ses3[2, ] <- sqrt((props3[2, ] * (1 - props3[2, ])) / size3[2])
ses3[3, ] <- sqrt((props3[3, ] * (1 - props3[3, ])) / size3[3])
ses3[4, ] <- sqrt((props3[4, ] * (1 - props3[4, ])) / size3[4])
print(ses3)

### Examining heterogeneity index ####
hetero3 <- colMeans(
  rbind(
    abs(props3[1, ] - props3[4, ]),
    abs(props3[2, ] - props3[4, ]),
    abs(props3[3, ] - props3[4, ])
  )
)
print(hetero3)

# Combining outputs to R data objects ####
wave1 <- list(
  size = size1,
  counts = counts1,
  props = props1,
  test = NULL,
  ses = ses1,
  h_index = NULL
)

wave2 <- list(
  size = size2,
  counts = counts2,
  props = props2,
  test = test2,
  ses = ses2,
  h_index = hetero2
)

wave3 <- list(
  size = size3,
  counts = counts3,
  props = props3,
  test = test3,
  ses = ses3,
  h_index = hetero3
)

# Exporting ####
save(
  wave1, wave2, wave3,
  file = paste0(sub_dir, "mutually_exclusive_example.rda")
)