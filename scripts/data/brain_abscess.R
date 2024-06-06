# The function of this script file is to import and clean the Brain Abscess
# spread sheet from Data for saturation point analyses_2024_05_16.xlsx

# Loading libraries and functions
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Import data ###
brain <- readxl::read_xlsx(
  path = "./data-raw/Data for saturation point analyses_2024_05_16.xlsx",
  sheet = "Brain Abscess",
  range = "A1:Z552"
)

# Cleaning data based on question numbering system ####

## Question 1 ####

### Subsetting the data ####
q1 <- subset(
  x = brain,
  select = c(EIN_ID, Q1)
)

### Renaming the variables ####
colnames(q1) <- c("EIN_ID", "q1")

### Turning default value to missing value ####
q1$q1 <- ifelse(test = q1$q1 == "default", yes = NA, no = q1$q1)

### Turning responses into factor variable ####
q1$q1 <- factor(
  x = q1$q1,
  levels = c("None", "LTone", "One-5", "Six-10", "GT10"),
  labels = c("None", "<1", "1-5", "6-10", ">10")
)

## Question 2 ####

### Subsetting the data ####
q2 <- subset(
  x = brain,
  select = c(EIN_ID, Q2a, Q2b, Q2b_other, Q2c, Q2c_other, Q2d, Q2d_other)
)

### Renaming the variables ####
colnames(q2) <- gsub(pattern = "Q", replacement = "q", x = colnames(q2))

### Turning default value to missing value ####
q2 <- within(
  data = q2,
  expr = {
    q2a <- ifelse(test = q2a == "default", yes = NA, no = q2a)
    q2b <- ifelse(test = q2b == "default", yes = NA, no = q2b)
    q2c <- ifelse(test = q2c == "default", yes = NA, no = q2c)
    q2d <- ifelse(test = q2d == "default", yes = NA, no = q2d)
  }
)

### Turning responses into factor variables ####
q2 <- within(
  data = q2,
  expr = {
    q2a <- factor(
      x = q2a,
      levels = c("Now", "Twenty4Hrs", "Seventy2Hrs", "Moretime"),
      labels = c("Right away", "<24 hours", "<72 hours", "Longer")
    )
    q2b <- factor(
      x = q2b,
      levels = c(
        "ThirdFlagyl", "ThirdFlagyVanc", "FourthFlagyl", "FourthFlaglyVanc",
        "Meropenem", "MeroVanc", "Other"
      ),
      labels = c(
        "3rd gen cephalosporin + metronidazole",
        "3rd gen cephalosporin + metronidazole + vancomycin",
        "4th gen cephalosporin + metronidazole",
        "4th gen cephalosporin + metronidazole + vancomycin",
        "Meropenem",
        "Meropenem + vancomycin",
        "Other"
      )
    )
    q2c <- factor(
      x = q2c,
      levels = c("None", "Qweek", "Q2wks", "Txend", "Other"),
      labels = c(
        "None", "Every week", "Every 2 weeks", "End of treatment only", "Other"
      )
    )
    q2d <- factor(
      x = q2d,
      levels = c("Four", "Six", "Eight", "Other"),
      labels = c("4-5 weeks", "6-8 weeks", ">8 weeks", "Other")
    )
  }
)

### Subsetting the data ####
q2 <- subset(
  x = q2,
  select = c(EIN_ID, q2a, q2b, q2c, q2d)
)

## Question 3 ####

### Subsetting the data ####
q3 <- subset(
  x = brain,
  select = c(EIN_ID, Q3a, Q3b, Q3c, Q3d, Q3e)
)

### Renaming the variables ####
colnames(q3) <- gsub(pattern = "Q", replacement = "q", x = colnames(q3))

### Turning default value to missing ####
for (j in 2:ncol(q3)) {
  for (i in seq_len(nrow(q3))) {
    if (q3[i, j] == "default") {
      q3[i, j] <- NA
    } else {
      q3[i, j] <- q3[i, j]
    }
  }
}

### Turning responses to factor variables ####
q3 <- within(
  data = q3,
  expr = {
    q3a <- factor(
      x = q3a,
      levels = c("Not", "Slight", "Some", "Very"),
      labels = c(
        "Not important", "Slightly important", "Moderately important",
        "Very important"
      )
    )
    q3b <- factor(
      x = q3b,
      levels = c("Not", "Slight", "Some", "Very"),
      labels = c(
        "Not important", "Slightly important", "Moderately important",
        "Very important"
      )
    )
    q3c <- factor(
      x = q3c,
      levels = c("Not", "Slight", "Some", "Very"),
      labels = c(
        "Not important", "Slightly important", "Moderately important",
        "Very important"
      )
    )
    q3d <- factor(
      x = q3d,
      levels = c("Not", "Slight", "Some", "Very"),
      labels = c(
        "Not important", "Slightly important", "Moderately important",
        "Very important"
      )
    )
    q3e <- factor(
      x = q3e,
      levels = c("Not", "Slight", "Some", "Very"),
      labels = c(
        "Not important", "Slightly important", "Moderately important",
        "Very important"
      )
    )
  }
)

## Question 4 ####

### Subsetting the data ####
q4 <- subset(
  x = brain,
  select = c(EIN_ID, Q4_molec)
)

### Splitting responses ####
q4_split <- strsplit(x =  q4$Q4_molec, split = "|", fixed = TRUE)

### Turning default value to missing value ####
q4_split <- lapply(
  X = q4_split,
  FUN = function(x) {
    x <- ifelse(test = x == "default", yes = NA, no = x)
    return(x)
  }
)

### Initializing binary response variables ####
q4$q4_no_notavail <- rep(x = NA, times = nrow(q4))
q4$q4_no_notchoose <- rep(x = NA, times = nrow(q4))
q4$q4_yes_noresponse <- rep(x = NA, times = nrow(q4))
q4$q4_yes_negcx <- rep(x = NA, times = nrow(q4))
q4$q4_yes_immuno <- rep(x = NA, times = nrow(q4))
q4$q4_yes_all <- rep(x = NA, times = nrow(q4))

### Filling in the responses ####
for (i in seq_along(q4_split)) {
  if (!all(is.na(q4_split[[i]]))) {
    q4$q4_no_notavail[i] <- ifelse(
      test = "Notavail" %in% q4_split[[i]],
      yes = 1, no = 0
    )
    q4$q4_no_notchoose[i] <- ifelse(
      test = "Notchoose" %in% q4_split[[i]],
      yes = 1, no = 0
    )
    q4$q4_yes_noresponse[i] <- ifelse(
      test = "Yes_noresponse" %in% q4_split[[i]],
      yes = 1, no = 0
    )
    q4$q4_yes_negcx[i] <- ifelse(
      test = "Yes_negcx" %in% q4_split[[i]],
      yes = 1, no = 0
    )
    q4$q4_yes_immuno[i] <- ifelse(
      test = "Yes_immuno" %in% q4_split[[i]],
      yes = 1, no = 0
    )
    q4$q4_yes_all[i] <- ifelse(
      test = "Yes_all" %in% q4_split[[i]],
      yes = 1, no = 0
    )
  } else {
    q4$q4_no_notavail[i] <- NA
    q4$q4_no_notchoose[i] <- NA
    q4$q4_yes_noresponse[i] <- NA
    q4$q4_yes_negcx[i] <- NA
    q4$q4_yes_immuno[i] <- NA
    q4$q4_yes_all[i] <- NA
  }
}

### Turning responses into factor variables ####
q4 <- within(
  data = q4,
  expr = {
    q4_no_notavail <- factor(
      x = q4_no_notavail,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
    q4_no_notchoose <- factor(
      x = q4_no_notchoose,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
    q4_yes_noresponse <- factor(
      x = q4_yes_noresponse,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
    q4_yes_negcx <- factor(
      x = q4_yes_negcx,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
    q4_yes_immuno <- factor(
      x = q4_yes_immuno,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
    q4_yes_all <- factor(
      x = q4_yes_all,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  }
)

### Subsetting the data ####
q4 <- q4[, -2]


## Question 5 ####

### Subsetting the data ####
q5 <- subset(
  x = brain,
  select = c(EIN_ID, Q5_which, Q5_Other)
)

### Splitting the responses ####
q5_split <- strsplit(x = q5$Q5_which, split = "|", fixed = TRUE)

### Turning default valut to missing value ####
q5_split <- lapply(
  X = q5_split,
  FUN = function(x) {
    x <- ifelse(test = x == "default", yes = NA, no = x)
    return(x)
  }
)

### Initialzing binary response variables ####
q5$q5_pathogen_pcr <- rep(x = NA, times = nrow(q5))
q5$q5_broad_pcr <- rep(x = NA, times = nrow(q5))
q5$q5_ngs <- rep(x = NA, times = nrow(q5))
q5$q5_other <- rep(x = NA, times = nrow(q5))
q5$q5_notsure <- rep(x = NA, times = nrow(q5))

### Filling in the responses ####
for (i in seq_along(q5_split)) {
  if (!all(is.na(q5_split[[i]]))) {
    q5$q5_pathogen_pcr[i] <- ifelse(
      test = "PathogenPCR" %in% q5_split[[i]],
      yes = 1, no = 0
    )
    q5$q5_broad_pcr[i] <- ifelse(
      test = "BroadPCR" %in% q5_split[[i]],
      yes = 1, no = 0
    )
    q5$q5_ngs[i] <- ifelse(
      test = "NGS" %in% q5_split[[i]],
      yes = 1, no = 0
    )
    q5$q5_other[i] <- ifelse(
      test = "Other" %in% q5_split[[i]],
      yes = 1, no = 0
    )
    q5$q5_notsure[i] <- ifelse(
      test = "Notsure" %in% q5_split[[i]],
      yes = 1, no = 0
    )
  } else {
    q5$q5_pathogen_pcr[i] <- NA
    q5$q5_broad_pcr[i] <- NA
    q5$q5_ngs[i] <- NA
    q5$q5_other[i] <- NA
    q5$q5_notsure[i] <- NA
  }
}

### Turning the responses into factor variables ####
q5 <- within(
  data = q5,
  expr = {
    q5_pathogen_pcr <- factor(
      x = q5_pathogen_pcr,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
    q5_broad_pcr <- factor(
      x = q5_broad_pcr,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
    q5_ngs <- factor(
      x = q5_ngs,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
    q5_other <- factor(
      x = q5_other,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
    q5_notsure <- factor(
      x = q5_notsure,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  }
)

### Subsetting the data ####
q5 <- q5[, -c(2, 3)]

## Question 6 ###

### Subsetting the data ####
q6 <- subset(
  x = brain,
  select = c(EIN_ID, Q6_earlypo, Q6_when)
)

### Combining the variables to together ####
q6$q6 <- rep(x = NA, times = nrow(q6))
for (i in seq_len(nrow(q6))) {
  if (q6$Q6_earlypo[i] == "default") {
    q6$q6[i] <- NA
  } else if (q6$Q6_earlypo[i] == "No") {
    q6$q6[i] <- "No"
  } else if (q6$Q6_earlypo[i] == "Yes" && q6$Q6_when[i] != "default") {
    q6$q6[i] <- paste0("Yes, ", q6$Q6_when[i])
  } else {
    q6$q6[i] <- NA
  }
}

### Turning responses into factor variable ####
q6$q6 <- factor(
  x = q6$q6,
  levels = c("No", "Yes, One-2", "Yes, Three-4", "Yes, GT4"),
  labels = c("No", "Yes, 1-2 weeks", "Yes, 3-4 weeks", "Yes, >4 weeks")
)

### Subsetting the data ###
q6 <- subset(x = q6, select = c(EIN_ID, q6))

## Question 7 ####

### Subsetting the data ####
q7 <- subset(
  x = brain,
  select = c(EIN_ID, Q7)
)

### Renaming the variables ####
colnames(q7) <- c("EIN_ID", "q7")

#### Turning default value to missing value ####
q7$q7 <- ifelse(test = q7$q7 == "default", yes = NA, no = q7$q7)

### Turning responses into factor variable ####
q7$q7 <- factor(
  x = q7$q7,
  levels = c("No", "Yes"),
  labels = c("No", "Yes")
)

## Question 8 ####

### Subsetting the data ####
q8 <- subset(
  x = brain,
  select = c(EIN_ID, Q8)
)

### Renaming the variables ####
colnames(q8) <- c("EIN_ID", "q8")

#### Turning default value to missing value ####
q8$q8 <- ifelse(test = q8$q8 == "default", yes = NA, no = q8$q8)

### Turning responses into factor variable ####
q8$q8 <- factor(
  x = q8$q8,
  levels = c("Not", "Slight", "Moderate", "Very"),
  labels = c("Not at all", "Slightly", "Moderately", "Very")
)

## Question 9 ####

### Subseting the data ####
q9 <- subset(
  x = brain,
  select = c(EIN_ID, Q9specify)
)

### Renaming the variables ####
colnames(q9) <- c("EIN_ID", "q9")

### Turning default value to missing ####
q9$q9 <- ifelse(test = q9$q9 == "default", yes = NA, no = q9$q9)

### Creating binary variable indicating a response ####
q9$q9 <- as.numeric(!is.na(q9$q9))

### Turning response indicator variable into a factor variable ####
q9$q9 <- factor(
  x = q9$q9,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

## Question 10 ####

### Subsetting the data ####
q10 <- subset(
  x = brain,
  select = c(EIN_ID, Q_10Comments)
)

### Renaming the variables ####
colnames(q10) <- c("EIN_ID", "q10")

### Turning default value to missing ####
q10$q10 <- ifelse(test = q10$q10 == "default", yes = NA, no = q10$q10)

### Creating binary variable indicating a response ####
q10$q10 <- as.numeric(!is.na(q10$q10))

### Turning response indicator variable into a factor variable ####
q10$q10 <- factor(
  x = q10$q10,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
## Creating wave variable ####

### Subsetting the data ####
wave <- subset(
  x = brain,
  select = c(EIN_ID, BrainAbsc_date)
)

### Creating date only variable ####
wave$date <- as.Date(wave$BrainAbsc_date)

### Initializing wave dates ####
wave_dates <- as.Date(
  x = c("2023-09-07", "2023-09-14", "2023-09-21"),
  format = "%Y-%m-%d"
)

### Determine wave of response ####
wave$wave <- rep(x = NA, times = nrow(wave))
for (i in seq_along(wave$date)) {
  if (wave$date[i] >= wave_dates[1] && wave$date[i] < wave_dates[2]) {
    wave$wave[i] <- 1
  } else if (wave$date[i] >= wave_dates[2] && wave$date[i] < wave_dates[3]) {
    wave$wave[i] <- 2
  } else if (wave$date[i] >= wave_dates[3]) {
    wave$wave[i] <- 3
  } else {
    wave$wave[i] <- NA
  }
}

### Subsetting the data ####
wave <- subset(
  x = wave,
  select = c(EIN_ID, wave)
)

## Combining question-based data sets ####
questions <- dplyr::bind_cols(
  wave, q1[, -1], q2[, -1], q3[, -1], q4[, -1], q5[, -1], q6[, -1], q7[, -1],
  q8[, -1], q9[, -1], q10[, -1]
)

# Export data ####
saveRDS(
  object = questions,
  file = "./data/brain_abscess.rds"
)