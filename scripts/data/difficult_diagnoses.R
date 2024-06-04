# The function of this script file is to import and clean the Difficult
# Diagnoses spread sheet from Data for saturation point analyses_2024_05_16.xlsx

# Loading libraries and functions
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Import data ###
diff_diag <- readxl::read_xlsx(
  path = "./data-raw/Data for saturation point analyses_2024_05_16.xlsx",
  sheet = "Difficult Diagnoses",
  range = "A1:AC644"
)

# Cleaning data based on question numbering system ####

## Question 1 ####

### Subsetting data ####
q1 <- subset(
  x = diff_diag,
  select = c(EIN_ID, Q1, Q1_other)
)

### Splitting the response to create binary responses ####
q1_split <- strsplit(x = q1$Q1, split = "|", fixed = TRUE)

### Initializing Q1 binary response variables ####
q1$q1_broadrange <- rep(x = NA, times = nrow(q1))
q1$q1_wholegenome <- rep(x = NA, times = nrow(q1))
q1$q1_mngs <- rep(x = NA, times = nrow(q1))
q1$q1_none <- rep(x = NA, times = nrow(q1))
q1$q1_other <- rep(x = NA, times = nrow(q1))

### Filling in the responses ####
for (i in seq_len(nrow(q1))) {
  if (!all(is.na(q1_split[[i]]))) {
    q1$q1_broadrange[i] <- ifelse(
      test = "Broadrange" %in% q1_split[[i]],
      yes = 1, no = 0
    )
    q1$q1_wholegenome[i] <- ifelse(
      test = "Wholegenome" %in% q1_split[[i]],
      yes = 1, no = 0
    )
    q1$q1_mngs[i] <- ifelse(
      test = "MNGS" %in% q1_split[[i]],
      yes = 1, no = 0
    )
    q1$q1_none[i] <- ifelse(
      test = "None" %in% q1_split[[i]],
      yes = 1, no = 0
    )
    q1$q1_other[i] <- ifelse(
      test = "Other" %in% q1_split[[i]],
      yes = 1, no = 0
    )
  } else {
    q1$q1_broadrange[i] <- NA
    q1$q1_wholegenome[i] <- NA
    q1$q1_mngs[i] <- NA
    q1$q1_none[i] <- NA
    q1$q1_other[i] <- NA
  }
}

### Turning binary responses into factor variables ####
q1$q1_broadrange <- factor(
  x = q1$q1_broadrange,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q1$q1_wholegenome <- factor(
  x = q1$q1_wholegenome,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q1$q1_mngs <- factor(
  x = q1$q1_mngs,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q1$q1_none <- factor(
  x = q1$q1_none,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q1$q1_other <- factor(
  x = q1$q1_other,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

### Finalizing the Q1 data set ####
q1 <- subset(
  x = q1,
  select = c(EIN_ID, q1_broadrange, q1_wholegenome, q1_mngs, q1_none, q1_other)
)

## Question 2 ####

### Subsetting data ####
q2 <- subset(
  x = diff_diag,
  select = c(EIN_ID, Q2)
)

### Renaming variables ####
colnames(q2) <- c("EIN_ID", "q2")

### Turning default value into missing value ####
q2$q2 <- ifelse(test = q2$q2 == "default", yes = NA, no = q2$q2)

### Turning responses into an ordered factor ####
q2$q2 <- factor(
  x = q2$q2,
  levels = c("Notatall", "Once", "Rarely", "Sometimes", "Often"),
  labels = c("Not at all", "Once", "Rarely", "Sometimes", "Often")
)

## Question 3 ####

### Subsetting the data ####
q3 <- subset(
  x = diff_diag,
  select = c(EIN_ID, Q3a, Q3b, Q3b_other)
)

### Sub-part A ####

#### Splitting the response to create binary responses ####
q3a_split <- strsplit(x = q3$Q3a, split = "|", fixed = TRUE)

#### Turning default to missing value ####
q3a_split <- lapply(
  X = q3a_split,
  FUN = function(x) {
    x <- ifelse(test = x == "default", yes = NA, no = x)
  }
)

#### Initializing binary response variables ####
q3$q3a_acute <- rep(x = NA, times = nrow(q3))
q3$q3a_subacute <- rep(x = NA, times = nrow(q3))
q3$q3a_chronic <- rep(x = NA, times = nrow(q3))

#### Filling in the responses ####
for (i in seq_len(nrow(q3))) {
  if (!all(is.na(q3a_split[[i]]))) {
    q3$q3a_acute[i] <- ifelse(
      test = "Acute" %in% q3a_split[[i]],
      yes = 1, no = 0
    )
    q3$q3a_subacute[i] <- ifelse(
      test = "Subacute" %in% q3a_split[[i]],
      yes = 1, no = 0
    )
    q3$q3a_chronic[i] <- ifelse(
      test = "Chronic" %in% q3a_split[[i]],
      yes = 1, no = 0
    )
  } else {
    q3$q3a_acute[i] <- NA
    q3$q3a_subacute[i] <- NA
    q3$q3a_chronic[i] <- NA
  }
}

#### Turning binary responses into factor variables ####
q3$q3a_acute <- factor(
  x = q3$q3a_acute,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q3$q3a_subacute <- factor(
  x = q3$q3a_subacute,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q3$q3a_chronic <- factor(
  x = q3$q3a_chronic,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

### Sub-part B ####

#### Splitting the response to create binary responses ####
q3b_split <- strsplit(x = q3$Q3b, split = "|", fixed = TRUE)

#### Turning default to missing value ####
q3b_split <- lapply(
  X = q3b_split,
  FUN = function(x) {
    x <- ifelse(test = x == "default", yes = NA, no = x)
  }
)

#### Initializing binary response variables ####
q3$q3b_blood <- rep(x = NA, times = nrow(q3))
q3$q3b_plasma <- rep(x = NA, times = nrow(q3))
q3$q3b_bone <- rep(x = NA, times = nrow(q3))
q3$q3b_stool <- rep(x = NA, times = nrow(q3))
q3$q3b_resp <- rep(x = NA, times = nrow(q3))
q3$q3b_csf <- rep(x = NA, times = nrow(q3))
q3$q3b_synovial <- rep(x = NA, times = nrow(q3))
q3$q3b_tissue <- rep(x = NA, times = nrow(q3))
q3$q3b_urine <- rep(x = NA, times = nrow(q3))
q3$q3b_other <- rep(x = NA, times = nrow(q3))

#### Filling in the responses ####
for (i in seq_len(nrow(q3))) {
  if (!all(is.na(q3b_split[[i]]))) {
    q3$q3b_blood[i] <- ifelse(
      test = "Blood" %in% q3b_split[[i]],
      yes = 1, no = 0
    )
    q3$q3b_plasma[i] <- ifelse(
      test = "Plasma" %in% q3b_split[[i]],
      yes = 1, no = 0
    )
    q3$q3b_bone[i] <- ifelse(
      test = "Bone" %in% q3b_split[[i]],
      yes = 1, no = 0
    )
    q3$q3b_stool[i] <- ifelse(
      test = "Stool" %in% q3b_split[[i]],
      yes = 1, no = 0
    )
    q3$q3b_resp[i] <- ifelse(
      test = "Resp" %in% q3b_split[[i]],
      yes = 1, no = 0
    )
    q3$q3b_csf[i] <- ifelse(
      test = "CSF" %in% q3b_split[[i]],
      yes = 1, no = 0
    )
    q3$q3b_synovial[i] <- ifelse(
      test = "Synovial" %in% q3b_split[[i]],
      yes = 1, no = 0
    )
    q3$q3b_tissue[i] <- ifelse(
      test = "Tissue" %in% q3b_split[[i]],
      yes = 1, no = 0
    )
    q3$q3b_urine[i] <- ifelse(
      test = "Urine" %in% q3b_split[[i]],
      yes = 1, no = 0
    )
    q3$q3b_other[i] <- ifelse(
      test = "Other" %in% q3b_split[[i]],
      yes = 1, no = 0
    )
  } else {
    q3$q3b_blood[i] <- NA
    q3$q3b_plasma[i] <- NA
    q3$q3b_bone[i] <- NA
    q3$q3b_stool[i] <- NA
    q3$q3b_resp[i] <- NA
    q3$q3b_csf[i] <- NA
    q3$q3b_synovial[i] <- NA
    q3$q3b_tissue[i] <- NA
    q3$q3b_urine[i] <- NA
    q3$q3b_other[i] <- NA
  }
}

#### Turning binary responses into factor variables ####
q3$q3b_blood <- factor(
  x = q3$q3b_blood,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q3$q3b_plasma <- factor(
  x = q3$q3b_plasma,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q3$q3b_bone <- factor(
  x = q3$q3b_bone,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q3$q3b_stool <- factor(
  x = q3$q3b_stool,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q3$q3b_resp <- factor(
  x = q3$q3b_resp,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q3$q3b_csf <- factor(
  x = q3$q3b_csf,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q3$q3b_synovial <- factor(
  x = q3$q3b_synovial,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q3$q3b_tissue <- factor(
  x = q3$q3b_tissue,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q3$q3b_urine <- factor(
  x = q3$q3b_urine,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q3$q3b_other <- factor(
  x = q3$q3b_other,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

### Subsetting the data ####
q3 <- q3[, -c(2, 3, 4)]

## Question 4 ####

### Subsetting the data ####
q4 <- subset(
  x = diff_diag,
  select = c(EIN_ID, Q4)
)

### Splitting the response to create binary responses ####
q4_split <- strsplit(x = q4$Q4, split = "|", fixed = TRUE)

### Turning default to missing value ####
q4_split <- lapply(
  X = q4_split,
  FUN = function(x) {
    x <- ifelse(test = x == "default", yes = NA, no = x)
  }
)

### Initializing binary response variables ####
q4$q4_local <- rep(x = NA, times = nrow(q4))
q4$q4_commercial <- rep(x = NA, times = nrow(q4))
q4$q4_research <- rep(x = NA, times = nrow(q4))
q4$q4_state <- rep(x = NA, times = nrow(q4))
q4$q4_federal <- rep(x = NA, times = nrow(q4))

#### Filling in the responses ####
for (i in seq_len(nrow(q4))) {
  if (!all(is.na(q4_split[[i]]))) {
    q4$q4_local[i] <- ifelse(
      test = "Local" %in% q4_split[[i]],
      yes = 1, no = 0
    )
    q4$q4_commercial[i] <- ifelse(
      test = "Commercial" %in% q4_split[[i]],
      yes = 1, no = 0
    )
    q4$q4_research[i] <- ifelse(
      test = "Research" %in% q4_split[[i]],
      yes = 1, no = 0
    )
    q4$q4_state[i] <- ifelse(
      test = "Statepubhlth" %in% q4_split[[i]],
      yes = 1, no = 0
    )
    q4$q4_federal[i] <- ifelse(
      test = "Fedpubhlth" %in% q4_split[[i]],
      yes = 1, no = 0
    )
  } else {
    q4$q4_local[i] <- NA
    q4$q4_commercial[i] <- NA
    q4$q4_research[i] <- NA
    q4$q4_state[i] <- NA
    q4$q4_federal[i] <- NA
  }
}

### Turning binary responses into factor variables ####
q4$q4_local <- factor(
  x = q4$q4_local,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q4$q4_commercial <- factor(
  x = q4$q4_commercial,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q4$q4_research <- factor(
  x = q4$q4_research,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q4$q4_state <- factor(
  x = q4$q4_state,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q4$q4_federal <- factor(
  x = q4$q4_federal,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

### Subsetting the data ####
q4 <- q4[, -2]

## Question 5 ####

### Subsetting the data ####
q5 <- subset(
  x = diff_diag,
  select = c(
    EIN_ID, Q5a_pt, Q5b_insurance, Q5c_instit, Q5d_lab, Q5e_research, Q5f_other,
    Q5f_othertext
  )
)

### Renaming the variables ####
colnames(q5) <- gsub(pattern = "Q", replacement = "q", x = colnames(q5))

### Turning default to missing ####
q5$q5a_pt <- ifelse(
  test = q5$q5a_pt == "default",
  yes = NA,
  no = q5$q5a_pt
)
q5$q5b_insurance <- ifelse(
  test = q5$q5b_insurance == "default",
  yes = NA,
  no = q5$q5b_insurance
)
q5$q5c_instit <- ifelse(
  test = q5$q5c_instit == "default",
  yes = NA,
  no = q5$q5c_instit
)
q5$q5d_lab <- ifelse(
  test = q5$q5d_lab == "default",
  yes = NA,
  no = q5$q5d_lab
)
q5$q5e_research <- ifelse(
  test = q5$q5e_research == "default",
  yes = NA,
  no = q5$q5e_research
)
q5$q5f_other <- ifelse(
  test = q5$q5f_other == "default",
  yes = NA,
  no = q5$q5f_other
)

### Turning responses into an ordered factor ####
q5$q5a_pt <- factor(
  x = q5$q5a_pt,
  levels = c("Notsure", "Never", "Sometimes", "Usually"),
  labels = c("Unsure", "Never", "Sometimes", "Usually")
)
q5$q5b_insurance <- factor(
  x = q5$q5b_insurance,
  levels = c("Notsure", "Never", "Sometimes", "Usually"),
  labels = c("Unsure", "Never", "Sometimes", "Usually")
)
q5$q5c_instit <- factor(
  x = q5$q5c_instit,
  levels = c("Notsure", "Never", "Sometimes", "Usually"),
  labels = c("Unsure", "Never", "Sometimes", "Usually")
)
q5$q5d_lab <- factor(
  x = q5$q5d_lab,
  levels = c("Notsure", "Never", "Sometimes", "Usually"),
  labels = c("Unsure", "Never", "Sometimes", "Usually")
)
q5$q5e_research <- factor(
  x = q5$q5e_research,
  levels = c("Notsure", "Never", "Sometimes", "Usually"),
  labels = c("Unsure", "Never", "Sometimes", "Usually")
)
q5$q5f_other <- factor(
  x = q5$q5f_other,
  levels = c("Notsure", "Never", "Sometimes", "Usually"),
  labels = c("Unsure", "Never", "Sometimes", "Usually")
)

### Subsetting the data ####
q5 <- q5[, -8]

## Question 6 ####

### Subsetting the data ####
q6 <- subset(
  x = diff_diag,
  select = c(EIN_ID, Q6)
)

### Rennaming the variables ####
colnames(q6) <- c("EIN_ID", "q6")

### Turning default value to missing value ####
q6$q6 <- ifelse(test = q6$q6 == "default", yes = NA, no = q6$q6)

### Turning responses to factor variable ####
q6$q6 <- factor(
  x = q6$q6,
  levels = c("Notsure", "No", "Yes"),
  labels = c("Not sure", "No", "Yes")
)

## Question 7 ####

### Subsetting the data ####
q7 <- subset(
  x = diff_diag,
  select = c(EIN_ID, Q7)
)

### Renaming the variables ####
colnames(q7) <- c("EIN_ID", "q7")

### Turning default value to missing value ####
q7$q7 <- ifelse(test = q7$q7 == "default", yes = NA, no = q7$q7)

### Turning responses to factor variable ####
q7$q7 <- factor(
  x = q7$q7,
  levels = c("Oneday", "Twodays", "Threedays", "Week", "Tendays"),
  labels = c(">24 hours", ">48 hours", ">3 days", ">7 days", ">10 days")
)

## Question 8 ####
q8 <- subset(
  x = diff_diag,
  select = c(EIN_ID, Q8a_prevent, Q8b_difficult, Q8_other)
)

### Sub-part A: Prevent ####

#### Splitting the response to create binary responses ####
q8a_split <- strsplit(x = q8$Q8a_prevent, split = "|", fixed = TRUE)

#### Turning default to missing value ####
q8a_split <- lapply(
  X = q8a_split,
  FUN = function(x) {
    x <- ifelse(test = x == "default", yes = NA, no = x)
  }
)

### Initializing binary response variables ####
q8$q8a_prevent_dxsteward <- rep(x = NA, times = nrow(q8))
q8$q8a_prevent_cost <- rep(x = NA, times = nrow(q8))
q8$q8a_prevent_noguideline <- rep(x = NA, times = nrow(q8))
q8$q8a_prevent_noclia <- rep(x = NA, times = nrow(q8))
q8$q8a_prevent_nofda <- rep(x = NA, times = nrow(q8))
q8$q8a_prevent_interpret <- rep(x = NA, times = nrow(q8))
q8$q8a_prevent_whichlab <- rep(x = NA, times = nrow(q8))
q8$q8a_prevent_howorder <- rep(x = NA, times = nrow(q8))
q8$q8a_prevent_specimen <- rep(x = NA, times = nrow(q8))
q8$q8a_prevent_sharept <- rep(x = NA, times = nrow(q8))
q8$q8a_prevent_other <- rep(x = NA, times = nrow(q8))

#### Filling in the responses ####
for (i in seq_len(nrow(q8))) {
  if (!all(is.na(q8a_split[[i]]))) {
    q8$q8a_prevent_dxsteward[i] <- ifelse(
      test = "Dxsteward" %in% q8a_split[[i]],
      yes = 1, no = 0
    )
    q8$q8a_prevent_cost[i] <- ifelse(
      test = "Cost" %in% q8a_split[[i]],
      yes = 1, no = 0
    )
    q8$q8a_prevent_noguideline[i] <- ifelse(
      test = "Noguideline" %in% q8a_split[[i]],
      yes = 1, no = 0
    )
    q8$q8a_prevent_noclia[i] <- ifelse(
      test = "Noclia" %in% q8a_split[[i]],
      yes = 1, no = 0
    )
    q8$q8a_prevent_nofda[i] <- ifelse(
      test = "Nofda" %in% q8a_split[[i]],
      yes = 1, no = 0
    )
    q8$q8a_prevent_interpret[i] <- ifelse(
      test = "Interpret" %in% q8a_split[[i]],
      yes = 1, no = 0
    )
    q8$q8a_prevent_whichlab[i] <- ifelse(
      test = "Whichlab" %in% q8a_split[[i]],
      yes = 1, no = 0
    )
    q8$q8a_prevent_howorder[i] <- ifelse(
      test = "Howorder" %in% q8a_split[[i]],
      yes = 1, no = 0
    )
    q8$q8a_prevent_specimen[i] <- ifelse(
      test = "Specimen" %in% q8a_split[[i]],
      yes = 1, no = 0
    )
    q8$q8a_prevent_sharept[i] <- ifelse(
      test = "Sharept" %in% q8a_split[[i]],
      yes = 1, no = 0
    )
    q8$q8a_prevent_other[i] <- ifelse(
      test = "Other" %in% q8a_split[[i]],
      yes = 1, no = 0
    )
  } else {
    q8$q8a_prevent_dxsteward[i] <- NA
    q8$q8a_prevent_cost[i] <- NA
    q8$q8a_prevent_noguideline[i] <- NA
    q8$q8a_prevent_noclia[i] <- NA
    q8$q8a_prevent_nofda[i] <- NA
    q8$q8a_prevent_interpret[i] <- NA
    q8$q8a_prevent_whichlab[i] <- NA
    q8$q8a_prevent_howorder[i] <- NA
    q8$q8a_prevent_specimen[i] <- NA
    q8$q8a_prevent_sharept[i] <- NA
    q8$q8a_prevent_other[i] <- NA
  }
}

### Turning binary responses into factor variables ####
q8$q8a_prevent_dxsteward <- factor(
  x = q8$q8a_prevent_dxsteward,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8a_prevent_cost <- factor(
  x = q8$q8a_prevent_cost,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8a_prevent_noguideline <- factor(
  x = q8$q8a_prevent_noguideline,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8a_prevent_noclia <- factor(
  x = q8$q8a_prevent_noclia,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8a_prevent_nofda <- factor(
  x = q8$q8a_prevent_nofda,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8a_prevent_interpret <- factor(
  x = q8$q8a_prevent_interpret,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8a_prevent_whichlab <- factor(
  x = q8$q8a_prevent_whichlab,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8a_prevent_howorder <- factor(
  x = q8$q8a_prevent_howorder,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8a_prevent_specimen <- factor(
  x = q8$q8a_prevent_specimen,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8a_prevent_sharept <- factor(
  x = q8$q8a_prevent_sharept,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8a_prevent_other <- factor(
  x = q8$q8a_prevent_other,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

### Sub-part B: Difficult ####

#### Splitting the response to create binary responses ####
q8b_split <- strsplit(x = q8$Q8b_difficult, split = "|", fixed = TRUE)

#### Turning default to missing value ####
q8b_split <- lapply(
  X = q8b_split,
  FUN = function(x) {
    x <- ifelse(test = x == "default", yes = NA, no = x)
  }
)

### Initializing binary response variables ####
q8$q8b_difficult_dxsteward <- rep(x = NA, times = nrow(q8))
q8$q8b_difficult_cost <- rep(x = NA, times = nrow(q8))
q8$q8b_difficult_noguideline <- rep(x = NA, times = nrow(q8))
q8$q8b_difficult_noclia <- rep(x = NA, times = nrow(q8))
q8$q8b_difficult_nofda <- rep(x = NA, times = nrow(q8))
q8$q8b_difficult_interpret <- rep(x = NA, times = nrow(q8))
q8$q8b_difficult_whichlab <- rep(x = NA, times = nrow(q8))
q8$q8b_difficult_howorder <- rep(x = NA, times = nrow(q8))
q8$q8b_difficult_specimen <- rep(x = NA, times = nrow(q8))
q8$q8b_difficult_sharept <- rep(x = NA, times = nrow(q8))
q8$q8b_difficult_other <- rep(x = NA, times = nrow(q8))

#### Filling in the responses ####
for (i in seq_len(nrow(q8))) {
  if (!all(is.na(q8b_split[[i]]))) {
    q8$q8b_difficult_dxsteward[i] <- ifelse(
      test = "Dxsteward" %in% q8b_split[[i]],
      yes = 1, no = 0
    )
    q8$q8b_difficult_cost[i] <- ifelse(
      test = "Cost" %in% q8b_split[[i]],
      yes = 1, no = 0
    )
    q8$q8b_difficult_noguideline[i] <- ifelse(
      test = "Noguideline" %in% q8b_split[[i]],
      yes = 1, no = 0
    )
    q8$q8b_difficult_noclia[i] <- ifelse(
      test = "Noclia" %in% q8b_split[[i]],
      yes = 1, no = 0
    )
    q8$q8b_difficult_nofda[i] <- ifelse(
      test = "Nofda" %in% q8b_split[[i]],
      yes = 1, no = 0
    )
    q8$q8b_difficult_interpret[i] <- ifelse(
      test = "Interpret" %in% q8b_split[[i]],
      yes = 1, no = 0
    )
    q8$q8b_difficult_whichlab[i] <- ifelse(
      test = "Whichlab" %in% q8b_split[[i]],
      yes = 1, no = 0
    )
    q8$q8b_difficult_howorder[i] <- ifelse(
      test = "Howorder" %in% q8b_split[[i]],
      yes = 1, no = 0
    )
    q8$q8b_difficult_specimen[i] <- ifelse(
      test = "Specimen" %in% q8b_split[[i]],
      yes = 1, no = 0
    )
    q8$q8b_difficult_sharept[i] <- ifelse(
      test = "Sharept" %in% q8b_split[[i]],
      yes = 1, no = 0
    )
    q8$q8b_difficult_other[i] <- ifelse(
      test = "Other" %in% q8b_split[[i]],
      yes = 1, no = 0
    )
  } else {
    q8$q8b_difficult_dxsteward[i] <- NA
    q8$q8b_difficult_cost[i] <- NA
    q8$q8b_difficult_noguideline[i] <- NA
    q8$q8b_difficult_noclia[i] <- NA
    q8$q8b_difficult_nofda[i] <- NA
    q8$q8b_difficult_interpret[i] <- NA
    q8$q8b_difficult_whichlab[i] <- NA
    q8$q8b_difficult_howorder[i] <- NA
    q8$q8b_difficult_specimen[i] <- NA
    q8$q8b_difficult_sharept[i] <- NA
    q8$q8b_difficult_other[i] <- NA
  }
}

### Turning binary responses into factor variables ####
q8$q8b_difficult_dxsteward <- factor(
  x = q8$q8b_difficult_dxsteward,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8b_difficult_cost <- factor(
  x = q8$q8b_difficult_cost,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8b_difficult_noguideline <- factor(
  x = q8$q8b_difficult_noguideline,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8b_difficult_noclia <- factor(
  x = q8$q8b_difficult_noclia,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8b_difficult_nofda <- factor(
  x = q8$q8b_difficult_nofda,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8b_difficult_interpret <- factor(
  x = q8$q8b_difficult_interpret,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8b_difficult_whichlab <- factor(
  x = q8$q8b_difficult_whichlab,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8b_difficult_howorder <- factor(
  x = q8$q8b_difficult_howorder,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8b_difficult_specimen <- factor(
  x = q8$q8b_difficult_specimen,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8b_difficult_sharept <- factor(
  x = q8$q8b_difficult_sharept,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
q8$q8b_difficult_other <- factor(
  x = q8$q8b_difficult_other,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

### Subsetting the data ####
q8 <- q8[, -c(2, 3, 4)]

## Question 9 ####

### Subsetting the data ####
q9 <- subset(
  x = diff_diag,
  select = c(EIN_ID, Q9a, Q9b, Q9c, Q9d, Q9e)
)

### Renaming the variables ####
colnames(q9) <- gsub(pattern = "Q", replacement = "q", x = colnames(q9))

### Turning default value to missing value ####
q9$q9a <- ifelse(test = q9$q9a == "default", yes = NA, no = q9$q9a)
q9$q9b <- ifelse(test = q9$q9b == "default", yes = NA, no = q9$q9b)
q9$q9c <- ifelse(test = q9$q9c == "default", yes = NA, no = q9$q9c)
q9$q9d <- ifelse(test = q9$q9d == "default", yes = NA, no = q9$q9d)
q9$q9e <- ifelse(test = q9$q9e == "default", yes = NA, no = q9$q9e)

### Turning responses into factor variables ####
q9$q9a <- factor(
  x = q9$q9a,
  levels = c("Neverused", "Rare", "Occasional", "Often"),
  labels = c("Never used", "Rarely", "Occasionally", "Often")
)
q9$q9b <- factor(
  x = q9$q9b,
  levels = c("Neverused", "Rare", "Occasional", "Often"),
  labels = c("Never used", "Rarely", "Occasionally", "Often")
)
q9$q9c <- factor(
  x = q9$q9c,
  levels = c("Neverused", "Rare", "Occasional", "Often"),
  labels = c("Never used", "Rarely", "Occasionally", "Often")
)
q9$q9d <- factor(
  x = q9$q9d,
  levels = c("Neverused", "Rare", "Occasional", "Often"),
  labels = c("Never used", "Rarely", "Occasionally", "Often")
)
q9$q9e <- factor(
  x = q9$q9e,
  levels = c("Neverused", "Rare", "Occasional", "Often"),
  labels = c("Never used", "Rarely", "Occasionally", "Often")
)

## Question 10 ####

### Subsetting the data ####
q10 <- subset(
  x = diff_diag,
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
  x = diff_diag,
  select = c(EIN_ID, AdvMolDx_date)
)

### Creating date only variable ####
wave$date <- as.Date(wave$AdvMolDx_date)

### Initializing wave dates ####
wave_dates <- as.Date(
  x = c("2023-05-03", "2023-05-16", "2023-05-23"),
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
  file = "./data/difficult_diagnoses.rds"
)