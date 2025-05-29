library(dplyr)

df <- read.csv("/Users/fyzeen/FyzeenLocal/GitHub/DivineWellness/data/DivineWellnessProjec-DWPTotal342Responses_DATA_2025-05-26_1407.csv")

# Computing CRS Score
crs_score <- rowSums(df[, c("crs_life_after_death", "believe_things_determined_fate", "justice_served_life", "deep_personal_meaning_faith", "faith_purpose_direction", "part_who_i_am")])
df$CRS_SCORE <- crs_score

# Compute MRS Score
# df$mrs_2 <- 6 - df[,"mrs_2"]
# df$mrs_12 <- 6 - df[,"mrs_12"]
# df$mrs_13 <- 6 - df[,"mrs_13"]
# df$mrs_14 <- 6 - df[,"mrs_14"]

MRS_TOTAL_SCORE <- rowSums(df[, c("mrs_1", "mrs_2", "mrs_3", "mrs_4", "mrs_5", "mrs_6", "mrs_7", "mrs_8", "mrs_9", "mrs_10", "mrs_11", "mrs_12", "mrs_13", "mrs_14")])
MRS_ReligiousPractice_Subscale <- rowSums(df[, c("mrs_2", "mrs_3", "mrs_4", "mrs_5", "mrs_6", "mrs_7", "mrs_8", "mrs_9", "mrs_10", "mrs_11")])
MRS_IntrinsicReligiosity_Subscale <- rowSums(df[, c("mrs_12", "mrs_13", "mrs_14")])

df$MRS_TOTAL_SCORE <- MRS_TOTAL_SCORE
df$MRS_ReligiousPractice_Subscale <- MRS_ReligiousPractice_Subscale
df$MRS_IntrinsicReligiosity_Subscale <- MRS_IntrinsicReligiosity_Subscale

# Computing HPLP II scores
hplp_columns <- grep("^hplp", colnames(df), value=TRUE)

df <- df %>%
  mutate(across(
    .cols = hplp_columns,
    .fns = ~ recode(.x, N = 1, S = 2, O = 3, R = 4),
    .names = "recoded_{.col}"
  ))

recoded_hplp_columns <- grep("^recoded_hplp", colnames(df), value=TRUE)

HPLP_Health_Promoting_Lifestyle <- rowSums(df[, recoded_hplp_columns])

HPLP_Health_Responsibility <- rowSums(df[, c("recoded_hplp_3", "recoded_hplp_9", "recoded_hplp_15", "recoded_hplp_21", "recoded_hplp_27", "recoded_hplp_33", "recoded_hplp_39", "recoded_hplp_45", "recoded_hplp_51")])

HPLP_Physical_Activity <- rowSums(df[, c("recoded_hplp_4", "recoded_hplp_10", "recoded_hplp_16", "recoded_hplp_22", "recoded_hplp_28", "recoded_hplp_34", "recoded_hplp_40", "recoded_hplp_46")])

HPLP_Nutrition <- rowSums(df[, c("recoded_hplp_2", "recoded_hplp_8", "recoded_hplp_14", "recoded_hplp_20", "recoded_hplp_26", "recoded_hplp_32", "recoded_hplp_38", "recoded_hplp_44", "recoded_hplp_50")])

HPLP_Spiritual_Growth <- rowSums(df[, c("recoded_hplp_6", "recoded_hplp_12", "recoded_hplp_18", "recoded_hplp_24", "recoded_hplp_30", "recoded_hplp_36", "recoded_hplp_42", "recoded_hplp_48", "recoded_hplp_52")])

HPLP_Interpersonal_Relations <- rowSums(df[, c("recoded_hplp_1", "recoded_hplp_7", "recoded_hplp_13", "recoded_hplp_19", "recoded_hplp_25", "recoded_hplp_31", "recoded_hplp_37", "recoded_hplp_43", "recoded_hplp_49")])

HPLP_Stress_Management <- rowSums(df[, c("recoded_hplp_5", "recoded_hplp_11", "recoded_hplp_17", "recoded_hplp_23", "recoded_hplp_29", "recoded_hplp_35", "recoded_hplp_41", "recoded_hplp_47")])

df$HPLP_Health_Promoting_Lifestyle <- HPLP_Health_Promoting_Lifestyle
df$HPLP_Health_Responsibility <- HPLP_Health_Responsibility
df$HPLP_Physical_Activity <- HPLP_Physical_Activity
df$HPLP_Nutrition <- HPLP_Nutrition
df$HPLP_Spiritual_Growth <- HPLP_Spiritual_Growth
df$HPLP_Interpersonal_Relations <- HPLP_Interpersonal_Relations
df$HPLP_Stress_Management <- HPLP_Stress_Management

# Compute BRS

# df$brs_2 <- 6 - df[,"brs_2"]
# df$brs_4 <- 6 - df[,"brs_4"]
# df$brs_6 <- 6 - df[,"brs_6"]

brs_score <- rowSums(df[, c("brs_1", "brs_2", "brs_3", "brs_4", "brs_5", "brs_6")])

df$BRS_SCORE <- brs_score


# Compute PHQ-2

PHQ_score <- rowSums(df[, c("phq_1", "phq_2")])
df$PHQ_SCORE <- PHQ_score

# Compute GAD-2

GAD_score <- rowSums(df[, c("gad_1", "gad_2")])
df$GAD_SCORE <- GAD_score

# WRITE TO CSV HERE!!!!

# Plot/Statistics

library (ggplot2)

data("df")

# GAD vs CRS
ggplot(data = df, aes(x = CRS_SCORE, y = GAD_SCORE)) + geom_point() + geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) + ggtitle("GAD-2 vs CRS") + theme_minimal()
model <- lm(df$GAD_SCORE ~ df$CRS_SCORE)
summary(model)

# GAD vs MRS Total Score
ggplot(data = df, aes(x = MRS_TOTAL_SCORE, y = GAD_SCORE)) + geom_point() + geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) + ggtitle("GAD-2 vs MRS Total") + theme_minimal()
model <- lm(df$GAD_SCORE ~ df$MRS_TOTAL_SCORE)
summary(model)


# GAD vs MRS Religious Practice Subscale
ggplot(data = df, aes(x = MRS_ReligiousPractice_Subscale, y = GAD_SCORE)) + geom_point() + geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) + ggtitle("GAD-2 vs MRS Religious Practice") + theme_minimal()
model <- lm(df$GAD_SCORE ~ df$MRS_ReligiousPractice_Subscale)
summary(model)

# GAD vs MRS Intrinsic Religiosity Subscale
ggplot(data = df, aes(x = MRS_IntrinsicReligiosity_Subscale, y = GAD_SCORE)) + geom_point() + geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) + ggtitle("GAD-2 vs MRS Intrinsic Religiosity") + theme_minimal()
model <- lm(df$GAD_SCORE ~ df$MRS_IntrinsicReligiosity_Subscale)
summary(model)

# BRS vs CRS
ggplot(data = df, aes(x = CRS_SCORE, y = BRS_SCORE)) + geom_point() + geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) + ggtitle("BRS vs CRS") + theme_minimal()
model <- lm(df$BRS_SCORE ~ df$CRS_SCORE)
summary(model)

# BRS vs MRS Total Score
ggplot(data = df, aes(x = MRS_TOTAL_SCORE, y = BRS_SCORE)) + geom_point() + geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) + ggtitle("BRS vs MRS Total") + theme_minimal()
model <- lm(df$BRS_SCORE ~ df$MRS_TOTAL_SCORE)
summary(model)

# BRS vs MRS Religious Practice Subscale
ggplot(data = df, aes(x = MRS_ReligiousPractice_Subscale, y = BRS_SCORE)) + geom_point() + geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) + ggtitle("BRS vs MRS Religious Practice") + theme_minimal()
model <- lm(df$BRS_SCORE ~ df$MRS_ReligiousPractice_Subscale)
summary(model)

# BRS vs MRS Intrinsic Religiosity Subscale
ggplot(data = df, aes(x = MRS_IntrinsicReligiosity_Subscale, y = BRS_SCORE)) + geom_point() + geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) + ggtitle("BRS vs MRS Intrinsic Religiosity") + theme_minimal()
model <- lm(df$BRS_SCORE ~ df$MRS_IntrinsicReligiosity_Subscale)
summary(model)

# Plot/Statistics
