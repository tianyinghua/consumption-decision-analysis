# ================================
# Consumption Decision Analysis Project
# Author: Tianying Hua
# Date: Feb 2026
# ================================

# 0. Make sure we are in the correct folder
# (optional) setwd("~/consumption decision")

# 1. Load data
data <- read.csv("data.csv", stringsAsFactors = FALSE, check.names = TRUE)

# 2. Rename key columns to simple names (robust way)
# Look at names(data) if you want: names(data)

# age group column
age_col <- grep("Age", names(data), ignore.case = TRUE, value = TRUE)[1]
# student column
student_col <- grep("student", names(data), ignore.case = TRUE, value = TRUE)[1]
# location column
location_col <- grep("location", names(data), ignore.case = TRUE, value = TRUE)[1]
# spending column (the long 'Monthly personal spending...' one)
spend_col <- grep("Monthly", names(data), ignore.case = TRUE, value = TRUE)[1]

names(data)[names(data) == age_col] <- "age"
names(data)[names(data) == student_col] <- "student"
names(data)[names(data) == location_col] <- "location"
names(data)[names(data) == spend_col] <- "spend"

# 3. Clean / check
stopifnot(length(data$age) == nrow(data))
stopifnot(length(data$student) == nrow(data))
stopifnot(length(data$location) == nrow(data))
stopifnot(length(data$spend) == nrow(data))

# 4. Create numeric spending variable (fix your NA problem)
s <- as.character(data$spend)
data$spend_num <- NA_real_

# Below / Above
data$spend_num[grepl("Below", s, ignore.case = TRUE)] <- 1000
data$spend_num[grepl("Above", s, ignore.case = TRUE)] <- 9000

# Ranges like "2,000 - 4,000" (handle commas, different dashes)
get_mean_two_nums <- function(x) {
  nums <- as.numeric(gsub(",", "", regmatches(x, gregexpr("[0-9,]+", x))[[1]]))
  if (length(nums) >= 2) mean(nums[1:2]) else NA_real_
}

range_idx <- is.na(data$spend_num) & grepl("[0-9]", s)
data$spend_num[range_idx] <- sapply(s[range_idx], get_mean_two_nums)

# 5. Convert predictors to factors
data$location <- factor(data$location)
data$student <- factor(data$student)
data$age <- factor(data$age)

# 6. Quick check
cat("Rows:", nrow(data), "\n")
cat("Missing spend_num:", sum(is.na(data$spend_num)), "\n")
print(table(data$location, useNA = "ifany"))
print(table(data$student, useNA = "ifany"))
print(table(data$age, useNA = "ifany"))

# 7. Regression (drop NAs, drop empty levels)
df <- na.omit(data[, c("spend_num","location","student","age")])
df$location <- droplevels(df$location)
df$student <- droplevels(df$student)
df$age <- droplevels(df$age)

# If any factor has <2 levels after NA removal, you can't include it
# (this prevents the contrasts error)
if (nlevels(df$location) < 2) df$location <- NULL
if (nlevels(df$student) < 2) df$student <- NULL
if (nlevels(df$age) < 2) df$age <- NULL

model <- lm(spend_num ~ ., data = df)
summary(model)





