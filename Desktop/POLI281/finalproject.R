#POLI 281 Final Project

setwd("C:/Users/sruthymammen/DESKTOP/POLI281")

cces18 <- read_csv("cces18.csv")

#clean the environment
rm(list = ls())


#libraries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)

#II

# Exclude respondents who answered "Don't know"
cces18_filtered <- cces18[cces18$wait != 6, ]

# Calculate proportion of people for each response
wait_table <- table(cces18_filtered$wait)
prop_wait <- prop.table(wait_table)

# Create a table with proportions
wait_table <- data.frame(Response = c("Not at all", "Less than 10 minutes", "10-30 minutes", "31 minutes – 1 hour", "More than 1 hour"),
                         Proportion = prop_wait,
                         stringsAsFactors = FALSE)

# Calculate mean and median wait time (excluding NA values)
mean_wait <- mean(cces18_filtered$wait, na.rm = TRUE)
median_wait <- median(cces18_filtered$wait, na.rm = TRUE)

# Display table and summary statistics
print("Proportion of people who provided each response:")
print(wait_table)
print(paste0("Mean wait time: ", mean_wait))
print(paste0("Median wait time: ", median_wait))

# Filter out rows where wait time is known and not "Don't know" (i.e., not equal to 6)
cces18_filtered <- cces18[cces18$wait != 6, ]

# Create a new Boolean variable 'more10' based on filtered data
cces18_filtered$more10 <- cces18_filtered$wait > 2  # 2 represents 'Less than 10 minutes'

# Convert to TRUE/FALSE
cces18_filtered$more10 <- as.logical(cces18_filtered$more10)

# View the updated dataset
View(cces18_filtered)

#III
# Filter out rows where wait time is known and not "Don't know" (i.e., not equal to 6)
cces18_filtered <- cces18[cces18$wait != 6, ]

# Create a new variable 'more10' based on wait times greater than 2 (which represents 'Less than 10 minutes')
cces18_filtered$more10 <- cces18_filtered$wait > 2  # 2 represents 'Less than 10 minutes'

# Convert to TRUE/FALSE
cces18_filtered$more10 <- as.logical(cces18_filtered$more10)

# Now, calculate the proportion of people who waited more than 10 minutes for each state
state_wait_percentage <- cces18_filtered %>%
  group_by(state) %>%
  summarize(percentage_more10 = mean(more10) * 100)

# Create the bar plot using ggplot2 with adjusted y-axis spacing
ggplot(state_wait_percentage, aes(x = reorder(state, -percentage_more10), y = percentage_more10)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Proportion of People Who Waited More Than Ten Minutes by State",
       x = "State",
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylim(0, 100) +
  scale_y_continuous(expand = expansion(add = c(0.05, 0.05)))

#IV


# Filter out rows with 'Don't know' responses in the 'wait' variable
cces18_filtered <- cces18 %>%
  filter(wait != 6)

# Group by state and region simultaneously, calculate the proportion of people who waited more than ten minutes
state_wait_percentage <- cces18_filtered %>%
  group_by(state, region) %>%
  summarize(prop_more10 = mean(wait > 2) * 100) %>%
  ungroup()

# Create the bar plot with color-coded bars by region
ggplot(state_wait_percentage, aes(x = reorder(state, -prop_more10), y = prop_more10, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of People Who Waited More Than Ten Minutes by State",
       x = "State",
       y = "Percentage",
       fill = "Region") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylim(0, 100) +
  scale_fill_discrete(name = "Region")


#V
cces18$conserv_vote <- cces18$vote2016

cces18$conserv_vote <- ifelse(cces18$conserv_vote == "1", 1, cces18$conserv_vote)

cces18$conserv_vote <- ifelse(cces18$conserv_vote == "2", 0, cces18$conserv_vote)

cces18$conserv_vote <- ifelse(cces18$conserv_vote == "3", "", cces18$conserv_vote)

cces18$conserv_vote <- ifelse(cces18$conserv_vote == "4", "", cces18$conserv_vote)

cces18$conserv_vote <- ifelse(cces18$conserv_vote == "5", "", cces18$conserv_vote)

cces18$conserv_vote <- as.numeric(cces18$conserv_vote)

#I have transformed the conserv_vote variable into a binary varible where 1=voted for Trump and 0=voted for Hillary. By converting it to a numeric variable, all the missing values are now noted as "NA".
cces18$wait <- ifelse(cces18$wait == 6, "", cces18$wait)
cces18$wait <- ifelse(cces18$wait <= "2", 0, cces18$wait)
cces18$wait <- ifelse(cces18$wait >= "3", 1, cces18$wait)

cces18$wait <- as.numeric(cces18$wait)

#I have created the wait variable into a binary variable where 0= wait less than 10 minutes and 1= wait more than 10 minutes. After, I transformed it to a numeric variable so that the missing values as reported as "NA", I have done this so that we can later remove them when calculating the proportion of those that wait more than 10 minutes.


time_liberals <- cces18 %>%
  filter(conserv_vote == 0) %>%
  summarise(prop = mean(wait, na.rm = TRUE))

time_conservatives <- cces18 %>%
  filter(conserv_vote == 1) %>%
  summarise(prop = mean(wait, na.rm = TRUE))

#This worked out the proportion of liberals and conservatives respectively that waited for more than 10 minutes.

time_liberals

time_conservatives

# ~~~~~~~~~~~~~~~~~+ VI - Waiting Times by Race +~~~~~~~~~~~~~~~~~ #
# Classifying respondents by race
race_labels <- c("White Non-Hispanic", "Black", "Hispanic", "Asian", "Other", "Other", "Other", "Other")
cces18$race_5 <- race_labels[cces18$race]

# Creating more10 variable -TEMPORARY-
more10 <- c(cces18$wait < 6 & cces18$wait > 2)

# Set up for bar chart
race_n <- table(cces18$race_5[more10])
race_d <- table(cces18$race_5)
race_prop <- (race_n / race_d)
race_df <- data.frame(race_prop)


# Making the bar chart
race_wait <- ggplot(race_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Race") +
  ylab("% that waited > 10 minutes")

race_wait


# ~~~~~~~~~~~~~~~~~+ VII - Waiting Times by Income +~~~~~~~~~~~~~~~~~ #
# Ignored all unsure responses to faminc.
cces18$faminc_4 <- ifelse(cces18$faminc < 4, "Lower Class", ifelse(cces18$faminc < 11, "Middle Class", ifelse(cces18$faminc < 15, "Upper Class", ifelse(cces18$faminc < 17, "The 1%", NA))))

table(cces18$faminc_4)

fam_n <- table(cces18$faminc_4[more10])
fam_d <- table(cces18$faminc_4)
fam_prop <- (fam_n / fam_d)
fam_df <- data.frame(fam_prop)

fam_wait <- ggplot(fam_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Class") +
  ylab("% that waited > 10 minutes")

fam_wait

#  ~~~+ VIII - Using subclassification to Account for the Effect of Income+~~~ #
# Create one race-based (VI) bar chart per faminc_4 category.

# Creating different dataframes to use for proportions and ggplot.
lower <- cces18 %>%
  filter(faminc < 4)
middle <- cces18 %>%
  filter(faminc > 3 & faminc < 11)
upper <- cces18 %>%
  filter(faminc > 10 & faminc < 15)
one <- cces18 %>%
  filter(faminc > 14 & faminc != 97)

# Need to create a new more10 for each because it just won't work otherwise :(.
low10 <- c(lower$wait < 6 & lower$wait > 2)
mid10 <- c(middle$wait < 6 & middle$wait > 2)
up10 <- c(upper$wait < 6 & upper$wait > 2)
one10 <- c(one$wait < 6 & one$wait > 2)

# Setting up proportions for the bar charts.
low_prop <- (table(lower$race_5[low10]) / table(lower$race_5))
low_df <- data.frame(low_prop)

mid_prop <- (table(middle$race_5[mid10]) / table(middle$race_5))
mid_df <- data.frame(mid_prop)

up_prop <- (table(upper$race_5[up10]) / table(upper$race_5))
up_df <- data.frame(up_prop)

one_prop <- (table(one$race_5[one10]) / table(one$race_5))
one_df <- data.frame(one_prop)

# Finally creating the bar charts themselves.
low_wait <- ggplot(low_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Race") +
  ylab("% of Lower Class voters that waited > 10 minutes")

mid_wait <- ggplot(mid_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Race") +
  ylab("% of Middle class voters that waited > 10 minutes")

up_wait <- ggplot(up_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Race") +
  ylab("% of Upper Class voters that waited > 10 minutes")

one_wait <- ggplot(one_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Race") +
  ylab("% of 1% that waited > 10 minutes")

low_wait
mid_wait
up_wait
one_wait

#IX
ggplot(cces18, aes(x=income_county)) + geom_histogram()

# Removing outliers in the income_county variable

q1_county <- as.numeric(quantile(cces18$income_county, 0.25, na.rm = TRUE))
q3_county <- as.numeric(quantile(cces18$income_county, 0.75, na.rm = TRUE))

IQR_county <- IQR(cces18$income_county, na.rm = TRUE)

cces18 <- cces18 %>%
  filter((income_county >= q1_county - (IQR_county * 1.5)) & (income_county <= q3_county + (IQR_county * 1.5)))

# Now the distribution does not include statistical outliers.

cces18$density <- (cces18$county_pop / cces18$land_area) / 1000

ggplot(cces18, aes(x=density)) + geom_histogram(binwidth = 0.1)

# Removing outliers in the density variable

p95_density <- as.numeric(quantile(cces18$density, 0.95, na.rm = TRUE))

cces18 <- cces18 %>%
  filter(density <= p95_density)

# Now the distribution of density does not include outliers.

# Creating variables for regressions

cces18$black <- ifelse(cces18$race == 2, 1, 0)
cces18$hispanic <- ifelse(cces18$race == 3, 1, 0)
cces18$asian <- ifelse(cces18$race == 4, 1, 0)
cces18$other <- ifelse((cces18$race == 5) | (cces18$race == 6) | (cces18$race == 7) | (cces18$race == 8), 1, 0)

cces18$wait_reg <- as.numeric(ifelse(cces18$wait == 6, "", cces18$wait))
cces18$faminc_reg <- as.numeric(ifelse(cces18$faminc == 97, "", cces18$faminc))

# Estimating regression models

fit1 <- lm(wait_reg ~ black + hispanic + asian + other, data = cces18)
fit2 <- lm(wait_reg ~ black + hispanic + asian + other + faminc_reg + income_county + density, data = cces18)

summary(fit1)
summary(fit2)





