
#-----------   QUESTION 1: PREPARING DATASET + TECHNIQUES  -------------

# commit test
# get Dir
getwd()

# set Dir
setwd("C:/Users/STUDENT/Desktop/COLLEGE/COLLEGE REPOSITORY/YEAR-4/CLASSES YEAR 4/7th -SEMESTER/01-DATA EXPLORATION/CAs/data_exploration_and_preparation_CA1")


# loading libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gridExtra")
library(ggplot2)
library(dplyr)
library(gridExtra)

# load dataset
mydata <- read.csv("Electric_Vehicle_Population_Data .csv", stringsAsFactors = FALSE)
mydata <- cars
View(mydata)


# Preview the dataset
head(mydata)


# Check the dimensions of the my data
dim(mydata)

# Structure of the dataset
str(mydata)

# Check for missing values
missing_values <- sum(is.na(mydata))
print(paste("Total missing values:", missing_values))




#https://www.geeksforgeeks.org/outlier-analysis-in-r/
  
# Min-Max Scaling
normalizeMinMax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

mydata_minmax <- mydata
mydata_minmax$Electric.Range.Norm <- normalizeMinMax(mydata$Electric.Range)


# Standardization
normalizeStandardized <- function(x) {
  return ((x - mean(x)) / sd(x))
}

mydata_standardized <- mydata
mydata_standardized$Electric.Range.Std <- normalizeStandardized(mydata$Electric.Range)


# Robust Scaling
mydata_robust <- mydata
mydata_robust$Electric.Range.Robust <- scale(mydata$Electric.Range, center = TRUE, scale = TRUE)


# Histograms for Electric Range
p1 <- ggplot(mydata, aes(x = Electric.Range)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of Electric Range (Original Data)")


p2 <- ggplot(mydata_minmax, aes(x = Electric.Range.Norm)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black") +
  labs(title = "Histogram of Electric Range (Min-Max Scaled)")


p3 <- ggplot(mydata_standardized, aes(x = Electric.Range.Std)) +
  geom_histogram(binwidth = 0.1, fill = "red", color = "black") +
  labs(title = "Histogram of Electric Range (Standardized)")


p4 <- ggplot(mydata_robust, aes(x = Electric.Range.Robust)) +
  geom_histogram(binwidth = 0.1, fill = "purple", color = "black") +
  labs(title = "Histogram of Electric Range (Robust Scaled)")


grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)


# Scatter plot example: Electric Range vs Model Year
scatter_plot <- ggplot(mydata, aes(x = Model.Year, y = Electric.Range)) +
  geom_point(color = "black") +
  labs(title = "Scatter Plot of Electric Range vs Model Year")

# Display the scatter plot
print(scatter_plot)


# Histogram for original data
ggplot(mydata, aes(x = Electric.Range)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of Electric Range (Original Data)")


#-----------   QUESTION 2: DATA ANALYSIS (EDA)  --------------------------------

# histogram plot for Variations in Feature
ggplot(mydata, aes(x = Electric.Range)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of Electric Range")

# scatter plot for covariation
ggplot(mydata, aes(x = Model.Year, y = Electric.Range)) +
  geom_point(color = "black") +
  labs(title = "Scatter Plot of Electric Range vs Model Year")

# scatter plot for covariation
ggplot(mydata, aes(y = Electric.Range)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Electric Range")





