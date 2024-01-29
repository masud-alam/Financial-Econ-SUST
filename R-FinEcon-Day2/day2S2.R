### IQAC R Workshop: Day2- Session 1 


## Calling packages

req <- substitute(require(x, character.only = TRUE))
libs<-c("psych", "tidyverse", "lessR", "patchwork")
sapply(libs, function(x) eval(req) || {install.packages(x); eval(req)})


###Analysis of data

job <- read_csv("JobSatisfaction.csv")

count(job,Gender)
count(job, JobSat1)
count(job, Gender, Location)
xtabs(~Gender+Location, data=job)
## NA issue
xtabs(~Gender+Location, data=job, addNA = T)


## Frequency and percentage in one table
# Calculate the frequency of each gender
gender_freq <- table(job$Gender)
gender_freq

# Calculate the percentage using prop.table()
gender_percentage <- prop.table(gender_freq) * 100
gender_percentage



# Create a data frame to combine frequency and percentage
gender_summary <- data.frame(Gender = names(gender_freq),
                             Frequency = as.vector(gender_freq),
                             Percentage = as.vector(gender_percentage))



# Display the summary table
print(gender_summary, digits = 2)



## Data visualization

BarChart(JobSat1,data = job)
?BarChart
BarChart(JobSat1,data = job, by1=Location)
BarChart(JobSat1,data = job, by=Location)
PieChart(Gender,data = job)

job$gender1 <- ifelse(job$Gender == "Man", 1, 2)

job1 <- na.omit(job)
gender_counts <- table(job1$gender1)

library(plotrix)
# Create a 3D pie chart
pie3D(gender_counts, 
      labels = c("Male", "Female"), 
      explode = 0.01,       # Explode the slices for emphasis
      main = "Gender Distribution")


## Side by side box plot

# Sample data
data <- data.frame(
  category = rep(c("A", "B", "C"), each = 20),
  value = rnorm(60)
)

# Create side-by-side box plot
boxplot(value ~ category, data = data, 
        main = "Side-by-Side Box Plot",
        xlab = "Category",
        ylab = "Value",
        col = c("lightblue","green","blue"),
        border = "black")



# library(ggplot2)
# # Simulated data
# set.seed(123)  # Setting seed for reproducibility
# categories <- rep(c("Category A", "Category B", "Category C"), each = 30)
# values <- rnorm(90, mean = c(10, 15, 8), sd = c(2, 3, 1))
# 
# # Create a data frame
# data <- data.frame(category = categories, value = values)
# 
# # Create a side-by-side box plot
# ggplot(data, aes(x = category, y = value, fill = category)) +
#   geom_boxplot() +
#   labs(title = "Side-by-Side Box Plot",
#        x = "Category",
#        y = "Value") +
#   theme_minimal() +
#   theme(legend.position = "none")



##  Continuous variable----

library(officer)
library(flextable)

job <- read_csv("JobSatisfaction.csv")

## Summary statistics using psych package
df_job <- job[,c(2,3,19,20)]


describe(df_job, fast = T)


## Create Summary stat table and export as word file
# Generate summary statistics using the describe() function
summary_stats <- describe(df_job, fast = T)

# Convert the summary statistics into a data frame
summary_df <- as.data.frame(summary_stats)


# Create a flextable from the summary data frame
ft <- flextable(summary_df)

# Specify the file path for the Word document
output_file <- "C:/Users/USER/OneDrive/Desktop/IQAC-Day2/sumst.docx"

# Save the flextable as a Word document
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = output_file)



## Visualization (base R function is not recommended)
hist(job$Tenure_Yrs, col = c("red","green","blue"))

##Violin plot Using lessR package

Plot(Tenure_Yrs, data = job)

Plot(Tenure_Yrs, data = job, by1 = Location)



##One Sample T-Test-----

# Load the iris dataset
data(iris)
# Perform the one-sample t-test
result_one_sample <- t.test(iris$Sepal.Length, mu = 5.8)

# View the test result
result_one_sample
##Compare the p-value to your chosen significance level 
#to determine whether to reject the null hypothesis.


##Two Independent Sample T-Test----

# Filter data for Setosa and Versicolor species
setosa_sepal_length <- iris$Sepal.Length[iris$Species == "setosa"]
versicolor_sepal_length <- iris$Sepal.Length[iris$Species == "versicolor"]

# Perform the two-sample t-test
result_two_sample <- t.test(setosa_sepal_length, versicolor_sepal_length)

# View the test result
result_two_sample


#Paired sample T-Test------


# Load the ToothGrowth dataset
data(ToothGrowth)
# View the first few rows of the dataset
head(ToothGrowth)
# Summary statistics of the dataset
summary(ToothGrowth)
# Structure of the dataset
str(ToothGrowth)

#Prepare the data
# Create vectors for OJ and VC
oj_tooth_length <- ToothGrowth$len[ToothGrowth$supp == "OJ"]
vc_tooth_length <- ToothGrowth$len[ToothGrowth$supp == "VC"]

# Perform the paired sample t-test
result_pt <- t.test(oj_tooth_length, vc_tooth_length, paired = TRUE)

# View the test result
result_pt


##Correlation Coefficients/heat map-----



##Violin plot Using lessR package
library(summarytools)
library(tidyverse)
library(readxl)
library(corrr)
flower <- read_excel("flower.xls")

## Select a few variables of interest
# Option 1: Table
flower %>% select(height,weight,leafarea, shootarea, flowers) %>% 
  correlate() %>% shave() %>% 
  fashion()

# Option 2: plot
flower %>% select(height,weight,leafarea, shootarea, flowers) %>% 
  correlate() %>% shave() %>% 
  rplot()

## Option 3
library(psych) # Load the psych package for the corr.test() function
flower %>% select(height,weight,leafarea, shootarea, flowers)->df5
# Run correlation tests for all pairs of variables in df5
correlation_tests <- corr.test(df5)
# View the results
correlation_tests
print(correlation_tests,short=FALSE)
##  Option 4
#install.packages("corrplot")
library(corrplot)
# Calculate the correlation matrix
correlation_matrix <- cor(df5)
# Create a correlation heatmap
corrplot(correlation_matrix, 
         method = "color", # Use color to represent correlations
         type = "full",   # Display the lower triangle of the matrix
         tl.cex = 0.7,     # Adjust text size for variable labels
         tl.col = "black"  # Set variable label color to black
)









