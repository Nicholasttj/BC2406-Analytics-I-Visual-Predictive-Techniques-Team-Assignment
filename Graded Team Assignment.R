install.packages("data.table")
install.packages("ggplot2")
install.packages("MASS")
install.packages("dplyr")
install.packages("ragg")

# Load necessary libraries
library(data.table)  # For fast data manipulation
library(ggplot2)     # For data visualization
library(MASS)        # For ordinal regression if needed
library(dplyr)       # For data frame manipulation
library(ragg)

file_path <- file.choose()  # Opens file explorer to choose the file
lawsuit.dt <- fread(file_path)

# Convert necessary columns to factors automatically (no manual mapping)
lawsuit.dt[, Dept := as.factor(Dept)]    # Convert Dept to factor
lawsuit.dt[, Gender := as.factor(Gender)]  # Convert Gender to factor
lawsuit.dt[, Clin := as.factor(Clin)]    # Convert Clin to factor
lawsuit.dt[, Rank := as.factor(Rank)]    # Convert Rank to factor

# Check for missing values in Gender
if (any(is.na(lawsuit.dt$Gender))) {
  # Remove rows with missing Gender
  lawsuit.dt <- lawsuit.dt[!is.na(Gender)]
  warning("Rows with missing Gender have been removed.")
}


# Create a new column that calculates the average salary for 1994 and 1995
lawsuit.dt[, avg_salary := (Sal94 + Sal95) / 2]


# Create a table to count the number of males and females in each department
gender_dept_counts <- lawsuit.dt[, .N, by = .(Dept, Gender)]


# Graph 1: Regression analysis
model <- lm(Sal95 ~ Gender + Exper + Prate + Dept + Clin + Cert + Rank, data = lawsuit.dt)
summary(model)


# Graph 2: Bar Chart on Average Salary in 1994 & 1995 by Gender
ggplot(lawsuit.dt, aes(x = Gender, y = avg_salary, fill = Gender)) + 
  geom_boxplot() + 
  ggtitle("Average Salary in 1994 & 1995 by Gender") + theme_minimal()


# Graph 3: Create a box plot of the average salary by department
ggplot(lawsuit.dt, aes(x = Dept, y = avg_salary, fill = Dept)) +
  geom_boxplot() +                                          # Create the box plot
  labs(title = "Box Plot of Average Salary (1994 and 1995) by Department",
       x = "Department",
       y = "Average Salary (1994-1995)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Graph 4: Bar Plot of count of genders by department using ggplot
ggplot(gender_dept_counts, aes(x = Dept, y = N, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +         # Dodge bars side by side
  labs(title = "Gender Distribution by Department", 
       x = "Department", 
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))  # Rotate x-axis labels for clarity


# Graph 5: Bar Chart on the average salary in 1994 & 1995 by Department and Gender
ggplot(lawsuit.dt, aes(x = Gender, y = avg_salary, fill = Gender)) +
  geom_col() + scale_y_continuous(labels=scales::comma) +  # Bar plot without dodge for single bars
  labs(title = "Average Salary (1994 & 1995) by Department and Gender", 
       x = "Departments", 
       y = "Average Salary", 
       fill = "Gender") +
  facet_wrap(~Dept) +                                     # Separate plots for each department
  theme_minimal()


# Graph 6: Bar Chart on the count of Rank by Gender and Department
ggplot(lawsuit.dt, aes(x = Rank, fill = Gender)) +
  geom_bar(position = "dodge") +   # Dodge to place bars side by side
  labs(title = "Count of Rank by Gender and Department",
       x = "Rank",
       y = "Count",
       fill = "Gender") +
  facet_wrap(~Dept) +                                       # Separate plots for each department
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Summarise average experience by Rank, Gender, and Dept
avg_exper <- lawsuit.dt %>%
  group_by(Rank, Gender, Dept) %>%
  summarise(avg_exper = mean(Exper, na.rm = TRUE))

# Graph 7: Bar Chart Create the bar chart, faceted by Dept with meaningful labels
ggplot(avg_exper, aes(x = Rank, y = avg_exper, fill = Gender)) +
  geom_col(position = "dodge") +   # Bar plot, dodge to place bars side by side
  labs(title = "Average Experience by Rank, Gender, and Department",
       x = "Rank",
       y = "Average Years of Experience",
       fill = "Gender") +
  facet_wrap(~Dept) +              # Create separate plots for each Department
  theme_minimal()