# Install necessary packages
install.packages("readxl")
install.packages("e1071")
install.packages("psych")

# Load the libraries
library(readxl)
library(e1071)
library(psych)
library(ggplot2)

# Load the dataset from an Excel file
Bodyfat_data<- read_excel("C://Users//HP//Documents//Assesment-2//assesement -2/Dataset_2024.xlsx")

# Display the structure and summary of the dataset
str(Bodyfat_data)
View(Bodyfat_data)

#Summary of the data
summary(Bodyfat_data)

# Describing the dataset
desc_stats <- describe(Bodyfat_data)
print(desc_stats)

# Check for missing values = 0
sum(is.na(Bodyfat_data))

# Renaming columns
colnames(Bodyfat_data)[colnames(Bodyfat_data) == "Body fat (%)"] <- "Body fat"
colnames(Bodyfat_data)[colnames(Bodyfat_data) == "Age (years)"] <- "Age"
colnames(Bodyfat_data)[colnames(Bodyfat_data) == "Knee circumference (cm)"] <- "Knee circumference"
colnames(Bodyfat_data)[colnames(Bodyfat_data) == "Chest circumference (cm)"] <- "Chest circumference"
colnames(Bodyfat_data)[colnames(Bodyfat_data) == "Density (g/cm³)"] <- "Density"
colnames(Bodyfat_data)[colnames(Bodyfat_data) == "Weight (lbs)"] <- "Weight"

str(Bodyfat_data)

# Using pairplots for linearilty
windows(20, 12)
pairs(Bodyfat_data, smooth = FALSE, scale = FALSE, density = TRUE, ellipses = FALSE, 
      method = "spearman", pch = 21, lm = FALSE, cor = TRUE, jiggle = FALSE, 
      factor = 2, hist.col = 4, stars = TRUE, ci = TRUE)

# Checking the relationships between Body Fat and other columns using scatter plot
windows(20, 12)
par(mfrow = c(4, 2))

scatter.smooth(x = Bodyfat_data$Age, y = Bodyfat_data$`Body fat`,
               xlab = "Age", ylab = "Body fat", 
               main = "Correlation for Body Fat and Age")

scatter.smooth(x = Bodyfat_data$`Chest circumference`, y = Bodyfat_data$`Body fat`,
               xlab = "Chest Circumference", ylab = "Body fat", 
               main = "Correlation for Body Fat and Chest Circumference")

scatter.smooth(x = Bodyfat_data$Density, y = Bodyfat_data$`Body fat`,
               xlab = "Density", ylab = "Body fat", 
               main = "Correlation for Body Fat and Density")

scatter.smooth(x = Bodyfat_data$`Knee circumference`, y = Bodyfat_data$`Body fat`,
               xlab = "Knee Circumference", ylab = "Body fat", 
               main = "Correlation for Body Fat and Knee Circumference")

scatter.smooth(x = Bodyfat_data$Weight, y = Bodyfat_data$`Body fat`,
               xlab = "Weight", ylab = "Body fat", 
               main = "Correlation for Body Fat and Weight")

# correlation matrix for the dataset
correlation_matrix <- cor(Bodyfat_data)
correlation_matrix

# Attach the dataset frame
attach(Bodyfat_data)

# Calculating the correlations between Body Fat and other columns
cat("Correlation for Body Fat & Age: ", round(cor(`Body fat`, Age), 2), "\n")
cat("Correlation for Body Fat & Chest Circumference: ", round(cor(`Body fat`, `Chest circumference`), 2), "\n")
cat("Correlation for Body Fat & Density: ", round(cor(`Body fat`, Density), 2), "\n")
cat("Correlation for Body Fat & Knee Circumference: ", round(cor(`Body fat`, `Knee circumference`), 2), "\n")
cat("Correlation for Body Fat & Weight: ", round(cor(`Body fat`, Weight), 2), "\n")

#Checking the skewness of the data for normality
skewness(Bodyfat_data$`Body fat`)

#plotting the ggplot
ggplot(Bodyfat_data, aes(x = Age, y = `Body fat`)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x)

shapiro.test(`Body fat`)
shapiro.test(Age)
shapiro.test(Density)
shapiro.test(`Knee circumference`)
shapiro.test(`Chest circumference`)

shapiro.test(Bodyfat_data$`Body fat`)

Bodyfat_data$log_bodyfat <- log(Bodyfat_data$`Body fat`)

# Fitting linear model 
Final_data <- lm(`Body fat` ~ Age + Density + `Knee circumference` + `Chest circumference`, data = Bodyfat_data)

# Summary of the linear model
summary(Final_data)

library(car)
#checkin for the multicollinearity
vif(Final_data)

final_model <- lm(`Body fat` ~ Age + `Chest circumference`+ Weight, data = Bodyfat_data)
summary(final_model)

#Plotting the final model 
plot(final_model)

#Comparing the two model 
AIC(Final_data,final_model)





