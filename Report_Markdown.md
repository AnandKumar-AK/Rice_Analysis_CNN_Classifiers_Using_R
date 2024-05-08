---
title: "Classification and Regression Analysis for Rice Varieties"
output: html_document
---

### Classification and Regression Analysis for Rice Varieties

The report presents the application of decision tree classification, linear regression, and K-Nearest Neighbors algorithms to a dataset containing information about rice varieties, specifically the Cammeo and Osmancik varieties. The dataset includes columns such as Area, Perimeter, Major Axis Length, Minor Axis Length, Eccentricity, Convex Area, Extent, and Class. The target variable "Class" is encoded as a factor with Cammeo and Osmancik labeled as 1 and 0, respectively.

### Decision Tree Classification
The dataset is split into training and test sets using a 75-25 split ratio. A decision tree classifier is then trained on the training set and evaluated on the test set. The confusion matrix is computed and visualized as a heatmap and a bar plot to assess the classification performance.

### K-Nearest Neighbors (KNN)
KNN algorithm is used to classify the rice varieties based on the features provided in the dataset. The optimal number of neighbors (k) is determined using cross-validation, and the model's performance is evaluated using the confusion matrix and classification accuracy.


### Linear Regression
A linear regression model is applied to the dataset to predict the Class variable based on the other features. The model's performance is evaluated using metrics such as Mean Squared Error  and R-squared.

The analysis provides insights into the classification and regression performance of different algorithms for predicting rice varieties based on various features.

#dataset
dataset = read.csv('Rice_Cammeo_Osmancik.csv')

dataset$Class = factor(dataset$Class, levels = c(0, 1))
print(dataset$Class)


library(caTools)
set.seed(123)
split = sample.split(dataset$Class, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


library(rpart)
classifier = rpart(formula = Class ~ ., data = training_set)


y_pred <- predict(classifier, newdata = test_set)
print(y_pred)
y_pred_subset <- y_pred[1:953]


cm <- table(test_set$Class, y_pred_subset)
length(test_set$Class)
length(y_pred)


# Making the Confusion Matrix
cm <- table(test_set$Class, y_pred_subset)

length(test_set$Class)
length(y_pred)




install.packages("ggplot2")
install.packages("plotly")
library(ggplot2)
library(plotly)

print(cm)

# Convert the confusion matrix to a data frame
cm_df <- as.data.frame(cm)
library(ggplot2)

# Convert the confusion matrix to a data frame
cm_df <- as.data.frame(as.table(cm))
colnames(cm_df) <- c("Actual_Class", "Predicted_Class", "Frequency")

# Plot the confusion matrix as a heatmap
library(ggplot2)
ggplot(data = cm_df, aes(x = Actual_Class, y = Predicted_Class, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +  # Color gradient for heatmap
  labs(title = "Confusion Matrix",
       x = "Actual Class",
       y = "Predicted Class",
       fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



# Create a data frame from the confusion matrix
cm_df <- as.data.frame(as.table(cm))
names(cm_df) <- c("Actual_Class", "Predicted_Class", "Frequency")

# Plot the confusion matrix as a bar plot
ggplot(data = cm_df, aes(x = Actual_Class, y = Frequency, fill = Predicted_Class)) +
  geom_bar(stat = "identity") +
  labs(title = "Confusion Matrix",
       x = "Actual Class",
       y = "Frequency",
       fill = "Predicted Class") +
  theme_minimal()
  
#KNN
# Fitting K-NN to the Training set and predicting the test set results
install.packages('class')
library(class)
y_pred1 = knn(train = training_set,
             test = test_set,
             cl = training_set$Class,
             k = 5)
y_pred1

# Extract the predicted values from y_pred1 (assuming y_pred1 is a vector)
y_pred_values <- y_pred1

# Extract the actual values from test_set
actual_values <- test_set$Class  # Class is the column name

# Create the confusion matrix
cm_knn <- table(actual_values, y_pred_values)


# Convert the confusion matrix to a data frame
cm_df_knn <- as.data.frame(cm_knn)
colnames(cm_df_knn) <- c("Actual_Class", "Predicted_Class", "Frequency")

# Plot the confusion matrix as a heatmap
library(ggplot2)
ggplot(data = cm_df_knn, aes(x = Actual_Class, y = Predicted_Class, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +  # Color gradient for heatmap
  labs(title = "Confusion Matrix",
       x = "Actual Class",
       y = "Predicted Class",
       fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability




# Fitting K-NN to the Training set and predicting the test set results
#install.packages('class')
library(class)
y_pred1 = knn(train = training_set,
             test = test_set,
             cl = training_set$Class,
             k = 5)
y_pred1

#install.packages("cowplot")

# Load required packages
library(ggplot2)
library(cowplot)  # For arranging plots


# Create a density plot of predicted values
density_plot <- ggplot(data = data.frame(Predicted = y_pred1), aes(x = Predicted)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Density Plot of Predicted Values", x = "Predicted Value", y = "Density") +
  theme_minimal()

# Display the plot
print(density_plot)


# Create a scatter plot of actual vs. predicted values
actual_vs_predicted_plot <- ggplot(data = data.frame(Actual = test_set$Class, Predicted = y_pred1), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  labs(title = "Actual vs. Predicted Values", x = "Actual Value", y = "Predicted Value") +
  theme_minimal()

# Display the plot
print(actual_vs_predicted_plot)

  
# regression
dataset <- read.csv('Rice_Cammeo_Osmancik.csv')

# Fitting Linear Regression to the dataset
regressor <- lm(formula = dataset$Class ~ ., data = dataset)

# Predicting the results
y_pred_lr <- predict(regressor, newdata = dataset)

# Evaluating the model
summary(regressor)

# Plotting the results
plot(dataset$Class, y_pred_lr, main = "Actual vs Predicted", xlab = "Actual", ylab = "Predicted")

# Residuals vs Fitted plot
plot(regressor)



rmarkdown::render("Report_Markdown.Rmd")

  
  
  
  
