#Decision Tree Classification

# Importing the dataset
dataset = read.csv('Rice_Cammeo_Osmancik.csv')

# Encoding the target feature as factor
dataset$Class = factor(dataset$Class, levels = c(0, 1))

print(dataset$Class)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Class, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)



# Fitting Decision TreeClassification to the Training set
#install.packages('rpart')
library(rpart)
classifier = rpart(formula = Class ~ ., data = training_set)

# Predicting the Test set results
y_pred <- predict(classifier, newdata = test_set)


print(y_pred)
y_pred_subset <- y_pred[1:953]

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
install.packages('class')
library(class)
y_pred1 = knn(train = training_set,
             test = test_set,
             cl = training_set$Class,
             k = 5)
y_pred1

install.packages("cowplot")

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




