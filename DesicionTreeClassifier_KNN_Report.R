---
  title: "Decision Tree Classification Report"
output: html_document
---
  
  # Decision Tree Classification Report
  
  ## Importing the Dataset
  
  ```{r}
dataset = read.csv('Rice_Cammeo_Osmancik.csv')
dataset$Class = factor(dataset$Class, levels = c(0, 1))


library(caTools)
set.seed(123)
split = sample.split(dataset$Class, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


library(rpart)
classifier = rpart(formula = Class ~ ., data = training_set)
y_pred <- predict(classifier, newdata = test_set)



cm <- table(test_set$Class, y_pred)
cm_df <- as.data.frame(as.table(cm))
colnames(cm_df) <- c("Actual_Class", "Predicted_Class", "Frequency")

library(ggplot2)
# Plot the confusion matrix as a heatmap
confusion_heatmap <- ggplot(data = cm_df, aes(x = Actual_Class, y = Predicted_Class, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix", x = "Actual Class", y = "Predicted Class", fill = "Frequency") +
  theme_minimal()

print(confusion_heatmap)



y_pred1 = knn(train = training_set, test = test_set, cl = training_set$Class, k = 5)

cm_knn <- table(test_set$Class, y_pred1)
cm_df_knn <- as.data.frame(as.table(cm_knn))
colnames(cm_df_knn) <- c("Actual_Class", "Predicted_Class", "Frequency")

# Plot the confusion matrix for KNN
confusion_heatmap_knn <- ggplot(data = cm_df_knn, aes(x = Actual_Class, y = Predicted_Class, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix for KNN", x = "Actual Class", y = "Predicted Class", fill = "Frequency") +
  theme_minimal()

print(confusion_heatmap_knn)
