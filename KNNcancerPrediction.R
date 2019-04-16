# import the dataset
dataset <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

#structure of the dataset
str(dataset)

# drop the id feature
dataset<-dataset[-1]

# table of diagnosis
table(wbcd$diagnosis)

# recode diagnosis as a factor
dataset$diagnosis <- factor(dataset$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))


# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# normalize the wbcd data
dataset_n<-as.data.frame(lapply(dataset[2:31], normalize))

# confirm that normalization worked
summary(dataset_n$area_mean)

# create training and test data
dataset_train <- dataset_n[1:469, ]
dataset_test <- dataset_n[470:569, ]

# create labels for training and test data

dataset_train_labels <- dataset[1:469, 1]
dataset_test_labels <- dataset[470:569, 1]

## Step 3: Training a model on the data ----

# load the "class" library
library(class)

dataset_test_pred <- knn(train = dataset_train, test = dataset_test,
                      cl = dataset_train_labels, k = 21)

## Step 4: Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = dataset_test_labels, y = dataset_test_pred,
           prop.chisq = FALSE)

