load("PenDigits.Rdata")
pendigits
library('randomForest')
rf = randomForest(digit ~ .-digit, data=train, ntree=50, type='class')

# We can then examine the confusion matrix to see how our model predicts each class:

rf$confusion

#The classwise error rates are extremely small for the entire model! \blue{rf\$err.rate} will show the progression of misclassification rates as each individual tree is added to the forest for each class and overall (on the out of bag (OOB) data that was remaining after the data was sampled to train the tree).

#rf$err.rate
head(rf$err.rate)
rf$err.rate[50,]

#The final misclassification rate on the out of bag data was just 0.01 using the ensemble of 50 trees! We might want to look at what digits we are struggling with to see if there is any pattern.

library(lattice)
wrong=train[rf$predicted!=train$digit,]
barchart(wrong$digit, main = 'Frequency of Errors by Digit',
         xlab = 'Number of Errors', ylab='Digit', col = 'orange')


#Finally we should check our random forest model on the validation model as a final test of performance and screen for overfitting.

vscores = predict(rf,test,type='class')
cat('Validation Misclassification Rate:', sum(vscores!=test$digit)/nrow(test))

