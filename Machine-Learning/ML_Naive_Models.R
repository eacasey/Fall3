#Monday, Nov. 9, 2020
#Machine Learning - Naive Models
install.packages('e1071')
install.packages('gmodels')
library(tm) # Because we have a term-document matrix
library(e1071) # Where the naiveBayes function is found
library(gmodels) # Where the CrossTable function is found

setwd("~/Documents")

load("SMS_tdm.Rdata")
#' Let's try to use Naive Bayes to classify the text messages. Naive bayes is typically trained on data 
#' with categorical inputs. We want to think of our observations not as a collection of word frequencies, 
#' but merely a collection of words, regardless of how many times the word appeared in the text message. 
#' For the vast majority of cases, the word frequency will be 1 anyway. To do that, we will convert our 
#' columns to factors with two levels indicating the presence or absense of a word.
#' First we write a function that does this for one column, then we apply it to all columns:
#' In the apply function, margin=2 says to operate along columns as opposed to rows (margin=1)

convert_counts <- function(x)
{y = ifelse(x>0,"yes","no") 
return(y) }
sms_subset1 = apply(sms_tdm,2,convert_counts)

#' Then we can train the model on our training data and see how it performs on the test dataset:
#' 
set.seed(11117)
train=sample(c(T,F),nrow(sms_subset1), replace=T, p=c(0.75,0.25))

model = naiveBayes(sms_subset1[train,], sms_raw$type[train])
pred = predict(model,sms_subset1[!train,], type="class")
CrossTable(pred, sms_raw$type[!train], prop.chisq=F, prop.t = F, dnn = c('Predicted','Actual'))
# Accuracy on Test Data
mean(pred==sms_raw$type[!train])

#' Can we improve model performance? Having a high false positive rate is undesirable because
#' it could result in someone missing an important text because we thought it was spam. The
#' Laplace estimator is one dial that will help tune the model. When the Laplace estimator is 0,
#' words that did not appear in any spam messages has an indisputable influence on the classification -
#' because P(X_k|C_i) = 0 ==> P(X|C_i) = 0 (even if all other probabilities are large!)
#' but simply because the word 'hello' may have appeared only in ham messages from our sample, 
#' it does not mean that every message containing that word should be classified as ham. 
#' 
#' The Laplace correction (or estimator) essentially adds a small specified number to every cell count in 
#' our table of words by class. If the sample size is sufficient, this will do very little to alter the
#' predicted probabilities, expect when the predicted probability is 0 (which is undesirable).
#' 
#' Let's see how we do when we add in this correction:

model2 = naiveBayes(sms_subset1[train,], sms_raw$type[train], laplace = 1)
pred2 = predict(model2,sms_subset1[!train,], type="class")
CrossTable(pred2, sms_raw$type[!train], prop.chisq=F, prop.t = F, dnn = c('Predicted','Actual'))
# Accuracy on Test Data
mean(pred2==sms_raw$type[!train])

#' We've lowered our false positive rate and improved our overall performance. Let's see what a 
#' different Laplace correction might do - perhaps this is a parameter we ought to fiddle with?
#' Nope. Everything looks the same. We'll stick with the convention of Laplace = 1.

model3 = naiveBayes(sms_subset1[train,], sms_raw$type[train], laplace = 0.05)
pred3 = predict(model3,sms_subset1[!train,], type="class")
CrossTable(pred3, sms_raw$type[!train], prop.chisq=F, prop.t = F, dnn = c('Predicted','Actual'))

################################################################################
#' Original data consisted of 5574 Text messages, containing 7K+ unique words AFTER basic preprocessing
#' We then removed words that happened infrequently. The tm package made this easy.
#' This is feature selection. Only included words that occured at least 4 times in collection.
# That left 1833 words/features
load('/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Linear Algebra/Linear Algebra 2020/Code/sms_subset.Rdata')
# Some preliminary analysis with PCA:
library(tm)

train = sample(c(T,F),nrow(sms_raw),replace=T,p=c(0.9,0.1))
pca = prcomp(sms_subset[train,]) 

# Screeplot
plot(pca$sdev^2)


# Plot of the 1833 dimensional data projected onto a plane
plot(pca$x[,1:2], col=c(rgb(66/245, 1, 149/245,alpha=0.2),rgb(206/245, 66/245, 245/245,alpha=0.2))[sms_raw$type], pch=19)
legend(x='topleft', c('Ham','Spam'), pch=19, col=c(rgb(66/245, 1, 149/245,alpha=0.5),rgb(206/245, 66/245, 245/245,alpha=0.5)), pt.cex=1.5)

# Plot of the 1833 dimensional data projected onto 3-dimensional subspace
library(rgl)
library(car)
scatter3d(pca$x[,1],pca$x[,2],pca$x[,3], groups=sms_raw$type, surface=F)

# Let's see if we can use some principal components as input to a model.
# Let k be the number of components used:
k=3
new_data = data.frame(pca$x[,1:k])
new_data = cbind(sms_raw$type[train], new_data)
colnames(new_data)[1]="type"

# Make a logistic regression model:
model = glm(type ~ . , family="binomial", data=new_data)
summary(model)
# Accuracy on training data
(c=table(model$fitted.values>0.5,sms_raw$type[train])) 
(misclass=(c[1,2]+c[2,1])/5574)

# Score the test set according to principal components found on training data
# then create predictions from logistic regression model and check accuracy
g = data.frame(predict(pca,sms_subset[!train,]))
pred=predict(model, g, type="response")
mean((pred>0.5)==(sms_raw$type[!train]=='spam'))
(c=table(pred>0.5,sms_raw$type[!train])) 
(misclass=(c[1,2]+c[2,1])/5574)

#' Technical note: This is not necessarily the prescribed method for modelling this problem. 
#' It is merely an illustration of the power of dimension reduction to force related observations
#' and variables close to one another. 
#' 
#' We will revisit this dataset later in the semester. Naive Bayes Classifiers tend to be well suited
#' for this type of problem, although they can be far slower to implement with new data, which can be 
#' problematic in a fast-paced solution environment.
#' 

