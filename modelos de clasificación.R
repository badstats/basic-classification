library(kknn)
library(rpart)
library(randomForest)
library(MASS)
library(e1071)
library(caret)

set.seed(1)


datos=read.csv("~/Mario/platónico/Programas/Semestre 7/Ciencia de datos/2-Clasificación/Maternal Health Risk Data Set.csv")
datos$RiskLevel=as.factor(datos$RiskLevel)




folds=createFolds(datos$RiskLevel, 10)
cv1=lapply(folds, function(x){
  train_fold=datos[-x,]
  test_fold=datos[x,]
  classifier=train.kknn(RiskLevel~., data=train_fold, kmax=12)
  y_pred=predict(classifier, newdata=test_fold[,-7])
  cm=table(test_fold[,7],y_pred)
  cm = as.matrix(cm)
  precision <- diag(cm) / rowSums(cm)
  recall <- diag(cm) / colSums(cm)
  accuracy <- sum(diag(cm)) / sum(cm)
  return(list(
    confusion_matrix = cm,
    best_k = classifier$best.parameters$k,
    precision = precision,
    recall = recall,
    accuracy = accuracy
  ))
})
cv1


cv2=lapply(folds, function(x){
  train_fold=datos[-x,]
  test_fold=datos[x,]
  classifier=rpart(RiskLevel~., data=train_fold)
  y_pred=predict(classifier, newdata=test_fold[,-7], type="class")
  cm=table(test_fold[,7],y_pred)
  cm = as.matrix(cm)
  precision <- diag(cm) / rowSums(cm)
  recall <- diag(cm) / colSums(cm)
  accuracy <- sum(diag(cm)) / sum(cm)
  return(list(
    confusion_matrix = cm,
    best_k = classifier$best.parameters$k,
    precision = precision,
    recall = recall,
    accuracy = accuracy
  ))
})
cv2


cv3=lapply(folds, function(x){
  train_fold=datos[-x,]
  test_fold=datos[x,]
  classifier=randomForest(RiskLevel~., data=train_fold)
  y_pred=predict(classifier, newdata=test_fold[,-7], type="class")
  cm=table(test_fold[,7],y_pred)
  cm = as.matrix(cm)
  precision <- diag(cm) / rowSums(cm)
  recall <- diag(cm) / colSums(cm)
  accuracy <- sum(diag(cm)) / sum(cm)
  return(list(
    confusion_matrix = cm,
    best_k = classifier$best.parameters$k,
    precision = precision,
    recall = recall,
    accuracy = accuracy
  ))
})
cv3


cv4=lapply(folds, function(x){
  train_fold=datos[-x,]
  test_fold=datos[x,]
  classifier=polr(RiskLevel~., data=train_fold)
  y_pred=predict(classifier, newdata=test_fold[,-7])
  cm=table(test_fold[,7],y_pred)
  cm = as.matrix(cm)
  precision <- diag(cm) / rowSums(cm)
  recall <- diag(cm) / colSums(cm)
  accuracy <- sum(diag(cm)) / sum(cm)
  return(list(
    confusion_matrix = cm,
    best_k = classifier$best.parameters$k,
    precision = precision,
    recall = recall,
    accuracy = accuracy
  ))
})
cv4


cv5=lapply(folds, function(x){
  train_fold=datos[-x,]
  test_fold=datos[x,]
  classifier=svm(RiskLevel~., data=train_fold, type="C-classification")
  y_pred=predict(classifier, newdata=test_fold[,-7])
  cm=table(test_fold[,7],y_pred)
  cm = as.matrix(cm)
  precision <- diag(cm) / rowSums(cm)
  recall <- diag(cm) / colSums(cm)
  accuracy <- sum(diag(cm)) / sum(cm)
  return(list(
    confusion_matrix = cm,
    best_k = classifier$best.parameters$k,
    precision = precision,
    recall = recall,
    accuracy = accuracy
  ))
})
cv5

