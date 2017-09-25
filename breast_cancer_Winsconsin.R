# Breast Cancer Winsconsin problem solved by Guilherme Passos - Classification
# Federal University of Minas Gerais

###############################

# A solution of the Breast Cancer Winsconsin classification problem using the gaussian mixture model from mclust library


BreastCancerWinsconsin <- function(BreastCancer, trainRatio){
  
  # Loading packgaes
  library('RSNNS')
  library('mclust')
  
  # Formatting, mixing and spliting the Winsconsin Breast Cancer DataBase
  X <- data.matrix(BreastCancer)
  X[is.na(X)] <- 0
  X <- X[sample(nrow(X),replace=F,size=nrow(X)),]
  X <- splitForTrainingAndTest( X[,2:10],  X[,11], ratio=trainRatio)
  
  # Getting parameters of class benign (Gaussian Mixture Model)
  benign <- which(X$targetsTrain==1)
  modelBenign <- densityMclust(X$inputsTrain[benign,])
  
  # Getting parameters of class  malignant (Gaussian Mixture Model)
  malignant <- which(X$targetsTrain==2)
  modelMalignant <- densityMclust(X$inputsTrain[malignant,])
  
  # Posterior probabilities  for each class
  PosterioriBenign <- dens(modelName=modelBenign$modelName, data = X$inputsTest, parameters = modelBenign$parameters)
  PosterioriMalignant <- dens(modelName=modelMalignant$modelName, data = X$inputsTest, parameters = modelMalignant$parameters)
  
  # checking which class has the higher posterior probability 
  predictions <- array(length(PosterioriBenign))
  for (i in 1:length(PosterioriBenign)){
    if (PosterioriBenign[i]>PosterioriMalignant[i]) predictions[i]<-1
    else predictions[i]<-2
  }
  
  # 
  pred <- list(xTrain=X$inputsTrain,yTrain = X$targetsTrain,xTest=X$inputsTest,yTest =X$targetsTest,pred= predictions)
  
  return (pred)
  
}

# ============================= ALGORTIHM VALIDATION ============================= 
  
  
data("BreastCancer")

ACC <- vector()
MSE <- vector()

for (i in 1:10){

  solution <- BreastCancerWinsconsin(BreastCancer,0.3)

  # Printing confusion Matrix
  cm <- RSNNS::confusionMatrix(solution$yTest,solution$pred)
  cm

  # Evaluating the gaussian mixture model's quality
  # Accuaracy
  ACC[i] <- sum(diag(cm))/sum(cm)
  # Mean squared Error
  MSE[i] <- mean((solution$yTest-solution$pred)^2)
}

