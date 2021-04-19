#Q1
library(factoextra)
crimedf <- read.table('uscrime.txt', header = TRUE)
factors <- crimedf[,-16]

naive <- lm(Crime~.,crimedf )

pca <- prcomp(factors, scale. = TRUE )

variances <- fviz_eig(pca)
summary <- summary(pca)
firstsix <- pca$x[ ,1:6]

sixandresponse = data.frame(cbind(firstsix,Crime = crimedf[,16]))


head(sixandresponse)

regression <- lm(Crime~.,sixandresponse)

summary(regression)

new <- data.frame( M = 14.0 , So = 0.0 , Ed = 10.0 , Po1 = 12.0,
                   Po2 = 15.5 , LF = 0.640 , M.F = 94.0 , Pop = 150.0 , 
                   NW = 1.1 , U1 = 0.120 , U2 = 3.6 , Wealth = 3200.0 , 
                   Ineq = 20.1 , Prob = 0.04 , Time = 39.0 )


Z <- as.matrix(regression$coefficients[-1])
V <- as.matrix(pca$rotation[,1:6])
X <- V %*% Z
intercept <- regression$coefficients[1]-sum(X * (pca$center /pca$scale))
coef <- X/pca$scale

finalpcr <- intercept + (as.matrix(new) %*% coef)

library (pls)


pcrtest <- pcr(Crime~., data = crimedf,ncomp = 6,scale = TRUE, )

otherway <- predict(pcrtest , new, ncomp = 6)
fit.pcr <- fitted(pcrtest)
#Q2
#regression tree model
library(tree)
library(randomForest)
tree <- tree(Crime~., crimedf)

yhattree <- predict(tree)
pruned <- prune.tree(tree)

set.seed(1)
tree_cv = cv.tree(tree)
plot(tree_cv$size, sqrt(tree_cv$dev / nrow(crimedf)), type = "b",
     xlab = "Tree Size", ylab = "CV-RMSE")

#random forests
#randomforest <- randomForest(Crime~.,crimedf)

library(randomForest)
#random forests


set.seed(100)
sample <- sample(nrow(crimedf), round(0.9*nrow(crimedf)), replace = FALSE)
train <- crimedf[sample,]
test <- crimedf[-sample,]

randomforest <- randomForest(Crime~.,train, ntree = 50)

SSE <- function(model,test) {
  guess <- predict(model, test)
  actual <- test[,16]
  SSE <- sum((guess-actual)^2)
  return(SSE)
}

for (i in seq(1,200,10)) {
  model <- randomForest(Crime~., train, ntree = i)
  a<-SSE(model, test)
  #cat('ntree:',i,'SSE',a,'\n')
 
}


# logistic regression
library(fastDummies)

#read in data
germancredit <- read.table('germancredit.txt')

#convert response variable to binary, and categorical variables to dummy binary variables
germancredit$V21[germancredit$V21 == 1] <- 0
germancredit$V21[germancredit$V21 == 2] <- 1
germancredit <- dummy_cols(germancredit, remove_selected_columns = TRUE, remove_most_frequent_dummy  = TRUE)

#create train and test set
set.seed(10)
sample <- sample(nrow(germancredit), round(0.8*nrow(germancredit)), replace = FALSE)
train <- germancredit[sample,]
test <- germancredit[-sample,]


logistic <- glm(formula = V21~. , train, family=binomial(link='logit'))

prediction <- (predict.glm(logistic, test, type = 'response'))
threshold = .7
actual <- test$V21

pred <- round(prediction-threshold + .5)
CM <- table(actual,pred)

cost <- CM[2,1] + 5*CM[1,2]

getcost <- function(t=.5,model = logistic, data = test) {
  prediction <- (predict.glm(model, data, type = 'response'))
  
  actual <- test$V21
  
  pred <- round(prediction-t + .5)
  CM <- table(actual,pred)
  
  cost <- CM[2,1] + 5*CM[1,2]
  
  return(cost)
}

for (i in seq(.01,.95,.01)){
  cat('Threshold:',i,'Cost:',getcost(t=i),'\n')
}
