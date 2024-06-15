################################################################################################
# Simple example of how to use caret to optimize parameters in a BRT
################################################################################################

library(caret)
library(gbm)
library(pROC) 
library(dismo) 
setwd("C:\\Users\\nereo\\OneDrive\\Escritorio\\Barb\\")

# Load data
fishData <- read.csv("sardDataForModelTraining.csv", head = TRUE, sep = ",")
fishData=data

# Subset just the data we need
fishData <- fishData[c("PA", "o2.surf", "o2.bot","ph.surf","ph.bot","phyc.vint","siconc","so.surf",
                       "so.bot","tos","tob")]
fishData=na.omit(fishData)

# Split into test/train
index <- sample(1:nrow(fishData), round(0.5 * nrow(fishData))) 
train <- fishData[index,]
test <- fishData[-index,]
dim(train)
dim(test)

# Set optimization options using caret
fitControl <- trainControl(method = "cv", number = 5) # Can be very slow with high "number"
# I used a "lr.coeff" to estimate sensible values of learning rate to test, but this dataset
# is only about 1000 observations, the relationship may not work for much larger datasets!
lr.coeff <- round(0.0000017 * nrow(train) - 0.0001912, 4) 
# Set the range of options for each parameter: I'm varying interaction.depth (= tree complexity),
# shrinkage (= learning rate), and n.trees
gbmGrid2 <- expand.grid(interaction.depth = seq(2, 5, by = 1), 
                       n.trees = seq(430000, 440000, by = 10000), 
                       shrinkage = (0.01:0.1)*lr.coeff, n.minobsinnode = 10)

# Now test which combination of parameters works best. For some reason, the presence/absence
# variable must be a factor
set.seed(123)
gbmFit1 <- caret::train(factor(PA) ~ o2.surf + o2.bot + ph.surf + ph.bot+phyc.vint+siconc+so.surf+so.bot+tos+tob, 
                        data = train, method = "gbm", trControl = fitControl, 
                        verbose = FALSE, tuneGrid = gbmGrid)

# You can plot the results: ideally, a shrinkage value (= learning rate) somewhere in the middle
# of the options you provided will be chosen, otherwise you may need to expand the range
plot(gbmFit1)

# Save the best values for learning rate, tree complexity, and no. trees
lr.best <- gbmFit1$bestTune$shrinkage
tc.best <- gbmFit1$bestTune$interaction.depth
n.trees.best <- gbmFit1$bestTune$n.trees

# Build final model
my.gbm <- gbm.fixed(data = train, gbm.x = 2:5, gbm.y = 1, family = "bernoulli", 
                    tree.complexity = tc.best, learning.rate = lr.best, bag.fraction = 0.6, 
                    n.trees = n.trees.best)
# Predict
test$pred <- predict(my.gbm, test, n.trees = n.trees.best)
# AUC against withheld data
myAUC <- roc(test$sardpa, test$pred)
myAUC$auc
