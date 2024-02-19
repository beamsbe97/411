library(RSNNS)
library(reshape2)
library(ggplot2)

#setwd("C:/Users/annguyen/Downloads/411")

df = rbind(read.csv("gt_2011.csv"), read.csv("gt_2012.csv"))
df_test = rbind(read.csv("gt_2014.csv"), read.csv("gt_2015.csv"))

df = subset(df, select = -c(`GTEP`, `CDP`, `TIT`))
df_test = subset(df_test, select = -c(`GTEP`, `CDP`, `TIT`))

df[, 1:6] = scale(df[, 1:6])
df_test[, 1:6] = scale(df[, 1:6])


cormat = round(cor(df),2)
melted_cormat = melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()



# df_target = df[, c(7,8)]
# df_predictor = df[,1:6]

# df = splitForTrainingAndTest(df_predictor, df_target, ratio=0.15)
# df = normTrainingAndTestSet(df)

#model = mlp(df$inputsTrain, df$targetsTrain, size=6, learnFunc = "SCG", inputsTest=df$inputsTest, targetsTest=df$targetsTest)

model = mlp(x = as.matrix(df[, 1:6]),
            y = as.matrix(df[, c(7, 8)]),
            initFuncParams=c(-0.3,0.3),
            size = c(10, 8),  # Adjust the size of the hidden layers as needed
            learnFuncParams = c(0.001),
            maxit = 1000,      # Adjust the number of training iterations as needed
            inputsTest = as.matrix(df_test[, 1:6]),
            targetsTest = as.matrix(df_test[, c(7, 8)]),
            retRes = TRUE,
            linOut=TRUE)

# modelMlp <- mlp(patterns$inputsTrain, 
#                 patterns$targetsTrain, 
#                 initFuncParams=c(-0.3,0.3),
#                 size=c(8), 
#                 learnFuncParams=c(0.05), 
#                 maxit=500,
#                inputsTest=patterns$inputsTest, 
#                targetsTest=patterns$targetsTest, 
#                linOut=TRUE)


predictions <- predict(model, as.matrix(df_test[, 1:6]))

plotIterativeError(model)


rmse_co <- sqrt(mean((predictions[, 1] - df_test$CO)^2))
rmse_nox <- sqrt(mean((predictions[, 2] - df_test$NOx)^2))

print(paste("RMSE for CO:", rmse_co))
print(paste("RMSE for NOx:", rmse_nox))


