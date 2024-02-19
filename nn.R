library(RSNNS)

setwd("C:/Users/annguyen/Downloads/411")

df = rbind(read.csv("gt_2011.csv"), read.csv("gt_2012.csv"))

df_target = df[,10:11]
df_predictor = df[,1:9]

df = splitForTrainingAndTest(df_predictor, df_target, ratio=0.15)
df = normTrainingAndTestSet(df)

model = mlp(df$inputsTrain, df$targetsTrain, size=6, learnFunc = "SCG", inputsTest=df$inputsTest, targetsTest=df$targetsTest)

