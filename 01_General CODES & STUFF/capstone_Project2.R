
  # Introduction
  ## Overivew

# Install all needed libraries if it is not present
setwd(getwd())
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(dplyr)) install.packages("dplyr") 
if(!require(data.table)) install.packages("data.table")
if(!require(psych)) install.packages("psych") 
if(!require(ggplot2)) install.packages("ggplot2") 
if(!require(caret)) install.packages("caret")
if(!require(bigleaf)) install.packages("bigleaf") 
if(!require(matrixStats)) install.packages("matrixStats")
if(!require(doSNOW)) install.packages("doSNOW")
if(!require(lubridate)) install.packages("lubridate") 
if(!require(stringr)) install.packages("stringr") 


## Kaggle Data Set 
### Download the Texas Wind Turbine Dataset - Simulated 
PowerCurve <-
  data.table(read.csv("./Data/GE Turbine Power Curve.csv"))
PowerCurve <- setnames(
  PowerCurve,
  c(
    "Power.curve.turbine.output.array....kW.",
    "Power.curve.wind.speed.array....m.s."
  ),
  c("power.kWh", "wind.bin")
)

wtgData <- data.table(read.csv("./Data/TexasTurbine.csv"))
wtgData <- setnames(
  wtgData,
  c(
    "Time.stamp",
    "System.power.generated....kW.",
    "Wind.speed....m.s.",
    "Wind.direction....deg.",
    "Pressure....atm.",
    "Air.temperature.....C."
  ),
  c(
    "timestamp",
    "active.power",
    "wind.speed",
    "wind.dir",
    "AirPressure",
    "AirTemperature"
  )
)



# Exploratory Data Analysis

# PowerCurve
str(PowerCurve)
head(PowerCurve, 10)


# Turbine data
str(wtgData)
head(wtgData, 10)



# for more information on the psych package: https://cran.r-project.org/web/packages/psych/index.html
#lets look at a Summary of Data
PPC <- psych::describe(PowerCurve, skew = FALSE)
Pwtg <- psych::describe(wtgData, skew = FALSE)

PPC
Pwtg




## GE Wind turbine Power Curve Chart

# GE Wind turbine Power Curve Chart
p <- ggplot(data = PowerCurve, aes(x = wind.bin, y = power.kWh)) +
  geom_col(aes(color = 'wind.bin'), color = "blue") +
  geom_line(
    data = PowerCurve,
    aes(x = wind.bin, y = power.kWh, group = 1),
    color = "red",
    size = 1
  ) +
  theme_light()

p + labs(title = "WTG Power Curve",
         subtitle = "GE 3600 mw")


# Preparing the Data for Analysis

## Wind Turbulence
# wind.speed standard deviation / wind.speed
wtgData$Turbulence <- sd(wtgData$wind.speed) / wtgData$wind.speed


## AIR DENSITY CALCULATION for each 1hr data set
ADvect <-
  air.density(Tair = wtgData$AirTemperature,
              pressure = wtgData$AirPressure)

mAD <- mean(ADvect[ADvect > 0], na.rm = TRUE)
hrs <- nrow(wtgData) * (10 / 60)


#### AIR DENSITY NORMALIZATION TO 1.225 KG/M^3 ###
NormWS <-
  (wtgData$wind.speed * ((ADvect / 1.225) ^ (1 / 3))) # wind.speed NORMALIZATION
Pmeas <- as.vector(wtgData$active.power) # mean active.power
Norm <- cbind(Pmeas, wtgData$wind.speed , NormWS, ADvect)
colnames(Norm) <- c("Mpower", "UnNormWS", "NormWS", "Air.Density")
Norm <- data.frame(Norm)
Norm$wind.bin <-
  round(Norm$UnNormWS, digits = 0) # add measured wind bin


### INTERPOLATION OF REFERENCE POWER CURVE TO MATCH BIN WIND SPEEDS ###
ToInterpolate <- merge(Norm, PowerCurve, by = "wind.bin")
colnames(ToInterpolate) <-
  c("Bins",
    "AP.Mean",
    "MeasuredWS",
    "Normailzed.Wind",
    "Air.Density",
    "RefPower")

ToInterpolate <- ToInterpolate %>% group_by(Bins) %>%
  summarise(MeasuredWS = mean(MeasuredWS),
            RefPower = mean(RefPower))

InterResult <-
  approx(
    ToInterpolate$Bins,
    y = ToInterpolate$RefPower,
    ToInterpolate$MeasuredWS,
    method = "linear"
  )
# xx <- as.data.frame(xxx[2])
interpolated <-
  cbind(ToInterpolate, as.data.frame(InterResult[2]))
colnames(interpolated)[4] <- "InterPower"
colnames(interpolated)[1] <- "wind.bin"
#### JOIN BINNED DATA RESULTS ###
Norm <- merge(Norm, interpolated, by = "wind.bin")
Norm <- Norm[, c(-3)]

# now lets pivot out data.frame and estimate overall wind power curve data
Norm <- Norm %>% group_by(wind.bin) %>%
  summarise(
    Mpower = mean(Mpower),
    NormWS = mean(NormWS),
    Air.Density = mean(Air.Density),
    MeasuredWS = mean(MeasuredWS),
    RefPower = mean(RefPower),
    InterPower = mean(InterPower)
  )


#### CAPACITY FACTOR CALCULATION ###
# this will be calculated for each row in Norm file :: https://calculator.academy/capacity-factor-calculator/
# total possible electrical output (tpe)
tpe <- 1 * 24 * Norm$RefPower
# actual electrical output of the plant over the same (aeo)
aeo <- Norm$Mpower
# Finally, using the formula CF = (AEO / MEO) * 1000, calculate the capacity factor.
CF <- (aeo / tpe) * 1000
Norm$CF <- CF

Norm$SDpower <- rowSds(as.matrix(Norm[, c(2, 7)]))
Norm[is.nan(as.matrix(Norm))] <- 0
Norm <-
  Norm %>% mutate_if(is.numeric, function(x)
    ifelse(is.infinite(x), 0, x))

Norm


## Now lets add our Normalization file to our master data file joined by the the wind.bin

wtgData$wind.bin <- round(wtgData$wind.speed, digits = 0) # add measured wind bin
wtgData <- merge(wtgData, Norm, by = "wind.bin")


## Correlation Matrix 

wtgData1 <- wtgData[, c(-1,-2)] #drop time stamp
set.seed(127)
correlationMatrix <- cor(wtgData1[, 2:13])

# summarize the correlation matrix
#print(correlationMatrix)

# find attributes that are highly correlated (ideally > 0.5)
highlyCorrelated <-
  findCorrelation(correlationMatrix, cutoff = 0.5, names = T)

print(highlyCorrelated)

# remove features that are not highly correlated
corr.data <- subset(wtgData1, select = highlyCorrelated)
wtgData1 <- cbind(active.power = wtgData1$active.power, corr.data)




## WTG Data Scatter plot of matrices 

psych::pairs.panels(
  wtgData1[, -1],
  method = "pearson",
  # correlation method
  hist.col = "#00AFBB",
  density = TRUE,
  # show density plots
  ellipses = TRUE,
  # show correlation ellipses
  lm = TRUE
)



# Build Models

# set train and test data-------------------------------------
set.seed(42)
index <-
  createDataPartition(wtgData1$active.power,
                      # change genbrg$xxxxxxx
                      times = 1,
                      p = .80,
                      list = FALSE)
train <- wtgData1[index,]
test  <- wtgData1[-index,]

# Run algorithms using 10-fold cross validation
metric <- "RMSE"


## Conditional Inference Trees

set.seed(123)
tune <- expand.grid(.mincriterion = .95, .maxdepth = as.integer(seq(2, 10, 2)))

fit.ctree <- train(active.power ~.,
                  data = train,
                  method = "ctree2",
                  metric = metric,
                  preProcess = c("center", "scale"),
                  tuneGrid = tune,
                  trControl = trainControl(method="repeatedcv", number = 10, repeats = 3, search = "grid")
                  )
saveRDS(fit.ctree, "./Models/fit.ctree.rds")

fit.ctree <- readRDS("./Models/fit.ctree.rds")
data.frame(metric = "ctree", RMSE = min(summary(fit.ctree$results$RMSE)))
plot(fit.ctree)



## GBM Gradient Boosted Machine
# Using caret with the default grid to optimize tune parameters automatically
# GBM Tuning parameters:
# n.trees (# Boosting Iterations)
# interaction.depth (Max Tree Depth)
# shrinkage (Shrinkage)
# n.minobsinnode (Min. Terminal Node Size)

caretGrid <- expand.grid(shrinkage = c(.001, .01, .05, .1),
                         interaction.depth = c(1, 3, 5, 7),
                         n.minobsinnode = c(5, 7, 10, 15),
                         n.trees = c(500, 1000, 1500))

set.seed(99)
fit.gbm <- train(active.power ~ ., 
                   data = train, 
                   distribution = "gaussian",
                   method = "gbm",
                   trControl = trainControl(method="cv", number=10, repeats = 3, search = "random"),
                   verbose = FALSE,
                   metric = metric,
                   bag.fraction = c(.65, .8, 1), 
                   tuneGrid = caretGrid
                   )                             
saveRDS(fit.gbm, "./Models/fit.gbm.rds")

fit.gbm <- readRDS("./Models/fit.gbm.rds")
data.frame(metric = "ctree", RMSE = min(summary(fit.gbm$results$RMSE)))
plot(fit.gbm)


## Random Forest

# Regression: Prediction is the average prediction across the decision trees.
# Good site for overall Random Forest guide lines: https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
set.seed(1234)
trControl <- trainControl(method = "cv", number = 10, repeats = 3)
tuneGrid <- expand.grid(.mtry = c(1:10))

#will run with parallel computing with library(doSNOW) Package
# Now building the model with parallel computing concept, 
# Because I have quad core machine, I specify three as the number of clusters.

# Register cluster so that caret will know to train in parallel.
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

fit.rf <- train(active.power ~. , 
	             data = train, 
	             method = "rf", 
	             metric = metric,
	             tuneGrid = tuneGrid,
	             ntrees = c(500, 1000),
              nodesize = c(1, 3, 5),
              trControl = trControl
	             )
stopCluster(cl)	
saveRDS(fit.rf, "./Models/fit.rf.rds")

fit.rf <- readRDS("./Models/fit.rf.rds")
data.frame(metric = "ctree", RMSE = min(summary(fit.rf$results$RMSE)))
fit.rf
plot(fit.rf)


## Summarize RMSE of models

data.frame(
  "ML Model" = c(
    "CTREE: Conditional Inference Trees",
    "GBM: Gradient Boosted Machine",
    "RF: Random Forest"
  ),
  "RMSE" = c(
    min(fit.ctree$results$RMSE),
    min(fit.gbm$results$RMSE),
    min(fit.rf$results$RMSE)
  )
)


# Predictions 

predictions <- predict(fit.gbm, test)

data.frame(
  "ML Model" = c("GBM: Gradient Boosted Machine",
                 "Prediction"),
  "RMSE" = c(
    min(fit.gbm$results$RMSE),
    RMSE(predictions, test$active.power) # RMSE of predictions
  ),
  "Accuracy" = 100 - (100 * min(fit.gbm$results$RMSE) / mean(test$active.power))
)


## Predction vs Acutal Graphs


Ctest <- cbind(Pred = predictions, test)

# 1st third chart
ggplot() +
  geom_line(data = test[1:581, ],
            aes(x = wind.speed, y = active.power, group = 1),
            color = "red") +
  geom_line(data = Ctest[1:581, ],
            aes(x = wind.speed, y = Pred, group = 1),
            color = "blue") +
  theme_light() +
  labs(title = "1st Sectional Chart: Predction vs Actual", subtitle = "0 - 500 mw : Predction RMSE 10.14, Accuracy %98.95")

# 2nd third chart
ggplot() +
  geom_line(data = test[581:1162, ],
            aes(x = wind.speed, y = active.power, group = 1),
            color = "red") +
  geom_line(data = Ctest[581:1162, ],
            aes(x = wind.speed, y = Pred, group = 1),
            color = "blue") +
  theme_light() +
  labs(title = "2nd Sectional Chart: Predction vs Actual", subtitle = "500 - 1600 mw : Predction RMSE 10.14, Accuracy %98.95")

# 3rd third chart
ggplot() +
  geom_line(data = test[1162:1744, ],
            aes(x = wind.speed, y = active.power, group = 1),
            color = "red") +
  geom_line(data = Ctest[1162:1744, ],
            aes(x = wind.speed, y = Pred, group = 1),
            color = "blue") +
  theme_light() +
  labs(title = "3rd Sectional Chart: Predction vs Actual", subtitle = "1600 - 3000 mw : Predction RMSE 10.14, Accuracy %98.95")

# overall chart
ggplot() +
  geom_line(
    data = test,
    aes(x = wind.speed, y = active.power, group = 1),
    color = "red",
    size = 1
  ) +
  geom_line(
    data = Ctest,
    aes(x = wind.speed, y = Pred, group = 1),
    color = "blue",
    size = 1
  ) +
  theme_light() +
  labs(title = "Predction vs Actual", subtitle = "0 - 3000 mw: Predction RMSE 10.14, Accuracy %98.95")










