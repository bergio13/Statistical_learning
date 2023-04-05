library(quantmod)
library(MASS)
library(boot)

###############################################################################
## Functions
##############################################################################
#Function to shift the data
lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}

download_and_create <- function(date, type){
  getSymbols("^GSPC", from = date)
  df <- as.data.frame(GSPC)
  price_diff <- diff(df$GSPC.Adjusted)
  df <- df[-1,]
  
  # Create a new column indicating up or down
  df$CloseLag <- lagpad(df$GSPC.Close, 1)
  df$HighLag <- lagpad(df$GSPC.High, 1)
  df$LowLag <- lagpad(df$GSPC.Low, 1)
  df$OpenLag <- lagpad(df$GSPC.Open, 1)
  df$Direction <- ifelse(lag(price_diff) > 0, "Up", "Down")
  df$Direction <- as.factor(df$Direction)
  # Convert the date column to a date format
  df$Date <- as.Date(rownames(df))
  
  
  if (type==0) {
    df <- df[c(1, 7, 8, 9, 10, 12, 11)]
    rownames(df) <- NULL
    return(df)
    
  } else {
    df1 <- df[-1,]
    df1 <- df1[c(1, 7, 8, 9, 10, 12, 6)]
    rownames(df1) <- NULL
    return(df1)
  }
}

loocv = function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

getSymbols("^GSPC", from = "1990-01-01")
###############################################################################
# Download stock data for SP500
#getSymbols("^GSPC", from = "2000-01-01")
#head(GSPC)
#tail(GSPC)
#
#df <- as.data.frame(GSPC)
#df <- df[-1,]
#head(df)
#
## Create a new column indicating up or down
#df$CloseLag <- lagpad(df$GSPC.Close, 1)
#df$HighLag <- lagpad(df$GSPC.High, 1)
#df$LowLag <- lagpad(df$GSPC.Low, 1)
#df$OpenLag <- lagpad(df$GSPC.Open, 1)
#df$Direction <- ifelse(lag(price_diff) > 0, "Up", "Down")
#df$Direction <- as.factor(df$Direction)
## Convert the date column to a date format
#df$Date <- as.Date(rownames(df))
#
#df1 <- df
#df1 <- df1[-1,]
#
#df <- df[c(1, 7, 8, 9, 10, 12, 11)]
#df1 <- df1[c(1, 7, 8, 9, 10, 12, 6)]
#
#rownames(df) <- NULL
#rownames(df1) <- NULL
#
#head(df)
#head(df1)
##############################################################
## Time series
# Calculate daily price differences and declare time series
pdiff.ts <- ts(diff(GSPC$GSPC.Adjusted))

rets <- dailyReturn(Cl(GSPC))*100
rets.ts <- ts(rets)

plot(pdiff.ts)
reg <- lm(pdiff.ts ~ time(pdiff.ts))
abline(reg, col = "red")
summary(reg)

#-----------------------------------------
loga <- log(abs(pdiff.ts))
loga<- loga[!is.na(loga) & !is.infinite(loga)]

plot(loga, type = 'l')
reg <- lm(loga ~ time(loga) + I(time(loga)^4))
summary(reg)

lines(sort(time(loga)), fitted(reg)[order(time(loga))], col='red', type='l')
#------------------------------------------------

#########################################################################
## Preliminary analysis
#########################################################################
df <- download_and_create("1990-01-01", 0)
df1 <- download_and_create("1990-01-01", 1)

cor(df[, 1:5], use = "complete.obs")

# Create a time series object
sp500.ts <- ts(df1$GSPC.Adjusted)
# Plot the time series data
plot(sp500.ts, main = "S&P 500 Index", xlab = "Year", ylab = "Index Close Value")
abline(reg=lm(sp500.ts ~ time(sp500.ts)))
# Plot the sample autocorrelation function
acf(sp500.ts)
# Plot the partial autocorrelation function
pacf(sp500.ts)
# Perform the Ljung-Box test for autocorrelation
Box.test(sp500.ts, lag = 12, type = "Ljung-Box")

summary((df$CloseLag - df$OpenLag)/df$OpenLag * 100)
summary(rets.ts)

#Plot time series of returns and then perform a regression
plot(rets.ts, x=time(rets), type = 'l')
reg <- lm(rets.ts ~ time(rets.ts))
abline(reg, col = "red")
summary(reg)

E_no_Open = 0.53*0.057
E_no_Open * 100
E_Open = 0.72*0.051
E_Open * 100

#########################################################################
## Train-test split
########################################################################
train <- (index(df) < (0.8 * nrow(df)))
df.train <- df[train, ]
df.test <- df[!train,]

train <- (index(df1) < (0.8 * nrow(df1)))
df1.train <- df1[train, ]
df1.test <- df1[!train,]

tail(df)
tail(df1)

tail(GSPC, n=2)

new_data <- data.frame(GSPC.Open=4102.2, CloseLag=4109.31, HighLag=4110.75, 
                       LowLag=4056.18, OpenLag=4056.18, Date=as.Date("2023-04-03"))

###############################################################################
#----------------------Logistic regression
##############################################################################
glm.fit <- glm(Direction ~ . - OpenLag, data = df.train, family = binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit, df.test, type = "response")
glm.probs[1:10]

glm.pred <- rep("Down", nrow(df.test))
glm.pred[glm.probs > 0.5] = "Up"
contrasts(df$Direction)

table(glm.pred, df.test$Direction)
mean(glm.pred == df.test$Direction)

predict(glm.fit, newdata = new_data, type = "response")

########################################################
#--------LDA
#########################################################
lda.fit <- lda(Direction ~ . - OpenLag, data = df.train)
lda.fit

lda.pred <- predict(lda.fit, df.test)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, df.test$Direction)
mean(lda.class == df.test$Direction)

sum(lda.pred$posterior[, 1] >= 0.5)
sum(lda.pred$posterior[, 1] >= 0.5 & df.test$Direction == "Up")
sum(lda.pred$posterior[, 1] < 0.4 & df.test$Direction == "Down")

tail(lda.pred$posterior[, 1])
tail(lda.class)
tail(df.test$Direction)

table(tail(lda.class),
      tail(df.test$Direction))

predict(lda.fit, newdata = new_data, type = "response")

#####################################################
#-------------QDA
####################################################
qda.fit <- qda(Direction ~ . - OpenLag, data = df.train)
qda.fit

qda.class <- predict(qda.fit, df.test)$class

table(qda.class, df.test$Direction)
mean(qda.class == df.test$Direction)

predict(qda.fit, newdata = new_data, type = "response")


#col.ind<-ifelse(df.train$Direction=="Up", "orange","blue")
#plot(HighLag ~ CloseLag, data=df, subset=train, pch=16, col=col.ind)

#len1<-80; len2<-80; delta<-0.1
#grid.X1<-seq(from=min(df$CloseLag, na.rm = TRUE)-delta,to=max(df$CloseLag, na.rm = TRUE)+delta,length=len1)
#grid.X2<-seq(from=min(df$HighLag, na.rm = TRUE)-delta,to=max(df$HighLag, na.rm = TRUE)+delta,length=len2)
#dataT<-expand.grid(Lag1=grid.X1,Lag2=grid.X2)
#
#lda.pred<-predict(lda.fit, dataT)
#zp <- lda.pred$posterior[,2] -lda.pred$posterior[,1]
#contour(grid.X1, grid.X2, matrix(zp, nrow=len1),
#        levels=0, las=1, drawlabels=FALSE, lwd=1.5, add=T, col="violet")
#
#qda.pred<-predict(qda.fit,dataT)
#zp <- qda.pred$posterior[,2] -qda.pred$posterior[,1]
#contour(grid.X1, grid.X2, matrix(zp, nrow=len1),
#        levels=0, las=1, drawlabels=FALSE, lwd=1.5, add=T, col="brown")

######################################################################################
##-----------Linear regression
#######################################################################################
lm.fit <- lm(GSPC.Adjusted ~ + CloseLag + GSPC.Open + HighLag + Date, data = df1.train)
summary(lm.fit)

par(mfrow=c(2, 2))
plot(lm.fit)
par(mfrow=c(1, 1))

#Values with highest leverage
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

lm.pred <- predict(lm.fit, newdata = df1.test, interval = "prediction")
tail(lm.pred)

pred.error <- lm.pred[, 1] - df1.test$GSPC.Adjusted
tail(pred.error)
summary(pred.error)
plot(pred.error, x=df1.test$Date)
hist(pred.error, breaks = 30)
plot(density(pred.error))

tail(lm.pred[, 1] - df1.test$CloseLag)

predict(lm.fit, interval = "prediction", newdata = new_data)



# Model accuracy test
# Compute MSE by validation sampling
mean((df1$GSPC.Adjusted - predict(lm.fit, df1))[-train]^2)
# Compute MSE by leave one out cross validation
loocv(lm.fit)

cv.error <- rep(0, 10)
degree <- 1:10
for (d in degree) {
  lm.fit <- lm(GSPC.Adjusted ~ poly(CloseLag, d) + poly(GSPC.Open, d) + poly(HighLag, d) + poly(Date, 3), data = df1)
  cv.error[d] <- loocv(lm.fit)
}
cv.error
plot(degree, cv.error, type = "b")

############################################################
####### Auto-regression on returns
###########################################################
acf(rets.ts)
AR <- arima(rets.ts, order = c(1, 0, 0))
print(AR)
ts.plot(rets.ts)
AR_fit <- rets.ts - residuals(AR)
points(AR_fit, type = "l", col = 2, lty = 2)
#Use predict() to make a 1-step forecast
predict_AR <- predict(AR)

#Obtain the 1-step forecast using $pred[1]
predict_AR$pred[1]
#Alternatively use predict to make 1-step through 10-step forecasts
predict(AR, n.ahead = 10)

ts.plot(rets.ts, xlim = c(8300, 8450))
AR_forecast <- predict(AR, n.ahead = 5)$pred
AR_forecast_se <- predict(AR, n.ahead = 25)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)

AIC(AR)
BIC(AR)
