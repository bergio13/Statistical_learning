library(quantmod)
library(MASS)

#Function to shift the data
lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}

# Download stock data for SP500
getSymbols("^GSPC", from = "2000-01-01")
head(GSPC)
tail(GSPC)

df <- as.data.frame(GSPC)
head(df)

# Calculate daily price differences
price_diff <- diff(df$GSPC.Adjusted)
df <- df[-1,]

price_diff
# Create a new column indicating up or down
df$CloseLag <- lagpad(df$GSPC.Close, 1)
df$HighLag <- lagpad(df$GSPC.High, 1)
df$LowLag <- lagpad(df$GSPC.Low, 1)
df$OpenLag <- lagpad(df$GSPC.Open, 1)
df$Direction <- ifelse(lag(price_diff) > 0, "Up", "Down")
df$Direction <- as.factor(df$Direction)
# Convert the date column to a date format
df$Date <- as.Date(rownames(df))

df1 <- df
df1 <- df1[-1,]

df <- df[c(1, 7, 8, 9, 10, 12, 11)]
df1 <- df1[c(1, 7, 8, 9, 10, 12, 6)]

rownames(df) <- NULL
rownames(df1) <- NULL

head(df)
head(df1)
#######################################################################
#Preliminary analysis
#########################################################################
cor(df[, 1:5], use = "complete.obs")


# Create a time series object
sp500.ts <- ts(df$GSPC.Open)
# Plot the time series data
plot(sp500.ts, main = "S&P 500 Index", xlab = "Year", ylab = "Index Close Value")
# Plot the sample autocorrelation function
acf(sp500.ts)
# Plot the partial autocorrelation function
pacf(sp500.ts)
# Perform the Ljung-Box test for autocorrelation
Box.test(sp500.ts, lag = 12, type = "Ljung-Box")

summary((df$CloseLag - df$OpenLag)/df$CloseLag * 100)
plot((df$CloseLag - df$OpenLag)/df$CloseLag * 100)
gs <- GSPC$GSPC.Close[-1, ]
d <- ((gs - df$CloseLag)/ gs * 100)
summary(d)
plot(d)

E_no_Open = 0.53*0.057
E_no_Open * 100
E_Open = 0.72*0.051
E_Open * 100

######-------------------------------------------------------#######
#Train-test split
train <- (index(df) < (0.8 * nrow(df)))
df.train <- df[train, ]
df.test <- df[!train,]

train <- (index(df1) < (0.8 * nrow(df1)))
df1.train <- df1[train, ]
df1.test <- df1[!train,]

tail(df)
tail(df1)



tail(GSPC, n=2)
new_data <- data.frame(GSPC.Open=4046.74, CloseLag=4027.81, HighLag=4030.59, 
                       LowLag=3999.53, OpenLag=3999.53, Date=as.Date("2023-03-30"))

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

#new_data <- data.frame(GSPC.Open=tail(GSPC$GSPC.Open, n=1)[1,1], CloseLag=tail(df$CloseLag, n=1), HighLag=tail(df$HighLag, n=1), 
#                      LowLag=tail(df$LowLag, n=1), OpenLag=tail(df$OpenLag, n=1))


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
df1.train <- df1.train[-4551, ]
lm.fit <- lm(GSPC.Adjusted ~ . - LowLag - OpenLag, data = df1.train)
summary(lm.fit)

confint(lm.fit)

plot(lm.fit)

hatvalues(lm.fit) 
plot(hatvalues(lm.fit))
max(hatvalues(lm.fit)) 
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
up_down <- ifelse((lm.pred[, 1] - df1.test$CloseLag) > 0, "Up", "Down")
table(up_down, df.test$Direction[-3])

predict(lm.fit, interval = "prediction", newdata = new_data)


