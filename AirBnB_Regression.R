#Sanfrancisco AirBnB price regression
#** Note: For visualization, Please refer to PowerBI file in here https://github.com/lequan26111991/AirBNB-Price-Data-Prediction/blob/master/San%20Fransico%20AIRBNB%20Visualization.pbix

#Get data set from website
tempdata <- tempfile()
download.file("http://data.insideairbnb.com/united-states/ca/san-francisco/2019-10-14/data/listings.csv.gz",tempdata)
df <- read.csv(tempdata,stringsAsFactors=FALSE)
unlink(tempdata)


#check column list
ls(df)

#Pick certain columns to make prediction and remove dollar sign for price
df.table <- data.frame(as.numeric(gsub("\\$", "", df$price)),df$beds,df$bedrooms,df$minimum_nights,df$review_scores_rating,df$availability_365,df$bathrooms,df$guests_included )
colnames(df.table) <- c("df.price","df.beds","df.bedrooms","df.minimum_nights","df.review_scores_rating","df.availability_365","df.bathrooms","df.guests_included")
df.table[1:5,]
nrow(df.table)

#Clean na column
df.table <- na.omit(df.table)
nrow(df.table)

#Perform PCA for Features Selection 
#install.packages("stats")
#library(stats)
#pca <- princomp(df.table, cor=TRUE)
#summary(pca) 

#Use caret package for test and train split
install.packages("caret")
library(caret)

#Split test and train
set.seed(10000)
y <- df$price
train <- createDataPartition(y, p = 0.7, list = FALSE)
df.train <- df.table[train,]
df.test <- df.table[-train,]

#Prepare test set for regression
y_actual <- df.test$df.price
df.test = subset(df.test, select = -c(df.price))

#Perform regression
lr.fit <- lm(df.price ~ ., data = df.train)
lr.fit
summary(lr.fit)
y_predict <- predict(lr.fit, df.test, interval="prediction")

#compare result and actual values of test set
result <- data.frame(y_predict[,"fit"],y_actual)
colnames(result) =c("Y Predict", "Y Actual")
result
