rm(list=ls())
require(data.table)

label_train <- fread("C:/Users/Akhil/Documents/talkingdata_kaggle/gender_age_train.csv",
                     colClasses=c("character","character",
                                  "integer","character"))
count(label_train$group)
sample(label_train)
nrow(label_train)
label_test <- fread("C:/Users/Akhil/Documents/talkingdata_kaggle/gender_age_test.csv",
                    colClasses=c("character"))

nrow(label_test)


label_test$gender <- label_test$age <- label_test$group <- NA

label <- rbind(label_train,label_test)

setkey(label,device_id)

rm(label_test,label_train);gc()

brand <- fread("C:/Users/Akhil/Documents/talkingdata_kaggle/phone_brand_device_model.csv",
               colClasses=c("character","character","character"))
setkey(brand,device_id)
brand0 <- unique(brand,by=NULL)
brand0 <- brand0[sample(nrow(brand0)),]
nrow(brand0)
brand2 <- brand0[-which(duplicated(brand0$device_id)),]

duplicated(label_train$group)

label1 <- merge(label,brand2,by="device_id",all.x=T)
rm(brand,brand0,brand2);gc()

events <- fread("C:/Users/Akhil/Documents/talkingdata_kaggle/events.csv",
                colClasses=c("character","character","character",
                             "numeric","numeric"))
setkey(events,device_id)
sample(unique(events$timestamp))
events0 <- events[events$timestamp>="2016-05-01 00:00:00" &
                    events$timestamp<="2016-05-07 23:59:59",]
timestamp <- strptime(events0$timestamp,format="%Y-%m-%d %H:%M:%S")
events0$date <- strftime(timestamp,format="%m%d")
events0$hour <- strftime(timestamp,format="%H")

events1 <- events0[,list(cnt=length(event_id)),by="device_id"]
events2 <- events0[,list(cnt_day=length(unique(date))),by="device_id"]

events3 <- events0[,list(cnt_date=length(event_id)),by="device_id,date"]
events33 <- reshape(events3,direction="wide",sep="_",
                    v.names="cnt_date",timevar="date",idvar="device_id")
events33[is.na(events33)] <- 0

events4 <- events0[,list(cnt_hour=length(event_id)),by="device_id,hour"]
events44 <- reshape(events4,direction="wide",sep="_",
                    v.names="cnt_hour",timevar="hour",idvar="device_id")
events44[is.na(events44)] <- 0

events5 <- merge(events3,events1,by="device_id",all.x=T)
events5$pct_date <- events5$cnt_date/events5$cnt
events55 <- reshape(events5[,list(device_id,date,pct_date)],direction="wide",sep="_",
                    v.names="pct_date",timevar="date",idvar="device_id")
events55[is.na(events55)] <- 0

events6 <- merge(events4,events1,by="device_id",all.x=T)
events6$pct_hour <- events6$cnt_hour/events6$cnt
events66 <- reshape(events6[,list(device_id,hour,pct_hour)],direction="wide",sep="_",
                    v.names="pct_hour",timevar="hour",idvar="device_id")
events66[is.na(events66)] <- 0

label2 <- merge(merge(merge(merge(merge(merge(label1,events1,by="device_id",all.x=T),
                                        events2,by="device_id",all.x=T),
                                  events33,by="device_id",all.x=T),
                            events44,by="device_id",all.x=T),
                      events55,by="device_id",all.x=T),
                events66,by="device_id",all.x=T)
rm(events1,events2,events3,events33,events4,events44,
   events5,events55,events6,events66,timestamp,events,events0);gc()

app_label1 <- fread("C:/Users/Akhil/Documents/talkingdata_kaggle/app_labels.csv",colClasses=rep("character",2))
app_label2 <- fread("C:/Users/Akhil/Documents/talkingdata_kaggle/label_categories.csv",
                    colClasses=rep("character",2))
app_label <- merge(app_label1,app_label2,by="label_id",all.x=T)
rm(app_label1,app_label2);gc()
app_label <- app_label[,list(labels=paste(label_id,collapse=",")),by="app_id"]
setkey(app_label,app_id)

event_app <- fread("C:/Users/Akhil/Documents/talkingdata_kaggle/app_events.csv",
                   colClasses=rep("character",4))
event_app$is_installed <- NULL
setkey(event_app,app_id)
event_app <- unique(event_app[,list(event_id,app_id)],by=NULL)

event_app_cat <- merge(event_app,app_label,by="app_id")
f_split_paste <- function(z){paste(unique(unlist(strsplit(z,","))),collapse=",")}
event_cat <- event_app_cat[,list(labels=f_split_paste(labels)),by="event_id"]
rm(event_app,event_app_cat,app_label);gc()
setkey(event_cat,event_id)

events <- fread("C:/Users/Akhil/Documents/talkingdata_kaggle/events.csv",
                colClasses=c("character","character","character",
                             "numeric","numeric"))
setkeyv(events,c("device_id","event_id"))

device_event_appcat <- merge(events[,list(device_id,event_id)],
                             event_cat,by="event_id")
rm(events,event_cat);gc()
device_appcat <- device_event_appcat[,list(labels=f_split_paste(labels)),by="device_id"]
rm(device_event_appcat);gc()

tmp <- strsplit(device_appcat$labels,",")
device_appcat_long <- data.table(device_id=rep(device_appcat$device_id,
                                               times=sapply(tmp,length)),
                                 label=unlist(tmp),isinstalled=1)
device_appcat_wide <- reshape(device_appcat_long,direction="wide",sep="_",
                              v.names="isinstalled",timevar="label",idvar="device_id")
device_appcat_wide[is.na(device_appcat_wide)] <- 0

rm(device_appcat_long,device_appcat,tmp);gc()

label3 <- merge(label2,device_appcat_wide,by="device_id",all.x=T)

label3 <- label3[sample(nrow(label3)),]

id <- label3$device_id
y <- label3$group
count(y)
y2 <- label3[,list(gender,age)]
x <- label3[,-c(1:4),with=F]
x$is_active_7d <- 1-as.integer(is.na(x$cnt))
ids_train <- id[!is.na(y)]
set.seed(114)
ids <- split(ids_train,sample(length(ids_train)) %% 5)

x <- as.data.frame(x)
for(i in which(sapply(x,class)=="character")) {x[,i] <- as.numeric(as.factor(x[,i]))}
rm(i)
y<-as.data.frame(y)
y2<-as.data.frame(y2)
id<-as.data.frame(id)
train_814<-cbind(id,y,y2,x)
train_814<-as.data.frame(train_814)
train_814v1<-as.data.frame(na.omit(train_814[which(train_814$id %in% unlist(ids)),]))



require(caret)



for ( i in 1:563 )

 { 
  ifelse (length(unique(train_814v1[,i]))>53,
          train_814v1[,i]<- as.numeric(train_814v1[,i]),
      train_814v1[,i]<- as.factor(train_814v1[,i]))
  
 }


control<-rfeControl(functions=rfFuncs,method="cv",number=10)
results<-rfe(train_814v1[,c(3:4,7:563)],train_814v1[,2],sizes=c(1:30),rfeControl = control)

require(caret)

x<-filterVarImp(train_814v1,factor(train_814v1$y))
write.csv(x,file="varimp.csv")
train_814v2<-train_814v1[,c("y","gender","age","isinstalled_1014","isinstalled_317","isinstalled_316")]
train_814v3<-na.omit(train_814v1[,c("y",
                            "isinstalled_783",
                            "isinstalled_757",
                            "isinstalled_779",
                            "isinstalled_959",
                            "isinstalled_960",
                            "isinstalled_1007",
                            "isinstalled_256",
                            "isinstalled_777",
                            "isinstalled_209",
                            "isinstalled_782",
                            "isinstalled_706",
                            "isinstalled_787",
                            "isinstalled_406",
                            "isinstalled_407",
                            "isinstalled_761",
                            "isinstalled_252",
                            "isinstalled_263",
                            "isinstalled_774",
                            "isinstalled_253",
                            "isinstalled_781",
                            "isinstalled_1014",
                            "isinstalled_751",
                            "isinstalled_1012",
                            "isinstalled_775",
                            "isinstalled_778",
                            "isinstalled_1015",
                            "isinstalled_254",
                            "isinstalled_562",
                            "isinstalled_691",
                            "isinstalled_758",
                            "isinstalled_752",
                           # "phone_brand",
                            "isinstalled_166",
                            "isinstalled_731",
                            "isinstalled_732",
                            "cnt_day",
                            "isinstalled_755",
                            "isinstalled_788",
                            "isinstalled_564",
                            #"device_model",
                            "isinstalled_168",
                            "cnt_hour_06",
                            "isinstalled_183",
                            "cnt",
                            "pct_hour_06",
                            "isinstalled_737",
                            "isinstalled_738",
                            "isinstalled_1011",
                            "cnt_hour_07",
                            "isinstalled_1005",
                            "isinstalled_1019",
                            "isinstalled_709",
                            "isinstalled_1020",
                            "cnt_date_0504",
                            "isinstalled_163",
                            "cnt_date_0503"
)])
n<-colnames(train_814v3)
train_814v3[is.na(train_814v3)]<-NA

form<-as.formula(paste("y~",paste(n[!n %in% c("y") ],collapse="+")))
train_814v3<-train_814v3[!is.nan(train_814v3),]
myTuneGrid <- expand.grid(n.trees = 1:5,interaction.depth = 2:5,shrinkage = 0.5,n.minobsinnode=2)

fitControl <- trainControl(method = "repeatedcv", number = 5,repeats = 2, verboseIter = FALSE,returnResamp = "all")

myModel <- train(form,data = train_814v3,method = "gbm",distribution="multinomial",trControl = fitControl,tuneGrid = myTuneGrid)

lapply(train_814v3,function(x) class(x))
train_814v3$pct_hour_06
is.factor(train_814v3$y)
#idx_test <- which(!id %in% unlist(ids))
#test_data <- x[idx_test,]
result<-predict(myModel,train_814v3,type="prob")
train_814v3<-cbind(train_814v3,result)

library(neuralnet)
n<-names(train_814v3)
n
require(dplyr)
train_814v3[]
colnames(train_814v3)[56]<-"F23"

colnames(train_814v3)
form<-as.formula(paste("y~",paste(n[!n %in% c("y") ],collapse="+")))
form
lapply(train_814v3,function(x) levels(x))

for (i in 3:67)
  
{ 
  if (is.factor(train_814v3[,i])==TRUE)
 
  { train_814v3[,i]<- mapvalues(train_814v3[,i], from = c("1", "0"), to = c("1", "-1"))  
 }
  
  
  
}


unique(train_814v3$isinstalled_706)
class(train_814v3[,2])

train_814v3[,2] <- mapvalues(train_814v3[,2], from = c("1", "0"), to = c("1", "-1"))


train_814v3[1,66]

colnames(train_814v3)
colnames(train_814v3)[56]<-"F_23"
colnames(train_814v3)[57]<-"F_24_26"
colnames(train_814v3)[58]<-"F_27_28"
colnames(train_814v3)[59]<-"F_29_32"
colnames(train_814v3)[60]<-"F_33_42"
colnames(train_814v3)[61]<-"F_43"
colnames(train_814v3)[62]<-"M_22"
colnames(train_814v3)[63]<-"M_23_26"
colnames(train_814v3)[64]<-"M_27_28"
colnames(train_814v3)[65]<-"M_29_31"
colnames(train_814v3)[66]<-"M_32_38"
colnames(train_814v3)[67]<-"M_39"



colnames(train_814v3[,1])
class(train_814v3$isinstalled_783)
unique(train_814v3$isinstalled_777)

train_814v4<-train_814v3
for(i in which(sapply(train_814v4,class)=="factor")) {train_814v4[,i] <- as.numeric(as.factor(train_814v4[,i]))}

require(neuralnet)
f<-neuralnet(form,data=train_814v4,hidden=c(10,10,10),linear.output = F)
neur<-prediction(f,train_814v4,type="raw")


require(xgboost)
depth <- 10
shrk <- 0.2
ntree <- 100
(group_name <- na.omit(unique(y)))

idx_train <- which(id %in% unlist(ids))
idx_test <- which(!id %in% unlist(ids))
train_data <- as.matrix(x[idx_train,])
test_data <- as.matrix(x[idx_test,])
train_label <- match(y[idx_train],group_name)-1
test_label <- match(y[idx_test],group_name)-1
dtrain <- xgb.DMatrix(train_data,label=train_label,missing=NA)
dtest <- xgb.DMatrix(test_data,label=test_label,missing=NA)
param <- list(booster="gbtree",
              num_class=length(group_name),
              objective="multi:softprob",
              eval_metric="mlogloss",
              eta=shrk,
              max.depth=depth,
              subsample=0.7,
              colsample_bytree=0.7,
              num_parallel_tree=1)
watchlist <- list(train=dtrain)
# set.seed(114)
# fit_cv <- xgb.cv(params=param,
#                  data=dtrain,
#                  nrounds=ntree*100000,
#                  watchlist=watchlist,
#                  nfold=5,
#                  early.stop.round=3,
#                  verbose=1)

# ntree should be 1100 to get 2.29934
ntree <- 50
set.seed(114)
fit_xgb <- xgb.train(params=param,
                     data=dtrain,
                     nrounds=ntree,
                     watchlist=watchlist,
                     verbose=1)
pred <- predict(fit_xgb,dtest,ntreelimit=ntree)
pred_detail <- t(matrix(pred,nrow=length(group_name)))
res_submit <- cbind(id=id[idx_test],as.data.frame(pred_detail))
colnames(res_submit) <- c("device_id",group_name)
write.csv(res_submit,file="submit_v0_2.csv",row.names=F,quote=F)


sapply(train_814v1,class)


