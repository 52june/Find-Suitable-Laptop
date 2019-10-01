############## Data Handling ################
library(plyr)
library(dplyr)
com<-as.data.frame(table(raw.group1$company))
other<-as.character(filter(com,Freq < 10)$Var1)
raw.group1$company<-mapvalues(raw.group1$company,from=other,to=rep("other",length(other)))

com<-as.data.frame(table(raw.group2$company))
other<-as.character(filter(com,Freq < 10)$Var1)
raw.group2$company<-mapvalues(raw.group2$company,from=other,to=rep("other",length(other)))

group1<-select(raw.group1,-c(name,price_avg,outerGPU_3D))
group2<-select(raw.group2,-c(name,price_avg,outerGPU_3D)) 
  # correlation between 3D & Compute : 0.988

############## Train/Test ################
set.seed(2017)
idx<-sample(1:nrow(group1),nrow(group1)*0.7)
train1<-group1[idx,]
test1<-group1[-idx,]

set.seed(1022)
idx<-sample(1:nrow(group2),nrow(group2)*0.7)
train2<-group2[idx,]
test2<-group2[-idx,]

############## Linear Regression ################
library(lmtest) # assumption check
library(car) # checking vif
#group1
null<-lm(logprice~1,data=train1)
full<-lm(logprice~battery+adapter+memory+company+release_month+
           CPU_score+core_num+monitor+anti_glare+wideview+touchscreen+
           brightness_auto+rotateLCD+memory_vol+faster_HDD+HDD_vol+
           SSD_vol+DVDrecorder+multiboost+graphic_inner+graphic_outer+graphic_memory+
           outerGPU_Compute+wired_lan+bluetooth+display_output_num+
           webcam+USB_type+Type_C+multireader+number_key+convex_key+antiwater_key+
           light_key+RGBlight_key+fingerscan+weight+world_warranty+dpi,
         data=train1) #delete OS (singularity)
step(null,scope=list(lower=null,upper=full),direction="both",trace=0)

lm.fit1<-lm(formula = logprice ~ memory_vol + company + CPU_score + weight + 
              release_month + core_num + touchscreen + monitor + outerGPU_Compute + 
              antiwater_key + dpi + display_output_num + memory + SSD_vol + 
              number_key + Type_C + fingerscan + wideview + DVDrecorder + 
              battery + HDD_vol + anti_glare + multireader + graphic_inner + 
              graphic_outer, data = train1) # all variables 
 
summary(lm.fit1)
shapiro.test(residuals(lm.fit1))
bptest(lm.fit1)
dwtest(lm.fit1)
vif(lm.fit1)
(vif(lm.fit1)[,3])^2 # compare between variables : no multicollinearity

yhat.lm<-predict(lm.fit1,newdata=test1)
mean((yhat.lm-test1[,"logprice"])^2) #MSE 0.03077082
sqrt(mean((yhat.lm-test1[,"logprice"])^2)) #RMSE 0.1754161
var(yhat.lm)/var(test1$logprice) # 0.8796989

#group2
null<-lm(logprice~1,data=train2)
full<-lm(logprice~battery+adapter+memory+company+release_month+CPU_company+
           CPU_score+monitor+anti_glare+wideview+touchscreen+graphic_memory+
           brightness_auto+rotateLCD+memory_vol+faster_HDD+HDD_vol+
           SSD_vol+DVDrecorder+multiboost+graphic_inner+graphic_outer+
           outerGPU_Compute+wired_lan+bluetooth+display_output_num+
           webcam+USB_type+Type_C+multireader+number_key+convex_key+antiwater_key+
           light_key+RGBlight_key+fingerscan+OS+weight+world_warranty+dpi,
         data=train2)
step(null,scope=list(lower=null,upper=full),direction="both",trace=0)

lm.fit2<-lm(formula = logprice ~ battery + memory_vol + company + OS + 
              CPU_score + fingerscan + SSD_vol + release_month + CPU_company + 
              outerGPU_Compute + antiwater_key + number_key + monitor + 
              display_output_num + RGBlight_key + dpi + memory + graphic_inner + 
              Type_C + DVDrecorder + faster_HDD + multiboost + wideview + 
              brightness_auto + HDD_vol + light_key + multireader, data = train2)

summary(lm.fit2)
shapiro.test(residuals(lm.fit2))
bptest(lm.fit2)
dwtest(lm.fit2)
(vif(lm.fit2)[,3])^2 # no multicollinearity

yhat.lm2<- predict(lm.fit2,newdata=test2)
mean((yhat.lm2-test2[,"logprice"])^2) #MSE 0.01964755
sqrt(mean((yhat.lm2-test2[,"logprice"])^2)) #RMSE 0.1401697 
var(yhat.lm2)/var(test2$logprice) # 0.909529

############## Regression Tree ################
library(tree)
#group1
tree1<-tree(logprice~.,data=train1)
plot(tree1);text(tree1)
cv.fit1<-cv.tree(tree1,K=10)
plot(cv.fit1$size,cv.fit1$dev,type='b')
m<-min(cv.fit1$dev)
points(cv.fit1$size[cv.fit1$dev==m],m,col="red",pch=16)

yhat.tree1<-predict(tree1,newdata=test1)
mean((yhat.tree1-test1[,"logprice"])^2) #MSE 0.05853857
sqrt(mean((yhat.tree1-test1[,"logprice"])^2)) #RMSE 0.2419475
var(yhat.tree1)/var(test1$logprice) # 0.706084

#group2
tree2<-tree(logprice~.,data=train2)
plot(tree2);text(tree2)

cv.fit2<-cv.tree(tree2,K=10)
plot(cv.fit2$size,cv.fit2$dev,type='b')
m<-min(cv.fit2$dev)
points(cv.fit2$size[cv.fit2$dev==m],m,col="red",pch=16)

yhat.tree2<-predict(tree2,newdata=test2)
mean((yhat.tree2-test2[,"logprice"])^2) #MSE 0.04356419
sqrt(mean((yhat.tree2-test2[,"logprice"])^2)) #RMSE 0.2087204
var(yhat.tree2)/var(test2$logprice) # 0.8152141

############## Random Forest ################
library(randomForest)
#group1
set.seed(10)
rf.fit1<-randomForest(train1[,-46],train1$logprice,ntree=500,mtry=15)
yhat.rf1<-predict(rf.fit1,newdata=test1)
mean((yhat.rf1-test1[,"logprice"])^2) #MSE 0.01620464
sqrt(mean((yhat.rf1-test1[,"logprice"])^2)) #RMSE 0.1272974
var(yhat.rf1)/var(test1$logprice) #0.8034623

#group2
set.seed(10)
rf.fit2<-randomForest(train2[,-46],train2$logprice,ntree=500,mtry=15)
yhat.rf2<-predict(rf.fit2,newdata=test2)
mean((yhat.rf2-test2[,"logprice"])^2) #MSE 0.01607958
sqrt(mean((yhat.rf2-test2[,"logprice"])^2)) #RMSE 0.1268053
var(yhat.rf2)/var(test2$logprice) #0.7555339

############## Boosting ################
detach("package:randomForest");detach("package:dplyr")
library(gbm)
library(caret)
library(dplyr)

#group1
newtrain1<-select(train1,-c(CPU_company,refresh,BlueRayrecorder,more_graphic,mechanical_key))
# factors with only one level
fitControl<-trainControl(method="cv", number=5, returnResamp = "all")
set.seed(1234) # tuning shrinkage & interaction.depth
bt.tuning<-train(logprice~., data=newtrain1, method="gbm",distribution="gaussian", 
              trControl=fitControl, verbose=F, 
              tuneGrid=data.frame(.n.trees=9000, .shrinkage=rep(c(0.03,0.04),2), 
                                 .interaction.depth=c(2,3,3,2), .n.minobsinnode=10))
bt.tuning

set.seed(1301) # tuning n.tree (5032)
boost.cv<-gbm(formula = logprice ~ .,distribution = "gaussian",data = newtrain1, 
              n.trees = 9000,cv.folds = 5,interaction.depth = 3,
              shrinkage=0.04,n.cores = 2)
best.tree<-gbm.perf(boost.cv);best.tree

boost1<- gbm(logprice~.,data=newtrain1,distribution="gaussian",n.trees=best.tree,
             shrinkage=0.04,interaction.depth=3)

yhat.bt<- predict(boost1,newdata=test1,n.trees=best.tree)
mean((yhat.bt-test1[,"logprice"])^2) # MSE 0.011375
sqrt(mean((yhat.bt-test1[,"logprice"])^2)) # RMSE 0.1066536
var(yhat.bt)/var(test1$logprice) # 0.9503649

par(mfrow=c(3,3))
plot(boost1,i=1);plot(boost1,i=2);plot(boost1,i=3)
plot(boost1,i=4);plot(boost1,i=5);plot(boost1,i=6)
plot(boost1,i=7);plot(boost1,i=8);plot(boost1,i=9)
plot(boost1,i=10);plot(boost1,i=11);plot(boost1,i=12)
plot(boost1,i=13);plot(boost1,i=14);plot(boost1,i=15)
plot(boost1,i=16);plot(boost1,i=17);plot(boost1,i=18)
plot(boost1,i=19);plot(boost1,i=20);plot(boost1,i=21)
plot(boost1,i=22);plot(boost1,i=23);plot(boost1,i=24)
plot(boost1,i=25);plot(boost1,i=26);plot(boost1,i=27)
plot(boost1,i=28);plot(boost1,i=29);plot(boost1,i=30)
plot(boost1,i=31);plot(boost1,i=32);plot(boost1,i=33)
plot(boost1,i=34);plot(boost1,i=35);plot(boost1,i=36)
plot(boost1,i=37);plot(boost1,i=38);plot(boost1,i=39)
plot(boost1,i=40);

par(mfrow=c(1,1))

#group2
newtrain2<-select(train2,-c(BlueRayrecorder,more_graphic,mechanical_key))

set.seed(1234)
bt.tuning<-train(logprice~., data=newtrain2, method="gbm",distribution="gaussian", 
                 trControl=fitControl, verbose=F, 
                 tuneGrid=data.frame(.n.trees=9000, .shrinkage=rep(c(0.03,0.04),2), 
                                     .interaction.depth=c(2,3,3,2), .n.minobsinnode=10))
bt.tuning

set.seed(1206)
boost.cv<-gbm(formula = logprice ~ .,distribution = "gaussian",data = newtrain2, 
              n.trees = 9000,cv.folds = 5,interaction.depth = 2,
              shrinkage=0.03,n.cores = 2)
best.tree<-gbm.perf(boost.cv);best.tree #6658

boost2<- gbm(logprice~.,data=newtrain2,distribution="gaussian",n.trees=best.tree,
             shrinkage=0.03,interaction.depth=2)

yhat.bt<- predict(boost2,newdata=test2,n.trees=best.tree)
mean((yhat.bt-test2[,"logprice"])^2) 
sqrt(mean((yhat.bt-test2[,"logprice"])^2)) 
var(yhat.bt)/var(test2$logprice) 

par(mfrow=c(3,3))
plot(boost2,i=1);plot(boost2,i=2);plot(boost2,i=3)
plot(boost2,i=4);plot(boost2,i=5);plot(boost2,i=6)
plot(boost2,i=7);plot(boost2,i=8);plot(boost2,i=9)
plot(boost2,i=10);plot(boost2,i=11);plot(boost2,i=12)
plot(boost2,i=13);plot(boost2,i=14);plot(boost2,i=15)
plot(boost2,i=16);plot(boost2,i=17);plot(boost2,i=18)
plot(boost2,i=19);plot(boost2,i=20);plot(boost2,i=21)
plot(boost2,i=22);plot(boost2,i=23);plot(boost2,i=24)
plot(boost2,i=25);plot(boost2,i=26);plot(boost2,i=27)
plot(boost2,i=28);plot(boost2,i=29);plot(boost2,i=30)
plot(boost2,i=31);plot(boost2,i=32);plot(boost2,i=33)
plot(boost2,i=34);plot(boost2,i=35);plot(boost2,i=36)
plot(boost2,i=37);plot(boost2,i=38);plot(boost2,i=39)
plot(boost2,i=40);

par(mfrow=c(1,1))

############## GAM ################
library(gam)
#group1
gam.fit1<-gam(logprice~s(battery)+s(adapter)+memory+company+s(release_month)+
                s(CPU_score)+core_num+s(monitor)+anti_glare+wideview+touchscreen+
                brightness_auto+rotateLCD+s(memory_vol)+faster_HDD+HDD_vol+
                s(SSD_vol)+DVDrecorder+multiboost+graphic_inner+graphic_outer+graphic_memory+
                s(outerGPU_Compute)+wired_lan+bluetooth+display_output_num+
                webcam+USB_type+Type_C+multireader+number_key+convex_key+antiwater_key+
                light_key+RGBlight_key+fingerscan+s(weight)+world_warranty+s(dpi),
              data=train1) # delete OS (similar reason with linear regression)
summary(gam.fit1)
par(mfrow=c(3,3))
plot(gam.fit1)

yhat.gam1<- predict(gam.fit1,newdata=test1)
mean((yhat.gam1-test1[,"logprice"])^2) #0.02462811
sqrt(mean((yhat.gam1-test1[,"logprice"])^2)) #0.1569335
var(yhat.gam1)/var(test1$logprice) #0.8610968

#corrected ver.
gam.fit11<-gam(logprice~s(battery)+adapter+memory+company+s(release_month)+
                s(CPU_score)+core_num+monitor+anti_glare+wideview+touchscreen+
                brightness_auto+rotateLCD+s(memory_vol)+faster_HDD+HDD_vol+
                s(SSD_vol)+DVDrecorder+multiboost+graphic_inner+graphic_outer+graphic_memory+
                s(outerGPU_Compute)+wired_lan+display_output_num+
                USB_type+Type_C+multireader+antiwater_key+
                light_key+fingerscan+s(weight)+s(dpi),
              data=train1)
summary(gam.fit11)
yhat.gam11<- predict(gam.fit11,newdata=test1)
mean((yhat.gam11-test1[,"logprice"])^2) #0.02508191
sqrt(mean((yhat.gam11-test1[,"logprice"])^2)) #0.1583727
var(yhat.gam11)/var(test1$logprice) #0.8724976

#group2
gam.fit2<-gam(logprice~s(battery)+s(adapter)+CPU_company+memory+company+s(release_month)+
                s(CPU_score)+core_num+s(monitor)+anti_glare+wideview+touchscreen+
                brightness_auto+rotateLCD+s(memory_vol)+faster_HDD+HDD_vol+refresh+
                s(SSD_vol)+DVDrecorder+multiboost+graphic_inner+graphic_outer+graphic_memory+
                s(outerGPU_Compute)+wired_lan+bluetooth+display_output_num+
                webcam+USB_type+Type_C+multireader+number_key+convex_key+antiwater_key+
                light_key+RGBlight_key+fingerscan+OS+s(weight)+world_warranty+s(dpi),
              data=train2)
summary(gam.fit2)
plot(gam.fit2)
par(mfrow=c(1,1))

yhat.gam2<- predict(gam.fit2,newdata=test2)
mean((yhat.gam2-test2[,"logprice"])^2) #0.01623778
sqrt(mean((yhat.gam2-test2[,"logprice"])^2)) #0.1274276
var(yhat.gam2)/var(test2$logprice) #0.9646207

#corrected ver.
gam.fit21<-gam(logprice~s(battery)+s(adapter)+CPU_company+memory+company+
                CPU_score+core_num+monitor+anti_glare+wideview+touchscreen+
                s(memory_vol)+faster_HDD+refresh+
                SSD_vol+DVDrecorder+multiboost+graphic_outer+graphic_memory+
                s(outerGPU_Compute)+wired_lan+display_output_num+
                Type_C+number_key+antiwater_key+
                RGBlight_key+fingerscan+OS+s(dpi),
              data=train2)
summary(gam.fit21)

yhat.gam21<- predict(gam.fit21,newdata=test2)
mean((yhat.gam21-test2[,"logprice"])^2) #0.01852431
sqrt(mean((yhat.gam21-test2[,"logprice"])^2)) #0.1361041
var(yhat.gam21)/var(test2$logprice) #0.9715313

############## Final Prediction Model : BOOSTING ################
# fitting boost1 and boost2 to group1 and group2 respectively
# group1
newgroup1<-select(group1,-c(CPU_company,refresh,BlueRayrecorder,more_graphic,mechanical_key))

set.seed(1234)
boost.cv<-gbm(formula = logprice ~ .,distribution = "gaussian",data = newgroup1, 
              n.trees = 9000,cv.folds = 5,interaction.depth = 3,
              shrinkage=0.04,n.cores = 2)
best.tree<-gbm.perf(boost.cv);best.tree  # 5425

boost1<-gbm(logprice~.,data=newgroup1,distribution="gaussian",n.trees=best.tree,
             shrinkage=0.04,interaction.depth=3)

yhat.final1<- predict(boost1,newdata=raw.group1,n.trees=best.tree)
mean((exp(yhat.final1)-raw.group1[,"price_avg"])^2) # 0.003929923
sqrt(mean((exp(yhat.final1)-raw.group1[,"price_avg"])^2)) # 0.0626891
var(yhat.final1)/var(raw.group1$logprice) # 0.9675946

raw.group1$gsb<-ifelse(yhat.final1>raw.group1$logprice,1,0)
good1<-filter(raw.group1,gsb==1);good1$cluster<-1
filter(good1,weight %in% unique(sort(weight))[1:5],company %in% c("LG전자","삼성전자"))$name
filter(good1,price_avg %in% unique(sort(price_avg))[1:5])$name

# group2
newgroup2<-select(group2,-c(BlueRayrecorder,more_graphic,mechanical_key))

set.seed(1234)
boost.cv<-gbm(formula = logprice ~ .,distribution = "gaussian",data = newgroup2, 
              n.trees = 9000,cv.folds = 5,interaction.depth = 2,
              shrinkage=0.03,n.cores = 2)
best.tree<-gbm.perf(boost.cv);best.tree # 8949

boost2<- gbm(logprice~.,data=newgroup2,distribution="gaussian",n.trees=best.tree,
             shrinkage=0.03,interaction.depth=2)

yhat.final2<- predict(boost2,newdata=raw.group2,n.trees=best.tree)
mean((yhat.final2-raw.group2[,"logprice"])^2) # MSE 0.002918408
sqrt(mean((yhat.final2-raw.group2[,"logprice"])^2)) # RMSE 0.05402229
var(yhat.final2)/var(raw.group2$logprice) #0.9591688

raw.group2$gsb<-ifelse(yhat.final2>raw.group2$logprice,1,0)
good2<-filter(raw.group2,gsb==1);good2$cluster<-2
filter(good2,CPU_score %in% unique(sort(CPU_score))[1:5])$name
filter(good2,outerGPU_Compute %in% unique(sort(outerGPU_Compute))[1:5])$name

############## Bootstrap ################
good<-read.csv("good.csv",header=T)
good$cluster<-factor(good$cluster,labels = c("entry-end","high-end"))

spec<-select(good,-c(price_avg,logprice,name,outerGPU_3D,gsb))
str(spec) # only one value : BlueRayrecorder, more_graphic, mechanical_key
spec<-select(spec,-c(BlueRayrecorder, more_graphic, mechanical_key))
good<-select(good,-c(BlueRayrecorder, more_graphic, mechanical_key, outerGPU_3D, gsb,logprice))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))] }

mixed<-function(x){
  if (class(x)[1] %in% c("numeric","integer")) return(mean(x))
  else return(getmode(x)) }

basic<-as.data.frame(lapply(spec,mixed)) 
 
high_end<-c("battery","adapter","memory","release_month","CPU_score","core_num", 
        "monitor","anti_glare","memory_vol","SSD_vol","graphic_inner","graphic_outer",
        "graphic_memory","outerGPU_Compute","display_output_num","number_key","light_key",
        "weight","dpi")

#lower better : weight, dpi, SSD_vol, graphic_inner
#if not select graphic_outer : graphic_inner = 1 

high<-filter(spec,cluster=="high-end");low<-filter(spec,cluster=="entry-end")
spec_high<-as.data.frame(lapply(high[,colnames(high)%in%high_end],mixed))
spec_low<-as.data.frame(lapply(low[,colnames(low)%in%high_end],mixed))

additional<-c("touchscreen","brightness_auto","rotateLCD","refresh","faster_HDD",
              "HDD_vol","DVDrecorder","multiboost","USB_type","antiwater_key",
              "RGBlight_key","fingerscan")

both<-c("wideview", "wired_lan", "bluetooth", "webcam", "Type_C","world_warranty", 
        "multireader","convex_key","CPU_company")

selection<-list(high_end=high_end, add=additional, company=levels(good$company),OS=levels(good$OS)[-2])

library(randomForest);library(cluster)
pricerange<-function(myown){
  want<-readline("Type high-end function: ")
  add<-readline("Type additional function: ")
  company<-readline("Which company? ")
  os<-readline("Do you need OS installed?(exlcude Mac) ")
  
  want<-unlist(strsplit(want," "))
  hl<-ifelse(want %in% c("dpi","SSD_vol","graphic_inner"),1,2)
  want_spec<-bind_cols(select(spec_high,want[which(hl==2)]),select(spec_low,want[which(hl==1)]))
  yn<-ifelse(selection$high_end %in% want,1,2)
  want_spec<-bind_cols(want_spec,select(spec_low,selection$high_end[which(yn==2)]))
  if ("outerGPU_Compute" %in% want) want_spec$graphic_outer<-1 
  if ("weight" %in% want) want_spec$weight<-1.2 else want_spec$weight<-1.8
  
  add<-unlist(strsplit(add," "))
  add_spec<-as.data.frame(matrix(rep(1,length(add)),nrow=1))
  colnames(add_spec)<-add
  yn<-ifelse(selection$add %in% add,1,2)
  add_spec<-bind_cols(add_spec,select(basic,selection$add[which(yn==2)]))
  if("USB_type" %in% add) add_spec$USB_type<-"USB 3.1"
  
  both_spec<-basic[,colnames(basic) %in% both]
  
  myown<-bind_cols(want_spec,add_spec,both_spec)
  
  os<-unlist(strsplit(os," "))
  myown$OS<-os
  
  company<-unlist(strsplit(company," "))
  myown$company<-company
  if (company == "APPLE") myown$OS<-"Mac OS"
  if (!(company %in% unique(good$company))) myown$company<-"other"
  
  myown$memory<-factor(as.character(myown$memory),levels=c("0","1"))
  myown$anti_glare<-factor(as.character(myown$anti_glare),levels=c("0","1"))
  myown$wideview<-factor(as.character(myown$wideview),levels=c("0","1"))
  myown$touchscreen<-factor(as.character(myown$touchscreen),levels=c("0","1"))
  myown$brightness_auto<-factor(as.character(myown$brightness_auto),levels=c("0","1"))
  myown$rotateLCD<-factor(as.character(myown$rotateLCD),levels=c("0","1"))
  myown$refresh<-factor(as.character(myown$refresh),levels=c("0","1"))
  myown$faster_HDD<-factor(as.character(myown$faster_HDD),levels=c("0","1"))
  myown$DVDrecorder<-factor(as.character(myown$DVDrecorder),levels=c("0","1"))
  myown$multiboost<-factor(as.character(myown$multiboost),levels=c("0","1"))
  myown$graphic_inner<-factor(as.character(myown$graphic_inner),levels=c("0","1"))
  myown$graphic_outer<-factor(as.character(myown$graphic_outer),levels=c("0","1"))
  myown$wired_lan<-factor(as.character(myown$wired_lan),levels=c("0","1"))
  myown$bluetooth<-factor(as.character(myown$bluetooth),levels=c("0","1"))
  myown$webcam<-factor(as.character(myown$webcam),levels=c("0","1"))
  myown$USB_type<-factor(myown$USB_type,levels=c("USB 2.0","USB 3.0","USB 3.1"),ordered=T)
  myown$Type_C<-factor(as.character(myown$Type_C),levels=c("0","1"))
  myown$multireader<-factor(as.character(myown$multireader),levels=c("0","1"))
  myown$number_key<-factor(as.character(myown$number_key),levels=c("0","1"))
  myown$convex_key<-factor(as.character(myown$convex_key),levels=c("0","1"))
  myown$antiwater_key<-factor(as.character(myown$antiwater_key),levels=c("0","1"))
  myown$light_key<-factor(as.character(myown$light_key),levels=c("0","1"))
  myown$RGBlight_key<-factor(as.character(myown$RGBlight_key),levels=c("0","1"))
  myown$fingerscan<-factor(as.character(myown$fingerscan),levels=c("0","1"))
  myown$world_warranty<-factor(as.character(myown$world_warranty),levels=c("0","1"))
  myown$core_num<-factor(as.character(myown$core_num),levels=c("2","4","8"),ordered = T)
  myown$HDD_vol<-factor(as.character(myown$HDD_vol),levels=c("0","512","1024","2048"),ordered = T)
  myown$display_output_num<-factor(as.character(myown$memory),ordered = T)
  myown$company<-levels(spec$company)[which(myown$company==levels(spec$company))]
  myown$company<-factor(myown$company,levels = levels(spec$company))
  myown$CPU_company<-levels(spec$CPU_company)[which(myown$CPU_company==levels(spec$CPU_company))]
  myown$CPU_company<-factor(myown$CPU_company, levels = levels(spec$CPU_company))
  myown$OS<-levels(spec$OS)[which(myown$OS==levels(spec$OS))]
  myown$OS<-factor(myown$OS,levels=levels(spec$OS))
  
  print(myown)
  
  n<-nrow(good) # bootstraping
  price<-NULL
  m<-100
  for (i in 1:m){
    id<-sample(1:n,n,replace=T)
    dt<-good[id,]
    rf<-randomForest(good[,-c(1,5,45)],good$price_avg,ntree=200,mtry=14)
    price[i]<-predict(rf,newdata=myown) }
  
  a<-seq(0.001,0.049,0.001) # calculating CI
  CI<-NULL
  for (k in a){
    conf<-quantile(price, prob=c(k,(0.95+k)))
    CI<-rbind(CI,conf) }
  leng<-CI[,2] - CI[,1]
  range<-CI[which.min(leng),];names(range)<-c("","")
  
  dist<-NULL
  for (j in 1:n){
    gower_dist<-daisy(rbind(good[j,-c(1,5,45)],myown),metric="gower",stand=T)
    dist[j]<-gower_dist[1]
  }
  close<-filter(good,dist %in% unique(sort(dist))[1:3])$name
  group<-getmode(filter(good,dist %in% unique(sort(dist))[1:3])$cluster)
  return(list(range=range,similar_products=close,group=group))
}

pricerange(myown)
 