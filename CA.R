library(tm)
library(SnowballC)
library(wordcloud)

Blueapron=read.csv("D3.2 Blueapron.csv")
View(Blueapron)
summary(Blueapron)
set.seed(177)

idx=sample(2,nrow(Blueapron),replace = TRUE,prob = c(.7,.3))
table(idx)

#Question 1
#model 1
lm1=lm(Blueapron$churn~ Blueapron$tenure + Blueapron$rating +Blueapron$partysize + Blueapron$urban, data=Blueapron[idx==1,])
summary(lm1)
Blueapron$prob_sub1=predict(lm1,newdata = Blueapron)
hist(Blueapron$prob_sub1[idx==1])
hist(Blueapron$prob_sub1[idx==2])
avg_not_sub1 = mean(Blueapron$prob_sub1[idx==1 & Blueapron$churn==0])
avg_sub1=mean(Blueapron$prob_sub1[idx==1 & Blueapron$churn==1])
thres1=0.5*(avg_sub1+avg_not_sub1)
Blueapron$pred_y1=ifelse(Blueapron$prob_sub1>=thres1,1,0)
table(Blueapron$churn[idx==2],Blueapron$pred_y1[idx==2],dnn=c("observed","predicted"))

#model 2
lm2=lm(Blueapron$churn~ Blueapron$tenure + Blueapron$rating +Blueapron$partysize + Blueapron$urban + Blueapron$rating*Blueapron$partysize + Blueapron$rating*Blueapron$urban +Blueapron$partysize*Blueapron$urban + Blueapron$urban*Blueapron$tenure, data=Blueapron[idx==1,])
summary(lm2)
Blueapron$prob_sub2=predict(lm2,newdata = Blueapron)
hist(Blueapron$prob_sub2[idx==1])
hist(Blueapron$prob_sub2[idx==2])
avg_not_sub2 = mean(Blueapron$prob_sub2[idx==1 & Blueapron$churn==0])
avg_sub2=mean(Blueapron$prob_sub2[idx==1 & Blueapron$churn==1])
thres2=0.5*(avg_sub2+avg_not_sub2)
Blueapron$pred_y2=ifelse(Blueapron$prob_sub2>=thres2,1,0)
table(Blueapron$churn[idx==2],Blueapron$pred_y2[idx==2],dnn=c("observed","predicted"))

#confusion matrices, side by side 
table(Blueapron$churn[idx==2],Blueapron$pred_y1[idx==2],dnn=c("observed","predicted"))
table(Blueapron$churn[idx==2],Blueapron$pred_y2[idx==2],dnn=c("observed","predicted"))

#choose model one 
lm3=lm(Blueapron$churn~ Blueapron$tenure + Blueapron$rating +Blueapron$partysize + Blueapron$urban, data=Blueapron)
Blueapron$prob_sub3=predict(lm3,newdata=Blueapron)
hist(Blueapron$prob_sub3)
Blueapron$prob_sub3=ifelse(Blueapron$prob_sub3<0,0,Blueapron$prob_sub3)
Blueapron$prob_sub3=ifelse(Blueapron$prob_sub3>1,1,Blueapron$prob_sub3)  
hist(Blueapron$prob_sub3)

#Question 2
#model 1
lm21=lm(Blueapron$monthlyaddons~ Blueapron$tenure + Blueapron$rating +Blueapron$partysize + Blueapron$urban, data=Blueapron[idx==1,])
summary(lm21)
Blueapron$prob_sub21=predict(lm21,newdata = Blueapron)
hist(Blueapron$prob_sub21[idx==1])
hist(Blueapron$prob_sub21[idx==2])
Blueapron$squarederror=(Blueapron$prob_sub21- Blueapron$monthlyaddons)^2
sum(Blueapron$squarederror[idx==2])


#model 2
lm22=lm(Blueapron$monthlyaddons~ Blueapron$tenure + Blueapron$rating +Blueapron$partysize + Blueapron$urban + Blueapron$rating*Blueapron$partysize + Blueapron$rating*Blueapron$urban +Blueapron$partysize*Blueapron$urban + Blueapron$urban*Blueapron$tenure, data=Blueapron[idx==1,])
summary(lm22)
Blueapron$prob_sub22=predict(lm22,newdata = Blueapron)
hist(Blueapron$prob_sub22[idx==1])
hist(Blueapron$prob_sub22[idx==2])
Blueapron$squarederror=(Blueapron$prob_sub22- Blueapron$monthlyaddons)^2
sum(Blueapron$squarederror[idx==2])


#select model 2 
lm23=lm(Blueapron$monthlyaddons~ Blueapron$tenure + Blueapron$rating +Blueapron$partysize + Blueapron$urban + Blueapron$rating*Blueapron$partysize + Blueapron$rating*Blueapron$urban +Blueapron$partysize*Blueapron$urban + Blueapron$urban*Blueapron$tenure, data=Blueapron)
Blueapron$prob_sub23=predict(lm23,newdata=Blueapron)
hist(Blueapron$prob_sub23)
Blueapron$prob_sub23=ifelse(Blueapron$prob_sub23<0,0,Blueapron$prob_sub23)
hist(Blueapron$prob_sub23)

#Question 3
dt1 <- Blueapron
dt1$Model2Probabilities <- Blueapron$prob_sub2
dt1$Model2AddOns <- Blueapron$addon_sub23
head(dt1)

# export as csv file
write.csv(dt1, file = "churn-reduction campaign evaluation_results3.csv",row.names=TRUE) 

