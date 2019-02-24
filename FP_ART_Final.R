# Import data

customer_data = read.csv("LocalArt.csv")
coupon_data = read.csv("coupon.csv")
customer_data = subset(customer_data,select= -c(X))
coupon_data = subset(coupon_data,select= -c(X))

# Data Pre-processing

## Checking missing values
sum(is.na(coupon_data))

## Converting JoinDate and DOB to numeric values
joinDay_list = numeric(0)
age_list = numeric(0)
for (i in 1:length(customer_data[,1])){
  joinDay = as.numeric(as.Date('2017-01-01') - as.Date(as.character(customer_data$JoinDate[i])))
  age = (as.numeric(as.Date('2017-01-01') - as.Date(as.character(customer_data$DOB[i]))))/365
  joinDay_list = c(joinDay_list,joinDay)
  age_list = c(age_list,age)
}

customer_data$age = age_list
customer_data$joinDay = joinDay_list


## Derive Revenue, Conversion
revenue_list = numeric(0)
conversion_list = numeric(0)
for (i in 1:length(customer_data[,1])){
  revenue = customer_data[i,"Paintings"]*0.08 + customer_data[i,"Jewelry"]*0.08+customer_data[i,"Mosaics"]*0.12 + customer_data[i,"Sculpture"]*0.12
  revenue_list = c(revenue_list,revenue)
  if (customer_data[i,"Visits"] == 0){
    conversion = 0
  }
  else {
    conversion = revenue/customer_data[i,"Visits"]
  }
  conversion_list = c(conversion_list,conversion)
 
}

customer_data$revenue = revenue_list
customer_data$conversion = conversion_list

## Merge coupon into customer data
customer_data$Coupon = numeric(5000)
customer_data$Coupon = ifelse(customer_data$Coupon == 1, "Yes", "No")
count = 0
for (i in 1:length(coupon_data[,1])){
  for (j in 1:length(customer_data[,1])){
    if (customer_data[j,"CustID"]==coupon_data[i,]){
      customer_data$Coupon[j] = "Yes"
      break
    }
  }
}

outcome = sum(customer_data$Coupon == "Yes")

# Summary Stastics, Business Insight (Selina)
library(pastecs)
numeric.n=NULL
for (i in 1:ncol(customer_data))
{
  if(is.numeric(customer_data[,i])==T){
    numeric.n=c(numeric.n,i)
  }
}
stat.desc(customer_data[numeric.n])
stat.desc(customer_data)
table(customer_data$Gender)
table(customer_data$Coupon)

## Answering core question 1: No, don't give 10% off coupon
data_withID = subset(customer_data,select= -c(JoinDate,DOB))
data_q1 = data.frame(data_withID)
yes_coupon = data_q1[which(data_q1$Coupon == "Yes"),]
no_coupon = data_q1[which(data_q1$Coupon == "No"),]

summary(yes_coupon)
summary(no_coupon)

## Visualization
# Barchart: average revenue of each age group
customer_data$age=floor(customer_data$age)
summary(customer_data$age)
customer_data$Age.group="Teenager"
for (i in 1:dim(customer_data)[1])
{
  if(customer_data[i,]$age<=18)
  {customer_data[i,]$Age.group="Teenager"}
  else if(customer_data[i,]$age<=30)
  {customer_data[i,]$Age.group="Young"}
  else if(customer_data[i,]$age<=45)
  {customer_data[i,]$Age.group="Middle"}
  else {customer_data[i,]$Age.group="Senior"}
}
library(ggplot2)
Teenager=mean(customer_data[customer_data$Age.group=="Teenager",]$revenue)
Young=mean(customer_data[customer_data$Age.group=="Young",]$revenue)
Middle=mean(customer_data[customer_data$Age.group=="Middle",]$revenue)
Senior=mean(customer_data[customer_data$Age.group=="Senior",]$revenue)
Age=data.frame(Age.group=c("Teenager","Young","Middle","Senior"),Revenue=c(Teenager,Young,Middle,Senior))
ggplot(Age,aes(x=Age.group,y=Revenue,fill=factor(Age.group)))+geom_bar(stat="identity")+
  scale_fill_brewer(palette="Greens")+ggtitle("Age Group VS. Revenue")

# Barchart: average revenue of different visits ranges
library(Hmisc)
min(customer_data$Visits)
cut2(x=customer_data$Visits,g=8,onlycuts=T)
customer_data$Visits.range=ifelse(customer_data$Visits<23,"[0,23)",
                                  ifelse(customer_data$Visits<36,"[23,36)",
                                         ifelse(customer_data$Visits<47,"[36,47)",
                                                ifelse(customer_data$Visits<58,"[47,58)",
                                                       ifelse(customer_data$Visits<64,"[58,64)",
                                                              ifelse(customer_data$Visits<76,"[64,76)",
                                                                     ifelse(customer_data$Visits<88,"[76,88)","[88,133]")))))))
V=aggregate(revenue~Visits.range,data=customer_data,mean)
ggplot(V,aes(x=Visits.range,y=revenue,fill=factor(Visits.range)))+geom_bar(stat = "identity")+
  ggtitle("Visits VS. Revenue")+scale_fill_brewer()

# Pie Chart: Percentage of revenues generated from each art type
slices=c(sum(customer_data$Paintings),sum(customer_data$Jewelry),
         sum.Mosaics=sum(customer_data$Mosaics),sum.Sculpture=sum(customer_data$Sculpture))
pct=round(slices/sum(slices)*100,2)
lbls=c("Paintings","Jewelry","Mosaics","Sculpture")
lbls2=paste(lbls," ",pct,"%",sep="")
library(plotrix)
pie3D(slices,labels=lbls2,explode=0.1,labelcex = 0.95,col=topo.colors(4),
      main="Percentage of revenues generated from each art type")

# Stack bar chart of revenue: Each art type vs. Gender
Paintings=c(sum(customer_data[customer_data$Gender=="Male",]$Paintings),
            sum(customer_data[customer_data$Gender=="Female",]$Paintings))
Jewelry=c(sum(customer_data[customer_data$Gender=="Male",]$Jewelry),
          sum(customer_data[customer_data$Gender=="Female",]$Jewelry))
Sculpture=c(sum(customer_data[customer_data$Gender=="Male",]$Sculpture),
            sum(customer_data[customer_data$Gender=="Female",]$Sculpture))
Mosaics=c(sum(customer_data[customer_data$Gender=="Male",]$Mosaics),
          sum(customer_data[customer_data$Gender=="Female",]$Mosaics))
Table=data.frame(Paintings,Jewelry,Mosaics,Sculpture)
rownames(Table)=c("Male","Famale")
barplot(as.matrix(Table),legend.text = c("Male","Famale"),args.legend = c(x=5.5,y=1200000),
        main="Stack bar chart: Each art type vs. Gender",col=c("lightyellow","burlywood1"))

# Box Plot of revenue: With Coupon or Not 
boxplot(revenue~Coupon,data=customer_data,col=c("thistle2","thistle3"),
        main="Box Plot of Revenue: With Coupon or Not",notch=T)

# Mosaic Plot: relationship between Age Group and Gender and Coupon
library(vcd)
mosaic(~Age.group+Gender+Coupon,data=customer_data,shade=T,legend=T)

# Heatmap
ggplot(data=customer_data,mapping=aes(x=Visits.range,y=Age.group,fill=revenue))+geom_tile()+
  ggtitle("Heat Map: Visits & Age Group VS. Revenue")

# 3D Scatter Plot: Visits, Join Days and Convesion Rate
library(scatterplot3d)
scatterplot3d(customer_data$Visits,customer_data$joinDay,customer_data$conversion,
              highlight.3d = T,
              type="h",pch=16,xlab="Vists",ylab="Join Days",zlab="Conversion Rate",
              main="3D Scatter Plot: Visits, Join Days and Convesion Rate")


# Model Building
## Classification - preparation
summary(customer_data$revenue)
customer_data$High.revenue="Yes"
for (i in 1:length(customer_data$revenue)){
  if (customer_data[i,]$revenue>98){
    customer_data[i,]$High.revenue="Yes"}
  else{
    customer_data[i,]$High.revenue="No"}
}
art=customer_data[,c(4,5,6,7,8,9,10,11,14,17)]
art$Coupon=as.factor(art$Coupon)
art$High.revenue=as.factor(art$High.revenue)
set.seed(100)
testing.id=sample(1:5000,1000)
art_testing=art[testing.id,]
art_training=art[-testing.id,]

## Logistic Regression
LR=glm(High.revenue~.,data=art_training,family="binomial",maxit=100)    
summary(LR) 
LR.prob.test=predict(LR,art_testing,type="response")
cutoff.points=seq(0.05,0.95,by=0.01)
LR.errors=NULL
for(i in 1:length(cutoff.points)){
  LR.prediction.test=ifelse(LR.prob.test>=cutoff.points[i],"Yes","No")
  LR.mis.test=mean(LR.prediction.test!=art_testing$High.revenue)
  LR.errors=c(LR.errors,LR.mis.test)
}
LR.prediction.test=ifelse(LR.prob.test>=cutoff.points[which(LR.errors==min(LR.errors))][1],"Yes","No")
LR.test.CM=table(art_testing$High.revenue,LR.prediction.test)
LR.test.mis=(LR.test.CM[1,2]+LR.test.CM[2,1])/sum(LR.test.CM)
LR.test.mis

# Decision Tree
library(tree)
Tree=tree(High.revenue~.,data=art_training)
tree.predictions.train=predict(Tree,art_training,type="class")
tree.predictions.test=predict(Tree,art_testing,type="class")
Tree.test.CM=table(art_testing$High.revenue,tree.predictions.test)
Tree.test.mis=(Tree.test.CM[1,2]+Tree.test.CM[2,1])/sum(Tree.test.CM)
Tree.test.mis
plot(Tree)
text(Tree,pretty=0,cex=0.8)

# LDA
library(MASS)
LDA=lda(High.revenue~.,art_training)
LDA.prob.test=predict(LDA,art_testing)
cutoff.points=seq(0.05,0.95,by=0.01)
LDA.errors=NULL
for(i in 1:length(cutoff.points)){
  LDA.prediction.test=ifelse(LDA.prob.test$posterior[,2]>=cutoff.points[i],"Yes","No")
  LDA.mis.test=mean(LDA.prediction.test!=art_testing$High.revenue)
  LDA.errors=c(LDA.errors,LDA.mis.test)
}
LDA.prediction.test=ifelse(LDA.prob.test$posterior[,2]>=cutoff.points[which(LDA.errors==min(LDA.errors))],"Yes","No")
LDA.test.CM=table(art_testing$High.revenue,LDA.prediction.test)
LDA.test.CM
LDA.test.mis=(LDA.test.CM[1,2]+LDA.test.CM[2,1])/sum(LDA.test.CM)
LDA.test.mis

# QDA
QDA=qda(High.revenue~.,art_training)
QDA.prob.test=predict(QDA,art_testing)
cutoff.points=seq(0.05,0.95,by=0.01)
QDA.errors=NULL
for(i in 1:length(cutoff.points)){
  QDA.prediction.test=ifelse(QDA.prob.test$posterior[,2]>=cutoff.points[i],"Yes","No")
  QDA.mis.test=mean(QDA.prediction.test!=art_testing$High.revenue)
  QDA.errors=c(QDA.errors,QDA.mis.test)
}
QDA.prediction.test=ifelse(QDA.prob.test$posterior[,2]>=cutoff.points[which(QDA.errors==min(QDA.errors))],"Yes","No")
QDA.test.CM=table(art_testing$High.revenue,QDA.prediction.test)
QDA.test.CM
QDA.test.mis=(QDA.test.CM[1,2]+QDA.test.CM[2,1])/sum(QDA.test.CM)
QDA.test.mis


## Clusters (Wayne)
# Data Cleaning
names(customer_data)
data = subset(customer_data,select= -c(CustID,JoinDate,DOB))
data_withID = subset(customer_data,select= -c(JoinDate,DOB))

## M1 - k=2, not normalized
set.seed(1)
data_temp = data.frame(data_withID)
data_m1 = data.frame(data_withID)
clusters_m1 <- kmeans(data_temp[3:11], 2)
data_m1$cluster = clusters_m1$cluster


clusters_m1$centers
length(data_m1$cluster[data_m1$cluster==1 & data_m1$Coupon =="Yes"])/length(data_m1$cluster[data_m1$cluster==1])
length(data_m1$cluster[data_m1$cluster==2 & data_m1$Coupon =="Yes"])/length(data_m1$cluster[data_m1$cluster==2])




## M2: K=2, normalized
set.seed(1)
data_m2 = data.frame(data_withID)
data_temp = data.frame(data_withID)
for (i in 3:11){
  data_temp[,i] = (data_temp[,i] - min(data_temp[,i])) / (max(data_temp[,i]) - min(data_temp[,i]))
}

clusters_m2 <- kmeans(data_temp[,3:11], 2)
str(clusters_m2)
data_m2$cluster = clusters_m2$cluster

centroid_denorm = clusters_m2$center

for (j in 3:11){
  for (i in 1:length(centroid_denorm[,1])){
    centroid_denorm[i,j-2] = centroid_denorm[i,j-2]*(max(data_m2[,j]) - min(data_m2[,j]))+min(data_m2[,j])
  }
}

centroid_denorm
library(factoextra)
fviz_cluster(clusters_m2, data = data_temp[3:11])

## M3 - k = 3, normalized
set.seed(1)
data_temp<-data.frame(data_withID)
data_m3 = data.frame(data_withID)


for (i in 3:11){
  data_temp[,i] = (data_temp[,i] - min(data_temp[,i])) / (max(data_temp[,i]) - min(data_temp[,i]))
}

clusters_m3 <- kmeans(data_temp[,3:11], 3)
str(clusters_m3)
data_m3$cluster = clusters_m3$cluster

clusters_m3$centers

centroid_denorm = clusters_m3$center

for (j in 3:11){
  for (i in 1:length(centroid_denorm[,1])){
    centroid_denorm[i,j-2] = centroid_denorm[i,j-2]*(max(data_m2[,j]) - min(data_m2[,j]))+min(data_m2[,j])
  }
}

centroid_denorm

length(data_m3$cluster[data_m3$cluster==1 & data_m3$Coupon =="Yes"])
length(data_m3$cluster[data_m3$cluster==2 & data_m3$Coupon =="Yes"])
length(data_m3$cluster[data_m3$cluster==3 & data_m3$Coupon =="Yes"])
length(data_m3$cluster[data_m3$cluster==1 & data_m3$Coupon =="Yes"])/length(data_m3$cluster[data_m3$cluster==1])
length(data_m3$cluster[data_m3$cluster==2 & data_m3$Coupon =="Yes"])/length(data_m3$cluster[data_m3$cluster==2])
length(data_m3$cluster[data_m3$cluster==3 & data_m3$Coupon =="Yes"])/length(data_m3$cluster[data_m3$cluster==3])

## M4 - Silhhoute finding best K
data_temp= data.frame(data_withID)
data_m4 = data.frame(data_withID)

for (i in 3:11){
  data_temp[,i] = (data_temp[,i] - min(data_temp[,i])) / (max(data_temp[,i]) - min(data_temp[,i]))
}
library(factoextra)
fviz_nbclust(data_temp[3:11], kmeans, method = "silhouette")
fviz_cluster(clusters_m3, data = data_temp[3:11])

## M5 - K-Promotypes, k = 3
set.seed(1)
data_temp<-data.frame(data_withID)
data_m5 = data.frame(data_withID)
for (i in 3:11){
  data_temp[,i] = (data_temp[,i] - min(data_temp[,i])) / (max(data_temp[,i]) - min(data_temp[,i]))
}
library(clustMixType)
clusters_m5= kproto(data_temp[2:12], 3)
k.max <- 15
data <- na.omit(data_temp[2:12]) # to remove the rows with NA's
wss <- sapply(1:k.max, 
              function(k){kproto(data, k)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

centroid_denorm = clusters_m5$center
for (j in 3:11){
  for (i in 1:length(centroid_denorm[,1])){
    centroid_denorm[i,j-1] = centroid_denorm[i,j-1]*(max(data_m5[,j]) - min(data_m5[,j]))+min(data_m5[,j])
  }
}
clusters_m5
centroid_denorm

## M6 - K-prototpye, k = 4
set.seed(1)
data_temp<-data.frame(data_withID)
data_m6 = data.frame(data_withID)
library(clustMixType)
for (i in 3:11){
  data_temp[,i] = (data_temp[,i] - min(data_temp[,i])) / (max(data_temp[,i]) - min(data_temp[,i]))
}

clusters_m6 =kproto(data_temp[2:12], 4)
print(clusters_m6)
#fviz_cluster(clusters_m6, data = data_m6)
centroid_denorm = clusters_m6$center
clusters_m6$center
clusters_m6
for (j in 3:11){
  for (i in 1:length(centroid_denorm[,1])){
    centroid_denorm[i,j-1] = centroid_denorm[i,j-1]*(max(data_m6[,j]) - min(data_m6[,j]))+min(data_m6[,j])
  }
}

centroid_denorm

## M7 - Kmeans, k=4
set.seed(1)
data_m7 = data.frame(data_withID)
data_temp = data.frame(data_withID)
for (i in 3:11){
  data_temp[,i] = (data_temp[,i] - min(data_temp[,i])) / (max(data_temp[,i]) - min(data_temp[,i]))
}

clusters_m7 <- kmeans(data_temp[3:11], 4)
str(clusters_m7)
data_m7$cluster = clusters_m7$cluster

centroid_denorm = clusters_m7$center

for (j in 3:11){
  for (i in 1:length(centroid_denorm[,1])){
    centroid_denorm[i,j-2] = centroid_denorm[i,j-2]*(max(data_m7[,j]) - min(data_m7[,j]))+min(data_m7[,j])
  }
}

centroid_denorm
fviz_cluster(clusters_m7, data = data_temp[3:11])
k.max <- 15
data <- data_temp[3:11]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fviz_nbclust(data_temp[3:11], kmeans, method = "silhouette")
fviz_cluster(clusters_m7, data = data_temp[3:11])
clusters_m7$centers

centroid_denorm = clusters_m7$center

for (j in 3:11){
  for (i in 1:length(centroid_denorm[,1])){
    centroid_denorm[i,j-2] = centroid_denorm[i,j-2]*(max(data_m2[,j]) - min(data_m7[,j]))+min(data_m7[,j])
  }
}

centroid_denorm

## M8 - Kmeans with binary values, k = 4
set.seed(1)
data_temp<-data.frame(data_withID)
data_m8 = data.frame(data_withID)

levels(data_temp$Gender) <- c(1,0)
data_temp$Gender <- as.numeric(data_temp$Gender)
names(data_temp)[2] <- "isMale"
data_temp$Coupon <- as.factor(data_temp$Coupon)
levels(data_temp$Coupon) <- c(1,0)
data_temp$Coupon <- as.numeric(data_temp$Coupon)
names(data_temp)[12] <- "isCoupon"

data_temp$isMale= data_temp$isMale -1
data_temp$isCoupon =data_temp$isCoupon-1
str(data_temp)
for (i in 3:11){
  data_temp[,i] = (data_temp[,i] - min(data_temp[,i])) / (max(data_temp[,i]) - min(data_temp[,i]))
}
library(factoextra)
fviz_nbclust(data_temp[,2:12], kmeans, method = "silhouette")
k.max <- 15
data <- data_temp[,2:12]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")





clusters_m8 <- kmeans(data_temp[,2:12], 4)
str(clusters_m8)
data_m8$cluster = clusters_m8$cluster

clusters_m8$centers

centroid_denorm = clusters_m8$center

for (j in 3:11){
  for (i in 1:length(centroid_denorm[,1])){
    centroid_denorm[i,j-1] = centroid_denorm[i,j-1]*(max(data_m8[,j]) - min(data_m8[,j]))+min(data_m8[,j])
  }
}

centroid_denorm
fviz_cluster(clusters_m8, data = data_temp[2:12])

## M9 - Kmeans with one-got encoding, k = 4
set.seed(1)
data_temp<-data.frame(data_withID)
data_m9 = data.frame(data_withID)

levels(data_temp$Gender) <- c(1,0)
data_temp$Gender <- as.numeric(data_temp$Gender)
names(data_temp)[2] <- "isMale"
data_temp$Coupon <- as.factor(data_temp$Coupon)
levels(data_temp$Coupon) <- c(1,0)
data_temp$Coupon <- as.numeric(data_temp$Coupon)
names(data_temp)[12] <- "isCoupon"

data_temp$isMale= data_temp$isMale -1
data_temp$isCoupon =data_temp$isCoupon-1

male_list = numeric(length(data_temp[,1]))
female_list = numeric(length(data_temp[,1]))
yesCoupon_list = numeric(length(data_temp[,1]))
noCoupon_list = numeric(length(data_temp[,1]))

for (i in 1:length(data_temp[,1])){
  if (data_temp$isMale[i] ==1){
    male_list[i] = 1
  }
  else {
    female_list[i] =1
  }
  if (data_temp$isCoupon[i] ==1){
    yesCoupon_list[i] =1
  }
  else {
    noCoupon_list[i] =1
  }
}

data_temp$male = male_list
data_temp$female = female_list
data_temp$hasCoupon = yesCoupon_list
data_temp$noCoupon = noCoupon_list
data_temp = subset(data_temp,select=-c(isCoupon,isMale))

for (i in 2:10){
  data_temp[,i] = (data_temp[,i] - min(data_temp[,i])) / (max(data_temp[,i]) - min(data_temp[,i]))
}
library(factoextra)
fviz_nbclust(data_temp[,2:14], kmeans, method = "silhouette")
k.max <- 15
data <- data_temp[,2:14]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares") # 5

clusters_m9 <- kmeans(data_temp[,2:14], 4)
fviz_cluster(clusters_m9, data = data_temp[2:14])
