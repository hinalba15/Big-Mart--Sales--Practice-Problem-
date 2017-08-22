# we'll use plyr and dplyr to manipulate the data
install.packages("plyr")
library(plyr)
library(dplyr)

# we'll use caret to dummify factors
# and to order the predictors by their 
# importance using Random Feature Elimination (RFE)
install.packages("caret")
library(caret)

# imputaion of missing values
library(mice)
install.packages("VIM")
library(VIM)

# parallel computing
install.packages("doParallel")
library(doParallel)
setwd("G:\\Hinal BA\\big-market")
train_data<- read.csv("Train_UWu5bXk.csv")
test_data<-read.csv("Test_u94Q5KV.csv")
#Dimension of the data-set
dim(train_data)
dim(test_data)
#check the variables and their types in train & test
str(train_data)
str(test_data)
# brief summary of train & test
summary(train_data)
summary(test_data)
test_data$Item_Outlet_Sales <-  0
combi<- rbind(train_data, test_data)
# the five different levels of Item_Fat_Content look fishy
levels(combi$Item_Fat_Content)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,
                                  c("LF" = "Low Fat", "low fat" = "Low Fat", "reg" = "Regular"))
# count fat levels for each Item type
fat <- as.data.frame( setNames(
  aggregate(
    combi$Item_Fat_Content, 
    by=list(Category=combi$Item_Type,
            Category=combi$Item_Fat_Content), 
    FUN= length),
  c("Item_Type", "Item_Fat_Content", "number")
))
fat
# clearly, assigning a fat content to non-food items, i.e. the
# categories "Health and Hygiene", "Household" and "Others"
# makes no sense.
# We'll introduce a fat level "None" for them

levels(combi$Item_Fat_Content) <- c(levels(combi$Item_Fat_Content), "None")

combi[ which(combi$Item_Type == "Health and Hygiene") ,]$Item_Fat_Content <- "None"
combi[ which(combi$Item_Type == "Household") ,]$Item_Fat_Content <- "None"
combi[ which(combi$Item_Type == "Others") ,]$Item_Fat_Content <- "None"

combi$Item_Fat_Content <- factor(combi$Item_Fat_Content)
str(combi)
# count fat levels for each Item type
fat <- as.data.frame( setNames(
  aggregate(
    combi$Item_Fat_Content, 
    by=list(Category=combi$Item_Type,
            Category=combi$Item_Fat_Content), 
    FUN= length),
  c("Item_Type", "Item_Fat_Content", "number")
))

fat
##############################################
#
# Also, some entries for Outlet_Size are empty
# let's temporarily call them "Other"
# we'll revisit the issue later
#
##############################################

levels(combi$Outlet_Size)[1] <- "Other"
##############################################
#
# taking care of missing values
#
##############################################

#any missing values?
table(is.na(combi))

#where exactly are those values missing?
colSums(is.na(combi))
# plenty of weight values are missing
# what can we learn about the weights?

library(ggplot2)

install.packages("gridExtra")
library(gridExtra)
# boxplot of weights vs Item type
ggplot(combi, aes(Item_Type, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Item Type")

# boxplot of weights vs. Outlet Identifier
ggplot(combi, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Outlet identifier")

# Interesting! So, OUT019 and OUT027 have not provided any weight data

# assuming that each item identifier actually identifies a unique item,
# hence a unique weight, let's create a dataframe containing the mean
# weights and standard deviations by item identifier
weightsByItem <- as.data.frame( ddply(na.omit(combi), 
                                      ~Item_Identifier, 
                                      summarise, 
                                      mean=mean(Item_Weight), 
                                      sd=sd(Item_Weight)))

# we can now use these values to fill in the missing weight values:
combi$Item_Weight <- ifelse(is.na(combi$Item_Weight), 
                            weightsByItem$mean[
                              match(combi$Item_Identifier, weightsByItem$Item_Identifier)], combi$Item_Weight)
#any values still missing?
table(is.na(combi))
# let's redo the plots we looked at earlier
# boxplot of weights vs Item type
ggplot(combi, aes(Item_Type, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Item Type")
# boxplot of weights vs. Outlet Identifier
ggplot(combi, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Outlet identifier")
# the data are from 2013, hence we'll replace the
# Outlet Establishment Year by the number of years
# it has existed till 2013
combi$Year <- as.factor(2013 - combi$Outlet_Establishment_Year)

# Now, we can drop Outlet_Establishment_Year, as we've replaced it
# by the new column Year
combi <- select(combi, -c(Outlet_Establishment_Year))

# let's have a look at the Item_MRP

ggplot(combi, aes(x=Item_MRP)) + 
  geom_density(color = "blue", adjust=1/5) +
  geom_vline(xintercept = 69, color="red")+
  geom_vline(xintercept = 136, color="red")+
  geom_vline(xintercept = 203, color="red") + 
  ggtitle("Density of Item MRP")
# Clearly, there are four different price categories
# We'll introduce a new factor MRP_Level to mark
# those groups

combi$MRP_Level <- as.factor(
  ifelse(combi$Item_MRP < 69, "Low",
         ifelse(combi$Item_MRP < 136, "Medium",
                ifelse(combi$Item_MRP < 203, "High", "Very_High")))
)

# reorder the dataset such that the response variable Item_Outlet_Sales comes last
combi <- select( combi, c(Item_Identifier,
                          Item_Weight,
                          Item_Fat_Content,
                          Item_Visibility,
                          Item_Type,
                          Item_MRP,
                          Outlet_Identifier,
                          Outlet_Size,
                          Outlet_Location_Type,
                          Outlet_Type,
                          Year,
                          MRP_Level,
                          Item_Outlet_Sales
))

str(combi)
# how often does each Outlet_Identifier appear in the data
aggregate(combi$Outlet_Identifier, by=list(Category=combi$Outlet_Identifier), FUN=length)

# clearly, the two grocery stores, OUT010 and OUT019 have reported far
# less data than the supermarkets.
# From the data and their description it's not really clear why.
# In the following I'll assume that it's just because they are
# much smaller and therefore have a smaller selection
# of goods to buy.
# As a check let's count the Item IDs:

aggregate(combi$Item_Identifier, by=list(Category=combi$Outlet_Identifier), FUN= length)
# Interesting! Exactly the same numbers as above
# when we counted how often each outlet appears
# in the data.
# Our suspicion is therefore confirmed, grocery
# stores simply have a smaller selection of
# wares to sell.

# What else can we learn about the different types of shops?

# boxplot of  Sales vs. Outlet identifier
ggplot(combi[1:nrow(train_data),], aes(Outlet_Identifier, Item_Outlet_Sales)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet identifier") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet identifier")

# boxplot of  Sales vs. Outlet Type
ggplot(combi[1:nrow(train_data),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet Type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet Type")
# Sales in the one type 2 supermarket appear a bit low,
# as one would expect them to be higher than in
# the type 1 supermarkets.
# Maybe it's because it's still fairly new, having
# been founded 4 years ago.

# boxplot of  Sales vs. Outlet Type
ggplot(combi[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet type")

# count the number of others per Outlet_Identifier and Outlet_Type
otherShops <- as.data.frame( setNames(
  aggregate(
    combi$Outlet_Size, 
    by=list(Category=combi$Outlet_Identifier, 
            Category=combi$Outlet_Type,
            Category=combi$Outlet_Location_Type,
            Category=combi$Outlet_Size), 
    FUN= length),
  c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")
))
otherShops
# a Grocery store certainly falls in the size category Small
# Type 1 supermarkets are most often classified as Small,
# i.e. the mode is "Small"
# Hence, we'll classify the two "Other" Type 1 supermarkets
# as "Small"
# so we only have to replace "Other" by "Small"
#levels(combi$Outlet_Size)[1] <- "Small"

# "OUT010" is clearly small
combi[ which(combi$Outlet_Identifier == "OUT010") ,]$Outlet_Size <- "Small"
# "OUT017" and "OUT045" could be small or medium
combi[ which(combi$Outlet_Identifier == "OUT017") ,]$Outlet_Size <- "Small"
combi[ which(combi$Outlet_Identifier == "OUT045") ,]$Outlet_Size <- "Small"
# count the number of others per Outlet_Identifier and Outlet_Type
otherShops <- as.data.frame( setNames(
  aggregate(
    combi$Outlet_Size, 
    by=list(Category=combi$Outlet_Identifier, 
            Category=combi$Outlet_Type,
            Category=combi$Outlet_Location_Type,
            Category=combi$Outlet_Size), 
    FUN= length),
  c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")
))
otherShops

# apply factor to Outlet_Size in order to drop the now
# unused level "Other"
combi$Outlet_Size <- factor(combi$Outlet_Size)

str(combi)
# boxplot of  Sales vs. Outlet location
ggplot(combi[1:nrow(train),], aes(x = Outlet_Location_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet location") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet location")
# boxplot of  Sales vs. Outlet type
ggplot(combi[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet type")

# boxplot of  Sales vs. Item type
ggplot(combi[1:nrow(train),], aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")
# boxplot of Visibility vs Item type
ggplot(combi, aes(Item_Type, Item_Visibility, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Item Type")

# boxplot of Visibility vs. Outlet Identifier
ggplot(combi, aes(Outlet_Identifier, Item_Visibility)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Outlet identifier")

# We use the MICE package to impute those missing values

# to compare the visibility distributions before
# and after imputing the missing values
# we create a copy of the non-vanishing entries
combiNonZeroVis <- subset(combi, Item_Visibility > 0)

# replace 0 by NA so that mice can work its magic
outletIdentifiers <- levels(combi$Outlet_Identifier)
itemTypes <- levels(combi$Item_Type)
for (outName in outletIdentifiers) {
  for (itemName in itemTypes) {
    combi[ which(combi$Outlet_Identifier == outName &
                   combi$Item_Type == itemName),]$Item_Visibility <-
      ifelse(
        combi[ which(combi$Outlet_Identifier == outName &
                       combi$Item_Type == itemName), ]$Item_Visibility == 0 ,
        NA ,
        combi[ which(combi$Outlet_Identifier == outName &
                       combi$Item_Type == itemName),]$Item_Visibility
      )
  }
}
#any missing values now?
table(is.na(combi))
colSums(is.na(combi))
#pattern of missing values
md.pattern(combi)
# let mice impute the missing visibilities
newCombi <- mice(combi,m=1,maxit=1,meth='pmm',seed=0)
# summary of imputations
summary(newCombi)
# comparison of the distribution of existing
# and imputed visibilities
densityplot(newCombi)
stripplot(newCombi, pch = 20, cex = 1.2)

# let's replace the NA (formerly zero) values
# by the imputed ones
combi <- complete(newCombi,1)

# total visibility per shop
# should be 100
shopSum <- as.data.frame(setNames(
  aggregate(combi$Item_Visibility, by=list(Category=combi$Outlet_Identifier), FUN=sum),
  c("Outlet_Identifier", "TotVis")))

shopSum

# let's normalize all visibilities such that
# the total per shop comes out at 100

for (outName in outletIdentifiers) {
  combi[ which(combi$Outlet_Identifier == outName),]$Item_Visibility <-
    combi[ which(combi$Outlet_Identifier == outName),]$Item_Visibility *
    100/shopSum[ which(shopSum$Outlet_Identifier == outName),]$TotVis
}
shopSum <- as.data.frame(setNames(
  aggregate(combi$Item_Visibility, by=list(Category=combi$Outlet_Identifier), FUN=sum),
  c("Outlet_Identifier", "TotVis")))

shopSum
# densities of visibilities before and
# after imputation
ggplot() + 
  geom_density(aes(x=Item_Visibility), colour="red", data=combiNonZeroVis) + 
  geom_density(aes(x=Item_Visibility), colour="blue", data=combi)

# histograms of visibilities before and
# after imputation
ggplot(combiNonZeroVis[combiNonZeroVis$Outlet_Type %in% "Grocery Store", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) + 
  xlim(0.0,0.35) +
  xlab("Item visibility") + 
  ggtitle("Grocery Stores")
ggplot(combi[combi$Outlet_Type %in% "Grocery Store", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.35) +
  xlab("Item visibility") + 
  ggtitle("Grocery Stores")

ggplot(combiNonZeroVis[combiNonZeroVis$Outlet_Type %in% "Supermarket Type1", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.3) +
  xlab("Item visibility") + 
  ggtitle("Type 1")
ggplot(combi[combi$Outlet_Type %in% "Supermarket Type1", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.3) +
  xlab("Item visibility") + 
  ggtitle("Type 1")

ggplot(combiNonZeroVis[combiNonZeroVis$Outlet_Type %in% "Supermarket Type2", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.25) +
  xlab("Item visibility") + 
  ggtitle("Type 2")
ggplot(combi[combi$Outlet_Type %in% "Supermarket Type2", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.25) +
  xlab("Item visibility") + 
  ggtitle("Type 2")

ggplot(combiNonZeroVis[combiNonZeroVis$Outlet_Type %in% "Supermarket Type3", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.25) +
  xlab("Item visibility") + 
  ggtitle("Type 3")
ggplot(combi[combi$Outlet_Type %in% "Supermarket Type3", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.25) +
  xlab("Item visibility") + 
  ggtitle("Type 3")
# boxplot of Visibility vs. Outlet Identifier
ggplot(combiNonZeroVis, aes(Outlet_Identifier, Item_Visibility)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Outlet identifier")
ggplot(combi, aes(Outlet_Identifier, Item_Visibility)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Outlet identifier")

str(combi)
###############################################
#
# let's have a look at the item identifiers now,
# there are way too many of them.
#
# keeping only the first two letters gives us three groups:
# food, drink and non-food
#
###############################################

combi$Item_Class <- strtrim(combi$Item_Identifier, 2)
combi$Item_Class <- factor(combi$Item_Class)

levels(combi$Item_Class)

# keeping the first three letters of the Item identifier
# gives a somewhat higher granularity
combi$Item_Identifier <- strtrim(combi$Item_Identifier, 3)
combi$Item_Identifier <- factor(combi$Item_Identifier)


# let's have a look at the numerical variables now

# correlation between numerical variables
corMatrix <- cor(combi[1:nrow(train),][sapply(combi[1:nrow(train),], is.numeric)])
corMatrix

# a brief overview of the correlation matrix
corrplot::corrplot(corMatrix, method="number", type="upper")
corrplot::corrplot(corMatrix, method="number", type="upper", order="hclust")
# Item_Outlet_Sales has a strong positive correlation with Item_MRP
# and a somewhat weaker negative one with Item_Visibility
# Time for a quick principal component analysis
#
View(combi)
write.csv(combi,"combi.csv")

# Split the data back into a train set and a test set
train <- combi[1:nrow(train_data),]
test <- combi[-(1:nrow(train_data)),]



library(randomForest)
rm_model<- randomForest(Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + Item_Visibility
                        + Item_Type + Item_MRP + Outlet_Identifier + Outlet_Size        
                        + Outlet_Location_Type + Outlet_Type +Year + MRP_Level       
                        + Item_Class, data=train ,importance=TRUE, 
                        ntree=500)
rm_model
Predict_rm<- predict(rm_model,test,type="response")
summary(Predict_rm)
head(Predict_rm)
tail(Predict_rm)
write.csv(Predict_rm, file = "firstforest.csv", row.names = FALSE)
library('Boruta')
library('rFerns')
bor<- Boruta(Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + Item_Visibility
             + Item_Type + Item_MRP + Outlet_Identifier + Outlet_Size        
             + Outlet_Location_Type + Outlet_Type +Year + MRP_Level       
             + Item_Class, data =train,doTrace=2)
plot(bor)
attStats(bor)
trainBoruta <- getSelectedAttributes(bor)

library(randomForest)
rm_model1<- randomForest(Item_Outlet_Sales ~  Item_Visibility
                         + Item_Type + Item_MRP + Outlet_Identifier + Outlet_Size        
                         + Outlet_Location_Type + Outlet_Type +Year + MRP_Level       
                         , data=train ,importance=TRUE, 
                         ntree=500)
rm_model1
Predict_rm1<- predict(rm_model1,test,type="response")
tail(Predict_rm1)
write.csv(Predict_rm1, file = "firstforest2.csv", row.names = FALSE)