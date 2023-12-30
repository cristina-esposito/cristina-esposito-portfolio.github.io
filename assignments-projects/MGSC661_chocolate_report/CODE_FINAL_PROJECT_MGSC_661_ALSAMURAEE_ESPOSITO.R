#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#                           1.0 DATA EXPLORATION
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

# Importing the dataset
cocoa=read.csv("C:\\Users\\crist\\Desktop\\MCGILL\\MGSC 661\\FINAL PROJECT\\flavors_of_cacao_updated.csv",fileEncoding="UTF-8-BOM", na.strings=c(""))
attach(cocoa)

# Viewing the dataset
View(cocoa)

# Getting summary statistics on the variables in the dataset
summary(cocoa)

# Dropping NAs from ingredients since this will be an important variable for analysis
cocoa<-cocoa[!is.na(cocoa$Number_ingredients),]
attach(cocoa)

### Taking a closer look at the categorical variables
# How many levels there are in the categorical variables?
nlevels(Company)
nlevels(Company_country)
nlevels(Company_continent)
nlevels(Country_Bean_Origin)
nlevels(Continent_Bean_Origin)
nlevels(Ingredients)
nlevels(Rating_Class)
nlevels(char1)
nlevels(char2)
nlevels(char3)
nlevels(char4)

# What are the levels in the categorical variables?
levels(Company)
levels(Company_country)
levels(Company_continent)
levels(Country_Bean_Origin)
levels(Continent_Bean_Origin)
levels(Ingredients)
levels(Rating_Class)
levels(char1) 
levels(char2)
levels(char3)
levels(char4)

# What are the frequencies of the levels in each categorical variable?
table(Company)
table(Company_country)
table(Company_continent)
table(Country_Bean_Origin)
table(Continent_Bean_Origin)
table(Ingredients)
table(Rating_Class)
table(char1) # should maybe only focus on first 2 characteristics
table(char2)
table(char3)
table(char4)


### Taking a closer look at the continuous variables
# Boxplots and histograms to understand the distributions of the continuous variables
par(mfrow=c(3,2))
# Boxplot and histogram for chocolate bar ratings
boxplot(Rating, main="Boxplot of Chocolate Bar Rating",horizontal = TRUE, col = "#BB4444") 
hist(Rating, main="Histogram of Rating", xlab="Chocolate Bar Rating",col = "#BB4444", breaks=5)

# Boxplot and histogram for number of ingredients in the chocolate bars
boxplot(Number_ingredients, main="Boxplot of Number of Ingredients in the Chocolate Bar",horizontal = TRUE, col = "#4477AA") 
hist(Number_ingredients, main="Histogram of Rating", xlab="Number of Ingredients in the Chocolate Bar",col = "#4477AA", breaks=6)

# Boxplot and histogram for the cocoa percentage in the chocolate bars
boxplot(Cocoa_Percent, main="Boxplot of Cocoa Percentage", horizontal = TRUE, col = "#EE9988") 
hist(Cocoa_Percent, main="Histogram of Cocoa Percentage", xlab="Cocoa Percentage", col = "#EE9988", breaks=10)
par(mfrow=c(1,1))



### Looking at the relationships between the variables
library(RColorBrewer)

# Compute the frequency of Continent_Bean_Origin
library(dplyr)
library(ggplot2)
library(ggpubr)
df <- cocoa %>%
  group_by(Continent_Bean_Origin) %>%
  summarise(Frequency = n())
df
# Remove blank observations
df = df[!(is.na(df$Continent_Bean_Origin) | df$Continent_Bean_Origin==""), ]
df

# Bar chart of Number of Chocolate Bars by Continent of Bean Origin 
a= ggplot(df, aes(x = Continent_Bean_Origin, y = Frequency)) +
  geom_bar(fill = "#4477AA", stat = "identity", width = 0.5, position = position_dodge()) +
  geom_text(aes(label = Frequency), vjust = -0.4) + 
  ggtitle("Number of Chocolate Bars by Continent of Bean Origin") +  theme_pubclean()+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "Number of chocolate bars")
a 


# Compute the frequency of Country_Bean_Origin
df <- cocoa %>%
    group_by(Country_Bean_Origin) %>%
    summarise(Frequency = n())
df 
  
  
#Remove blank answers
  df = df[!(is.na(df$Country_Bean_Origin) | df$Country_Bean_Origin==""), ]
  df
# Descending order based on frequency
  df <- df[order(df$Frequency, decreasing = TRUE),]
  df = head(df, n=10)
  

b= ggplot(df, aes(x = Country_Bean_Origin, y = Frequency)) +
    geom_bar(fill = "#4477AA", stat = "identity", width = 0.5, position = position_dodge()) +
    geom_text(aes(label = Frequency), vjust = -0.4) + 
    ggtitle("Number of Chocolate Bars by Top 10 Country of Bean Origin") +  theme_pubclean()+ theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = "", y = "Number of chocolate bars")     
b


# Put both graphs together
require(gridExtra)
grid.arrange(a, b, nrow=2)  




# Average rating based company continent
tab = cocoa %>%
  group_by(Company_continent) %>%
  summarise_at(vars(Rating), list(name = mean))

tab


# Average rating based company continent
tab = cocoa %>%
  group_by(Number_ingredients) %>%
  summarise_at(vars(Rating), list(Avg = mean))

tab


# Average rating based on number of ingredients
b= ggplot(df, aes(x = Country_Bean_Origin, y = Frequency)) +
  geom_bar(fill = "#4477AA", stat = "identity", width = 0.5, position = position_dodge()) +
  geom_text(aes(label = Frequency), vjust = -0.4) + 
  ggtitle("Number of Chocolate Bars by Top 10 Country of Bean Origin") +  theme_pubclean()+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "Number of chocolate bars")     

c= ggplot(tab, aes(x = Number_ingredients, y = Avg)) +
  geom_bar(fill = "#4477AA", stat = "identity", width = 0.3, position = position_dodge()) +
  geom_text(aes(label = round(Avg, digits = 2)), vjust = -0.4) + 
  ggtitle("Average Rating Based on Number of Ingredients") +  theme_pubclean()+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Number of ingredients", y = "Rating") +
   theme(axis.text = element_text(colour = "black"))

c


# Number of chocolate bar ratings per year
df <- cocoa %>%
  group_by(Review_Date) %>%
  summarise(Frequency = n())
df 

y= ggplot(df, aes(x = Review_Date, y = Frequency)) +
  geom_bar(fill = "#4477AA", stat = "identity", width = 0.5, position = position_dodge()) +
  geom_text(aes(label = Frequency), vjust = -0.4) + 
  ggtitle("Number of Chocolate Bar Ratings Per Year (2006-2021)") +  theme_pubclean()+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Year", y = "Number of chocolate bars")     
y


# Barplot to understand where continents of manufacturer's source their beans from
counts<-table(Continent_Bean_Origin, Company_continent, exclude = NA)
barplot(counts, beside=TRUE, legend=TRUE, args.legend=list(bty = "n", x = "top", ncol = 3),ylim=c(0,700),main="Where Manufacturer's Source their Beans",xlab = "Manufacturer's Continent", ylab = "Frequency", col=brewer.pal(n = 6, name = "RdBu"))


# WORD CLOUD to understand the characteristics of chocolate bars
source('http://www.sthda.com/upload/rquery_wordcloud.r')
filePath <- "C:\\Users\\crist\\Desktop\\MCGILL\\MGSC 661\\FINAL PROJECT\\flavors_of_cacao_updated_char.csv"
res<-rquery.wordcloud(filePath, type ="file", lang = "english", colorPalette = "RdBu")


#Corralelogram

#get only numerical values
cocoa_num = select_if(cocoa, is.numeric)

#Drop beans column because irrelevant (every bar has beans)
cocoa_num = select(cocoa_num, -Beans)
cocoa_num = select(cocoa_num, -REF)
names(cocoa_num)<-c("Review Date","Cocoa Percent","Rating","Number Ingredients","Sugar", "Sweetener", "Cocoa Butter", "Vanilla","Lecithin","Salt")



library(corrplot)
M = cor(cocoa_num)
corrplot(M, method = 'number') # colorful number



col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)




# Barplot to understand the ratings of the continents with manufacturers
counts<-table(Rating_Class, Company_continent)
barplot(counts, beside=TRUE, legend=TRUE, args.legend=list(bty = "n", x = "top", ncol = 3),ylim=c(0,700),main="Rating by Manufacturer's Continent",xlab = "Manufacturer's Continent", ylab = "Frequency", col=c("#67a9cf","#ef8a62","#b2182b","#fddbc7","#2166ac")) 

# Barplot to understand the ratings of the continents that supply beans
counts<-table(Rating_Class, Continent_Bean_Origin)
barplot(counts, beside=TRUE, legend=TRUE, args.legend=list(bty = "n", x = "top", ncol = 3),ylim=c(0,700),main="Rating by Bean Origin's Continent",xlab = "Bean Origin's Continent", ylab = "Frequency", col=c("#67a9cf","#ef8a62","#b2182b","#fddbc7","#2166ac")) 

# Barplot to understand the ratings of the continents over time
counts<-table(Rating_Class, Review_Date)
barplot(counts, beside=TRUE, legend=TRUE, args.legend=list(bty = "n", x = "top", ncol = 3),ylim=c(0,150),main="Rating by Year of Review",xlab = "Year of Review", ylab = "Frequency", col=c("#67a9cf","#ef8a62","#b2182b","#fddbc7","#2166ac")) 

# Barplot to understand the ratings of each ingredient group
counts<-table(Rating_Class, Ingredients)
barplot(counts, beside=TRUE, las=2,cex.names=0.4,legend=TRUE, args.legend=list(bty = "n", x = "top", ncol = 3),ylim=c(0,500),main="Rating by Ingredients",xlab = "Ingredients", ylab = "Frequency", col=c("#67a9cf","#ef8a62","#b2182b","#fddbc7","#2166ac")) 

# Barplot to understand the ratings in relation to number of ingredients used in the chocolate bars
counts<-table(Rating_Class, Number_ingredients)
barplot(counts, beside=TRUE, legend=TRUE, args.legend=list(bty = "n", x = "top", ncol = 3),ylim=c(0,500),main="Rating by Number of Ingredients in the Chocolate Bar",xlab = "Number of Ingredients in the Chocolate Bar", ylab = "Frequency", col=c("#67a9cf","#ef8a62","#b2182b","#fddbc7","#2166ac")) 




# Barplot to understand the ratings in relation with sugar used in the chocolate bars
counts<-table(Rating_Class, Sugar)
barplot(counts, beside=TRUE, legend=TRUE, names.arg=c("Does not contain sugar","Does contain sugar"),args.legend=list(bty = "n", x = "top", ncol = 3),ylim=c(0,1000),main="Rating by Chocolate Bars Containing Sugar",xlab = "Chocolate Bars Containing Sugar", ylab = "Frequency", col=c("#67a9cf","#ef8a62","#b2182b","#fddbc7","#2166ac")) 

# Barplot to understand the ratings in relation with sweetener used in the chocolate bars
counts<-table(Rating_Class, Sweetener)
barplot(counts, beside=TRUE, legend=TRUE, names.arg=c("Does not contain sweetener","Does contain sweetener"),args.legend=list(bty = "n", x = "top", ncol = 3),ylim=c(0,1200),main="Rating by Chocolate Bars Containing Sweetener",xlab = "Chocolate Bars Containing Sweetener", ylab = "Frequency", col=c("#67a9cf","#ef8a62","#b2182b","#fddbc7","#2166ac")) 

# Barplot to understand the ratings in relation with cocoa butter used in the chocolate bars
counts<-table(Rating_Class, Cocoa_Butter)
barplot(counts, beside=TRUE, legend=TRUE, names.arg=c("Does not contain cocoa butter","Does contain cocoa butter"),args.legend=list(bty = "n", x = "top", ncol = 3),ylim=c(0,700),main="Rating by Chocolate Bars Containing Cocoa Butter",xlab = "Chocolate Bars Containing Cocoa Butter", ylab = "Frequency", col=c("#67a9cf","#ef8a62","#b2182b","#fddbc7","#2166ac")) 

# Barplot to understand the ratings in relation with vanilla used in the chocolate bars
counts<-table(Rating_Class, Vanilla)
barplot(counts, beside=TRUE, legend=TRUE, names.arg=c("Does not contain vanilla","Does contain vanilla"),args.legend=list(bty = "n", x = "top", ncol = 3),ylim=c(0,1000),main="Rating by Chocolate Bars Containing Vanilla",xlab = "Chocolate Bars Containing Vanilla", ylab = "Frequency", col=c("#67a9cf","#ef8a62","#b2182b","#fddbc7","#2166ac")) 

# Barplot to understand the ratings in relation with lecithin used in the chocolate bars
counts<-table(Rating_Class, Lecithin)
barplot(counts, beside=TRUE, legend=TRUE, names.arg=c("Does not contain lecithin","Does contain lecithin"),args.legend=list(bty = "n", x = "top", ncol = 3),ylim=c(0,1000),main="Rating by Chocolate Bars Containing Lecithin",xlab = "Chocolate Bars Containing Lecithin", ylab = "Frequency", col=c("#67a9cf","#ef8a62","#b2182b","#fddbc7","#2166ac")) 



### Visualizing the manufacturer and bean origin countries in respect to the various predictors
# need to install the package maps to be able to create maps
#install.packages("maps")

library(ggplot2)
library(dplyr)
require(maps)

# initializing the world map by country/region
world_map <- map_data("world")
table(world_map$region)


## plotting average rating by manufacturer on world map
# need to rename some of the countries to join the datasets
cocoa$Company_country<-recode(cocoa$Company_country, "U.S.A."="USA","U.K."="UK","U.A.E."="United Arab Emirates")
attach(cocoa)
cocoa_rating<-cocoa[,c("Company_country","Rating")]

# getting the average by country 
cocoa_rating_avg<-aggregate(cocoa_rating$Rating, list(cocoa_rating$Company_country),FUN=mean)
names(cocoa_rating_avg)<-c("Company_country","Average_Rating")

# joining the datasets
world_map_joined<-left_join(world_map, cocoa_rating_avg, by=c("region"="Company_country"))

# plotting the map
ggplot(world_map_joined, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = Average_Rating), color = "white")+
  scale_fill_viridis_c(option = "H")+labs(fill="Average Rating")+ggtitle("Average Rating by Country's Manufacturers")+theme(plot.title=element_text(hjust=0.5))


## plotting average rating by cocoa producer on world map
# need to rename some of the countries to join the datasets
cocoa$Country_Bean_Origin<-recode(cocoa$Country_Bean_Origin, "U.S.A."="USA","U.K."="UK","U.A.E."="United Arab Emirates")
attach(cocoa)
cocoa_rating<-cocoa[,c("Country_Bean_Origin","Rating")]

# getting the average by country
cocoa_rating_avg<-aggregate(cocoa_rating$Rating, list(cocoa_rating$Country_Bean_Origin),FUN=mean)
names(cocoa_rating_avg)<-c("Country_Bean_Origin","Average_Rating")

# joining the datasets
world_map_joined<-left_join(world_map, cocoa_rating_avg, by=c("region"="Country_Bean_Origin"))

# plotting the map
ggplot(world_map_joined, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = Average_Rating), color = "white")+
  scale_fill_viridis_c(option = "H")+labs(fill="Average Rating")+ggtitle("Average Rating by Bean Origin")+theme(plot.title=element_text(hjust=0.5))



## plotting average number of ingredients used by manufacturer on world map
# need to rename some of the countries to join the datasets
cocoa$Company_country<-recode(cocoa$Company_country, "U.S.A."="USA","U.K."="UK","U.A.E."="United Arab Emirates")
attach(cocoa)
cocoa_ing<-cocoa[,c("Company_country","Number_ingredients")]

# getting the average by country
cocoa_ing_avg<-aggregate(cocoa_ing$Number_ingredients, list(cocoa_ing$Company_country),FUN=mean,na.action = na.omit)
names(cocoa_ing_avg)<-c("Company_country","Average_Number_Ingredients")

# joining the datasets
world_map_joined<-left_join(world_map, cocoa_ing_avg, by=c("region"="Company_country"))

# plotting the map
ggplot(world_map_joined, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = Average_Number_Ingredients), color = "white")+
  scale_fill_viridis_c(option = "H")+labs(title="Average Number of Ingredients used by Country's Manufacturers",fill="Average Rating")


## plotting average sugar used by manufacturer on world map
# need to rename some of the countries to join the datasets
cocoa$Company_country<-recode(cocoa$Company_country, "U.S.A."="USA","U.K."="UK","U.A.E."="United Arab Emirates")
attach(cocoa)
cocoa_sugar<-cocoa[,c("Company_country","Sugar")]

# getting the average by country
cocoa_sugar<-aggregate(cocoa_sugar$Sugar, list(cocoa_sugar$Company_country),FUN=mean)
names(cocoa_sugar)<-c("Company_country","Average_Sugar")

# joining the datasets
world_map_joined<-left_join(world_map, cocoa_sugar, by=c("region"="Company_country"))

# plotting the map
ggplot(world_map_joined, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = Average_Sugar), color = "white")+
  scale_fill_viridis_c(option = "H")+labs(title="Likelihood of Sugar Usage by Country",fill="Average Rating")



## plotting average cocoa_butter used by manufacturer on world map
# need to rename some of the countries to join the datasets
cocoa$Company_country<-recode(cocoa$Company_country, "U.S.A."="USA","U.K."="UK","U.A.E."="United Arab Emirates")
attach(cocoa)
cocoa_b<-cocoa[,c("Company_country","Cocoa_Butter")]

# getting the average by country
cocoa_b<-aggregate(cocoa_b$Cocoa_Butter, list(cocoa_b$Company_country),FUN=mean)
names(cocoa_b)<-c("Company_country","Average_Cocoa_Butter")

# joining the datasets
world_map_joined<-left_join(world_map, cocoa_b, by=c("region"="Company_country"))

# plotting the map
ggplot(world_map_joined, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = Average_Cocoa_Butter), color = "white")+
  scale_fill_viridis_c(option = "H")+labs(title="Likelihood of Cocoa Butter Usage by Country",fill="Average Rating")


## plotting average vanilla usage by manufacturer on world map
# need to rename some of the countries to join the datasets
cocoa$Company_country<-recode(cocoa$Company_country, "U.S.A."="USA","U.K."="UK","U.A.E."="United Arab Emirates")
attach(cocoa)
cocoa_vanilla<-cocoa[,c("Company_country","Vanilla")]

# getting the average by country
cocoa_vanilla<-aggregate(cocoa_vanilla$Vanilla, list(cocoa_vanilla$Company_country),FUN=mean)
names(cocoa_vanilla)<-c("Company_country","Average_Vanilla")

# joining the datasets
world_map_joined<-left_join(world_map, cocoa_vanilla, by=c("region"="Company_country"))

# plotting the map
ggplot(world_map_joined, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = Average_Vanilla), color = "white")+
  scale_fill_viridis_c(option = "H")+labs(title="Likelihood of Vanilla Usage by Country",fill="Average Rating")


## plotting average lecithin usage by manufacturer on world map
# need to rename some of the countries to join the datasets
cocoa$Company_country<-recode(cocoa$Company_country, "U.S.A."="USA","U.K."="UK","U.A.E."="United Arab Emirates")
attach(cocoa)
cocoa_l<-cocoa[,c("Company_country","Lecithin")]

# getting the average by country
cocoa_l<-aggregate(cocoa_l$Lecithin, list(cocoa_l$Company_country),FUN=mean)
names(cocoa_l)<-c("Company_country","Average_Lecithin")

# joining the datasets
world_map_joined<-left_join(world_map, cocoa_l, by=c("region"="Company_country"))

# plotting the map
ggplot(world_map_joined, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = Average_Lecithin), color = "white")+
  scale_fill_viridis_c(option = "H")+labs(title="Likelihood of Lecithin Usage by Country",fill="Average Rating")



### PCA
# defining the variables and labels
cocoa_vars=cocoa[,c("Cocoa_Percent", "Number_ingredients","Sugar","Sweetener","Cocoa_Butter","Lecithin","Salt")]
cocoa_labels=cocoa[,c("Company","Company_country","Company_continent","Country_Bean_Origin","Continent_Bean_Origin","Rating_Class","char1")]

# performing pca with the variables
pca=prcomp(cocoa_vars)
pca

# plotting the pca analysis - does not show anything interesting, some variables hard to read as they
# overlap with each other
library(ggfortify)
autoplot(pca, data = (cocoa_vars), loadings = TRUE, loadings.label = TRUE)
pve=(pca$sdev^2)/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))
par(mfrow=c(1,1))



#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#                           2.0 MODEL BUILDING
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

# Converting the categorical variables into factors
cocoa$Ingredients=as.factor(Ingredients)
cocoa$Company_continent=as.factor(Company_continent)
cocoa$Continent_Bean_Origin=as.factor(Continent_Bean_Origin)

# two sets of features
#   - Company_continent+Continent_Bean_Origin+Cocoa_Percent+Ingredients
#   - Company_continent+Continent_Bean_Origin+Cocoa_Percent+Number_ingredients+Sugar+Cocoa_Butter+Lecithin+Salt+Sweetener+Vanilla


### RANDOM FOREST
library(randomForest)

## using first feature group
set.seed(1)

## determining the best number of trees to use
myforest=randomForest(Rating~Company_continent+Continent_Bean_Origin+Cocoa_Percent+Ingredients, ntree=500, data=cocoa, importance=TRUE,na.action = na.omit)
myforest
# plotting the error rate vs number of trees
plot(myforest, main="Number of Trees vs Error Rate for Feature Group 1")
set.seed(1)
# model with the optimal trees
myforest40=randomForest(Rating~Company_continent+Continent_Bean_Origin+Cocoa_Percent+Ingredients, ntree=40, data=cocoa, importance=TRUE,na.action = na.omit)
myforest40

# determining which variables are important
set.seed(1)
importance(myforest40)
varImpPlot(myforest40, main="Variable Importance Plots for Feature Group 1")

# model with chosen variables of importance
set.seed(1)
randomForest(Rating~Cocoa_Percent+Ingredients, ntree=40, data=cocoa, importance=TRUE,na.action = na.omit, do.trace=50)



## using second feature group
## determining the best number of trees to use
set.seed(1)
myforest2=randomForest(Rating~Company_continent+Continent_Bean_Origin+Cocoa_Percent+Number_ingredients+Sugar+Cocoa_Butter+Lecithin+Salt+Sweetener+Vanilla, ntree=500, data=cocoa, importance=TRUE,na.action = na.omit)
myforest2
# plotting the error rate vs number of trees
plot(myforest2, main="Number of Trees vs Error Rate for Feature Group 2")
# model with the optimal trees
set.seed(1)
myforest30=randomForest(Rating~Company_continent+Continent_Bean_Origin+Cocoa_Percent+Number_ingredients+Sugar+Cocoa_Butter+Lecithin+Salt+Sweetener+Vanilla, ntree=30, data=cocoa, importance=TRUE,na.action = na.omit)
myforest30


# determining which variables are important
set.seed(1)
importance(myforest30)
varImpPlot(myforest30, main="Variable Importance Plots for Feature Group 2")

# model with chosen variables of importance
randomForest(Rating~Cocoa_Percent+Number_ingredients+Company_continent+Vanilla, ntree=30, data=cocoa, importance=TRUE,na.action = na.omit, do.trace=50)




### BOOSTED FOREST

library(gbm)
set.seed(1)

## using first feature group
boosted=gbm(Rating~Company_continent+Continent_Bean_Origin+Cocoa_Percent+Ingredients,distribution=
              "gaussian",n.trees=500, interaction.depth=4)
predicted_score=predict(boosted, newdata=cocoa, n.trees=500)
mean((predicted_score-Rating)^2)

## determining the best number of trees to use
set.seed(1)
best.iter=gbm.perf(boosted, method="OOB")

set.seed(1)
boosted18=gbm(Rating~Company_continent+Continent_Bean_Origin+Cocoa_Percent+Ingredients,distribution=
                "gaussian",n.trees=18, interaction.depth=4)
predicted_score=predict(boosted18, newdata=cocoa, n.trees=18)
mean((predicted_score-Rating)^2)


# determining which variables are important
set.seed(1)
summary(boosted18)

# model with chosen variables of importance
set.seed(1)
boosted18vars=gbm(Rating~Cocoa_Percent+Ingredients,distribution=
                    "gaussian",n.trees=18, interaction.depth=4)
predicted_score=predict(boosted18vars, newdata=cocoa, n.trees=18)
mean((predicted_score-Rating)^2)


### best model - making future predictions
value = data.frame(Cocoa_Percent=0.82,Ingredients="3- B,S,C")
predict(boosted18vars, value)

value = data.frame(Cocoa_Percent=0.68,Ingredients="4- B,S,C,L")
predict(boosted18vars, value)




## using second feature group
set.seed(1)
boosted2=gbm(Rating~Company_continent+Continent_Bean_Origin+Cocoa_Percent+Number_ingredients+Sugar+Cocoa_Butter+Lecithin+Salt+Sweetener+Vanilla,distribution=
               "gaussian",n.trees=500, interaction.depth=4)
predicted_score=predict(boosted2, newdata=cocoa, n.trees=500)
mean((predicted_score-Rating)^2)

## determining the best number of trees to use
set.seed(1)
best.iter=gbm.perf(boosted2, method="OOB")
boosted2_21=gbm(Rating~Company_continent+Continent_Bean_Origin+Cocoa_Percent+Number_ingredients+Sugar+Cocoa_Butter+Lecithin+Salt+Sweetener+Vanilla,distribution=
                  "gaussian",n.trees=21, interaction.depth=4)
predicted_score=predict(boosted2_21, newdata=cocoa, n.trees=21)
mean((predicted_score-Rating)^2)


# determining which variables are important
set.seed(1)
summary(boosted2_21)

#model with chosen variables of importance
boosted2_21vars=gbm(Rating~Cocoa_Percent+Vanilla+Continent_Bean_Origin,distribution=
                      "gaussian",n.trees=21, interaction.depth=4)
predicted_score=predict(boosted2_21vars, newdata=cocoa, n.trees=21)
mean((predicted_score-Rating)^2)




