getwd()
library(stringr)
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)


carprice <- read.csv("carprice_assignment.csv",stringsAsFactors = FALSE)
#######################################################################################
#
#
#          PART_1 >>>> EXPLORATORY DATA ANALYSIS (E D A)<<<<
#
###################################################################################################
#checking first few records
head(carprice)

# structure of data shows the data types
str(carprice)

              #### Understanding data in term of business understanding #####

# 1. The dataset has 205 obs. of  26 variables.
# 2. carname has 2 parts: a) The first part is company name & b) car model name.
# 3. symboling: It is assigned insurance risk rating, 
#  a value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.
# 4. aspiration: Has 2 types, std & turbo.
# 5. fueltype has 2 types gas & diesel.
# 6. doornumber has 2 types: "4 doors" & "2 doors".
# 7. drivewheel has 3 types: 4wd= four wheel drive, fwd= front wheel drive & rwd=  rear wheel drive.
# 8. cylindernumber's are 2,3,4,5,6,8
# 9. citympg shows city miles per gallon
# 10. highwaympg shows highway miles per gallon
# 11. price of car is the target variable to be anlaysed in the data set.
# 12. The car name has company names which can be grouped into 3 categories: 
#      American: "dodge","plymouth","buick","chevrolet","mercury".
#      European: "audi","bmw","porsche","volkswagen","jaguar","peugeot","renault","saab",
#                 "volvo","alfa-romero"  
#      Japanese: " toyota ","honda","nissan","mazda","mitsubishi","subaru","isuzu"

###################################################################################################
                                   # NA's in the data set.

missing_values <- carprice %>%
  summarise_all(funs(sum(is.na(.))/n()))
missing_values #There are no NA's


nrow(unique(carprice)) #No duplicate records
View(carprice)
str(carprice)
#-------------------------------------------------------------------------------------------------
                                 #cleaning of the variable CarName
# 1: CarName
# 2: enginetype
# 3. Drop level= -2 in variable symboling
#------------------------------------------------------------------------------------------------
# 1: CarName

car_price1<-str_split_fixed(carprice$CarName," ",n=2)
car_price1<-gsub("toyouta","toyota",car_price1)
car_price1<-gsub("vw","volkswagen",car_price1)
car_price1<-gsub("vokswagen","volkswagen",car_price1)
car_price1<-gsub("porcshce","porsche",car_price1)
car_price1<-gsub("maxda","mazda",car_price1)
car_price1<-gsub("Nissan","nissan",car_price1)

car_price1<-as.data.frame(car_price1)
View(car_price1)
car_price1<-car_price1[-2]
# Removed carname column & car_id from dataframe carprice

car_price2<-carprice[-c(1,3)]

#merging and renaming CarName to company
car_price3<-cbind(car_price2,company= car_price1$V1)

# 2: enginetype

#12 entries in engine type are missing(|).Let us remove them

car_price3<-subset(car_price3,enginetype=="ohc"|enginetype=="ohcf"|enginetype=="ohcv"|enginetype=="dohc"
       |enginetype=="rotor"|enginetype=="dohcv")
dim(car_price3)

#-------------------------------------------------------------------------------------------------
                  #converting integer varaiable to numeric except symboling
#----------------------------------------------------------------------------------------
car_price4<-as.data.frame(car_price3[-1])


car_price4[sapply(car_price4, is.integer)] <- lapply(car_price4[sapply(car_price4, is.integer)], 
                                                     as.numeric)

#converting symboling to factor variable and merge with car_price4
symboling<-as.factor(car_price3$symboling)
car_price5<-cbind(car_price4,symboling)




table(car_price5$symboling)
# -2 -1  0  1  2  3 
#  3 22 67 54 32 27

# 3. As symboling = -2 has only 3 records, let us drop that level

car_price5 <- filter(car_price5, symboling==-1|symboling==0|symboling==1|symboling==2|symboling==3)%>%
  droplevels("-2")

levels(car_price5$symboling)
#----------------------------------------------------------------------------------------------------
# Selecting numeric data from car_price5 for the purpose of correlation

numericdata <-car_price5[sapply(car_price5, is.numeric)]

# Let us check the distribution of price
plot(density (car_price5$price))

# The plot shows that there is hump near 35000 ,which may be due to outlier
boxplot(car_price5$price)

# The boxplot confirms the above fact.There are  outliers. However, we will not treat this as it is for
# the dependent variable.
#--------------------------------------------------------------------------------------------------------
#               Treating the outliers in the independent numerics of dataset by capping.
# 
#------------------------------------------------------------------------------------------------------

boxplot(numericdata)
# Shows presence of Outliers in wheelbase,carlength,carwidth,enginesize,stroke,
#  compressionratio,horsepower,peakrpm,citympg & highwaympg.



#1 wheelbase

qnt <- quantile(numericdata$wheelbase, probs=c(.25, .75), na.rm = T)
caps <- quantile(numericdata$wheelbase, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(numericdata$wheelbase, na.rm = T)
numericdata$wheelbase[numericdata$wheelbase < (qnt[1] - H)] <- caps[1]
numericdata$wheelbase[numericdata$wheelbase > (qnt[2] + H)] <- caps[2]
length(numericdata$wheelbase)
summary(numericdata$wheelbase)

#2 carlength

summary(numericdata$carlength)
qnt <- quantile(numericdata$carlength, probs=c(.25, .75), na.rm = T)
caps <- quantile(numericdata$carlength, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(numericdata$carlength, na.rm = T)
numericdata$carlength[numericdata$carlength < (qnt[1] - H)] <- caps[1]
numericdata$carlength[numericdata$carlength > (qnt[2] + H)] <- caps[2]
length(numericdata$carlength)


#3 carwidth

summary(numericdata$carwidth)
qnt <- quantile(numericdata$carwidth, probs=c(.25, .75), na.rm = T)
caps <- quantile(numericdata$carwidth, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(numericdata$carwidth, na.rm = T)
numericdata$carwidth[numericdata$carwidth < (qnt[1] - H)] <- caps[1]
numericdata$carwidth[numericdata$carwidth > (qnt[2] + H)] <- caps[2]
length(numericdata$carwidth)

#4 enginesize

summary(numericdata$enginesize)
qnt <- quantile(numericdata$enginesize, probs=c(.25, .75), na.rm = T)
caps <- quantile(numericdata$enginesize, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(numericdata$enginesize, na.rm = T)
numericdata$enginesize[numericdata$enginesize < (qnt[1] - H)] <- caps[1]
numericdata$enginesize[numericdata$enginesize > (qnt[2] + H)] <- caps[2]
length(numericdata$enginesize)

#5 stroke


summary(numericdata$stroke)
qnt <- quantile(numericdata$stroke, probs=c(.25, .75), na.rm = T)
caps <- quantile(numericdata$stroke, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(numericdata$stroke, na.rm = T)
numericdata$stroke[numericdata$stroke < (qnt[1] - H)] <- caps[1]
numericdata$stroke[numericdata$stroke > (qnt[2] + H)] <- caps[2]
length(numericdata$stroke)

#6 compressionratio

summary(numericdata$compressionratio)
qnt <- quantile(numericdata$compressionratio, probs=c(.25, .75), na.rm = T)
caps <- quantile(numericdata$compressionratio, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(numericdata$compressionratio, na.rm = T)
numericdata$compressionratio[numericdata$compressionratio < (qnt[1] - H)] <- caps[1]
numericdata$compressionratio[numericdata$compressionratio > (qnt[2] + H)] <- caps[2]
length(numericdata$compressionratio)

#7 horsepower

summary(numericdata$horsepower)
qnt <- quantile(numericdata$horsepower, probs=c(.25, .75), na.rm = T)
caps <- quantile(numericdata$horsepower, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(numericdata$horsepower, na.rm = T)
numericdata$horsepower[numericdata$horsepower < (qnt[1] - H)] <- caps[1]
numericdata$horsepower[numericdata$horsepower > (qnt[2] + H)] <- caps[2]
length(numericdata$horsepower)


#8 peakrpm

summary(numericdata$peakrpm)
qnt <- quantile(numericdata$peakrpm, probs=c(.25, .75), na.rm = T)
caps <- quantile(numericdata$peakrpm, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(numericdata$peakrpm, na.rm = T)
numericdata$peakrpm[numericdata$peakrpm < (qnt[1] - H)] <- caps[1]
numericdata$peakrpm[numericdata$peakrpm > (qnt[2] + H)] <- caps[2]
length(numericdata$peakrpm)

#9 citympg

summary(numericdata$citympg)
qnt <- quantile(numericdata$citympg, probs=c(.25, .75), na.rm = T)
caps <- quantile(numericdata$citympg, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(numericdata$citympg, na.rm = T)
numericdata$citympg[numericdata$citympg < (qnt[1] - H)] <- caps[1]
numericdata$citympg[numericdata$citympg > (qnt[2] + H)] <- caps[2]
length(numericdata$citympg)
names(numericdata)

#10 highwaympg

summary(numericdata$highwaympg)
plot(density(numericdata$highwaympg))
boxplot(numericdata$highwaympg)
qnt <- quantile(numericdata$highwaympg, probs=c(.25, .75), na.rm = T)
caps <- quantile(numericdata$highwaympg, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(numericdata$highwaympg, na.rm = T)
numericdata$highwaympg[numericdata$highwaympg < (qnt[1] - H)] <- caps[1]
numericdata$highwaympg[numericdata$highwaympg > (qnt[2] + H)] <- caps[2]
length(numericdata$highwaympg)

dim(numericdata)
str(numericdata)


# delete the columns of carprice 5 for which outlier treatment is done
# and also remove the column "price" as it is already there in numericdata
car_price6 <-car_price5[-c(7,8,9,10,11,14,16,17,18,19,20,21,22,23)]
dim(car_price6)
str(car_price6)
car_price7 <- cbind(car_price6,numericdata)

#________________________________________________________________________________________________
                               #univariate Analysis-categorical variables
#_________________________________________________________________________________________________
#(1) Number of standard and turbo cars
table(car_price5$aspiration) # There are 166 standard cars and 30 turbo cars.So,popular aspiration
# is "standard".

#(2) 10 Top companies manufacturing cars
top_company<-sort(table(car_price5$company),decreasing = T)

#let us find the top 10 companies in terms of number of cars manufactured
head(top_company,10)          

#toyota     nissan      mazda      honda mitsubishi     subaru volkswagen 
#32         18         17         13         13         12         12 
#peugeot      dodge        bmw 
#11          9          8  


#(3) Types of fuel used and their numbers
table(car_price5$fueltype)
# There are 175 gas cars and 15 diesel cars.So, the most popular feul type is "gas".

#(4) Types of car segments in terms of their body
table(car_price5$carbody)
# There are 86 sedan, 69 hatchback, 8 hardtop and 6 convertibles.So, most popular carbody is "sedan"
# followed by "hatchback".

#(5)Different types of "drivewheel" and their numbers
table(car_price5$drivewheel)
# There are 119 fwd(front_wheel drive),62 rwd(rearwheel drive)and 9 4wd(four wheel drive)
# So, the most popular drivewheel is "fwd".

#(6) front and rear engine locations and their numbers
table(car_price5$enginelocation)
# There are 187 front and 3 rear engine locations

#(7) Different types of engines
table(car_price5$enginetype) # There are 145 ohc,15ohcf,13ohcv,12dohc,4 rotr and 1 dohcv type engines

#(8)Number of cylinders in cars
table(car_price5$cylindernumber)
# 145 fourcyl,24 six cyl,5 eight cyl,4 two cyl, and 1 twelve cyl cars

#(9)Fuel injection system and their numbers
table(car_price5$fuelsystem)
# 85 mpfi,66 "2bbl", 15 idi, 11 "1bbl" ,9 spdi,1 spfi engine fuel systems

#(10)Door numbers in a car
table(car_price5$doornumber)
# 101 four-door models and 89 two-door models. Hence four-door models appear to be more popular.


#____________________________________________________________________________________________
                          # univariate Analysis- numeric variables
#______________________________________________________________________________________________
# summary(car_price5$price)
ggplot(subset(car_price4,price>=29588),aes(x=price,fill=as.factor(company)))+
  geom_histogram(bins=25)
nrow(subset(car_price4,price>=29588)) 

#So, there are 15 high end cars

summary(car_price4$price)
nrow(subset(car_price4,price>=9988))
# There are 97 cars whose price is greater than median price(greater than 50 %) 9988

ggplot(subset(car_price5,price>=9988),aes(x=price,fill=as.factor(company)))+
  geom_histogram(position="dodge",bins=8)


nrow(subset(car_price5,price<=7775))
#There are 50 cars whose price is below the first quartile(less than 25 %)  price 7775


#_____________________________________________________________________________________
                                      #Bivariate analysis
#____________________________________________________________________________________
 
table(car_price7$carbody)

# convertible     hardtop   hatchback     sedan 
#        6           8          69          86 
# wagon 
# 21 

#Correlation #

corr_df<-as.data.frame(cor(numericdata)) 
print(corr_df)
View(corr_df)
corrplot(cor(numericdata), order = "FPC", method = "color",
         type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))
#There are many pairs of variables with very high correlation
#Few examples  are highwaympg and citympg,carlength & wheelbase, carwidth and wheelbase ,etc
#Removal dependent variables ,let us do  it in regression analysis

ggplot(data = car_price5, aes(x=price)) + geom_histogram(color="red", fill="green", bins = 1)+
  facet_grid(~carbody)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
#The number of sedans is highest and next two in order are hatchback and wagon


table(car_price7$carbody,car_price7$drivewheel)
#              4wd fwd rwd
# convertible   0   1   5
# hardtop       0   1   7
# hatchback     2  48  19
# sedan         3  57  26
# wagon         4  12   5

table(car_price7$carbody,car_price7$company)

#              alfa-romero audi bmw buick chevrolet dodge honda isuzu
# convertible           2    0   0     1         0     0     0     0
# hardtop               0    0   0     2         0     0     0     0
# hatchback             1    1   0     0         1     5     7     1
# sedan                 0    5   8     4         1     3     5     3
# wagon                 0    1   0     1         0     1     1     0
#               jaguar mazda mercury mitsubishi nissan plymouth porsche
# convertible      0     0       0          0      0        0       1
# hardtop          0     0       0          0      1        0       2
# hatchback        0    10       1          9      5        4       2
# sedan            3     7       0          4      9        2       0
# wagon            0     0       0          0      3        1       0
#               renault saab subaru toyota volkswagen volvo
# convertible       0    0      0      1          1     0
# hardtop           0    0      0      3          0     0
# hatchback         1    3      3     14          1     0
# sedan             0    3      5     10          9     5
# wagon             1    0      4      4          1     3




# price and count of cars manufactured by different companies
ggplot(data = car_price7, aes(x=price)) + geom_histogram(color="red", fill="green", bins = 1)+
  facet_grid(~company)+theme(axis.text.x = element_text(angle = 90, hjust = 2))
# price and count cars for different drive wheel values
ggplot(data = car_price7, aes(x=price)) + geom_histogram(color="red", fill="green", bins = 1)+
  facet_grid(~ drivewheel)+theme(axis.text.x = element_text(angle = 90, hjust = 2))
# price and count of cars for different fuel type

ggplot(data = car_price7, aes(x=price)) + geom_histogram(color="red", fill="green", bins = 1)+
  facet_grid(~ fueltype)+theme(axis.text.x = element_text(angle = 90, hjust = 2))

## price and count of cars for different aspiration values
ggplot(data = car_price7, aes(x=price)) + geom_histogram(color="red", fill="green", bins = 1)+
  facet_grid(~ aspiration)+theme(axis.text.x = element_text(angle = 90, hjust = 2))
 
## price and count of cars for differentsymboling
ggplot(data = car_price7, aes(x=price)) + geom_histogram(color="red", fill="green", bins = 1)+
  facet_grid(~ symboling)+theme(axis.text.x = element_text(angle = 90, hjust = 2))


#-----------------------------------------------------------------------------------------------
                             #PART2-----MULTIVARIABLE REGRESSION ANALYSIS------
#
#------------------------------------------------------------------------------------------

# Converting character variables to factor type
car_price7[sapply(car_price7, is.character)] <- lapply(car_price7[sapply(car_price7, is.character)], 
                                                       as.factor)

#--------------------------------------------------------------------------------------
#convert factors with 2 levels to numeric variables
#------------------------------------------------------------------------------------ 
#1.fueltype (1 is assigned to gas and 0 assigned deisel)
levels(car_price7$fueltype)<-c(1,0)
car_price7$fueltype <- as.numeric(levels(car_price7$fueltype))[car_price7$fueltype]


#2.aspiration
levels(car_price7$aspiration)<-c(1,0)
car_price7$aspiration <- as.numeric(levels(car_price7$aspiration))[car_price7$aspiration]

#3.doornumber
levels(car_price7$doornumber)<-c(1,0)
car_price7$doornumber <- as.numeric(levels(car_price7$doornumber))[car_price7$doornumber]


#4.enginelocation (1 assigned to front and 0 assigned rear)

levels(car_price7$enginelocation)<-c(1,0)
car_price7$enginelocation <- as.numeric(levels(car_price7$enginelocation))[car_price7$enginelocation]


#-----------------------------------------------------------------------------------
#convert factors with more than 2 levels to numeric variables using model.matrix
#____________________________________________________________________________________

#1.carbody
dummy_1 <- data.frame(model.matrix( ~carbody, data = car_price7))

dummy_1 <- dummy_1[,-1]

#2.drivewheel
dummy_2 <- data.frame(model.matrix( ~drivewheel, data = car_price7))

dummy_2 <- dummy_2[,-1]

#3.enginetype
dummy_3 <- data.frame(model.matrix( ~enginetype, data = car_price7))

dummy_3 <- dummy_3[,-1]

#4.cylindernumber
dummy_4 <- data.frame(model.matrix( ~cylindernumber, data = car_price7))

dummy_4 <- dummy_4[,-1]


#5.fuelsystem
dummy_5 <- data.frame(model.matrix( ~fuelsystem, data = car_price7))

dummy_5 <- dummy_5[,-1]

#6.company
dummy_6 <- data.frame(model.matrix( ~company, data = car_price7))

dummy_6 <- dummy_6[,-1]

#7.symboling
dummy_7 <- data.frame(model.matrix( ~symboling, data = car_price7))

dummy_7 <- dummy_7[,-1]


# Combine the dummy variables and the numeric columns of car_price7 dataset, in a new dataset called car_price8

str(car_price7)
names(car_price7)

car_price8 <- car_price7[-c(4,5,7,8,9,10,11)]
str(car_price8)

car_price9 <- cbind(car_price8, dummy_1,dummy_2,dummy_3,dummy_4,dummy_5,dummy_6,dummy_7)
str(car_price9) # 190 observations with 65 variables


#-----------------------------------------------------------------------------------------
                                       #     REGRESSION ANALYSIS
#------------------------------------------------------------------------------------------
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(car_price9), 0.7*nrow(car_price9))
train = car_price9[trainindices,]
test = car_price9[-trainindices,]

# Build model 1 containing all variables
model1 <-lm(price~.,data=train)
summary(model1) # lot of NA's indicate high degree of multicollinearity.

# Now, lets us apply stepAIC

# In stepAIC function, we pass model1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 


# Lets load the library in which stepAIC function exists

library(MASS)

step1 <- stepAIC(model1, direction="both")
#stepAIC runs 13 iterations. The step AIC value progressively decreases to AIC=2024.75
# This shows that the model obtained in the end is vastly superior to the starting
# plus against a variable indicates that the variable is removed.
# Many insignificant variables are removed
#Step:  AIC=2042.62,Step:  AIC=2040.71,Step:  AIC=2038.81,Step:  AIC=2036.94,
#Step:  AIC=2035.09,Step:  AIC=2033.32,Step:  AIC=2031.53,Step:  AIC=2029.77
#Step:  AIC=2028.04,Step:  AIC=2026.21,Step:  AIC=2025.16,Step:  AIC=2024.92
#Step:  AIC=2024.75.
step1
#-------------------------------------------------------------------------------------------
#Let us execute the new model obtained.Let us eliminate as many variables as possible using
# VIF (multicollinearity) and also p-value significance
#-------------------------------------------------------------------------------------------------
model2<- lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carheight + curbweight + enginesize + 
              horsepower + citympg + highwaympg + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelrwd + enginetypedohcv + enginetypeohcf + 
              enginetypeohcv + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + companyaudi + 
              companybmw + companybuick + companychevrolet + companydodge + 
              companyisuzu + companymazda + companymercury + companymitsubishi + 
              companynissan + companyplymouth + companyporsche + companyrenault + 
              companysaab + companytoyota + symboling0 + symboling2 + symboling3, 
            data = train)
            
 summary(model2)
 vif(model2)
 
# citympg , drivewheelrwd, companysaab,symboling0
# 45.871612     5.752454     2.501958    2.909480
# remove citympg
 
 model3<- lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
               wheelbase + carlength + carheight + curbweight + enginesize + 
               horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + enginetypedohcv + enginetypeohcf + 
               enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + companyaudi + 
               companybmw + companybuick + companychevrolet + companydodge + 
               companyisuzu + companymazda + companymercury + companymitsubishi + 
               companynissan + companyplymouth + companyporsche + companyrenault + 
               companysaab + companytoyota + symboling0 + symboling2 + symboling3, 
             data = train)
 
 summary(model3)
 vif(model3)
#  doornumber, wheelbase,carheight, drivewheelrwd,companysaab
#   4.004582    9.384233   5.885743   5.371974      2.479462
# remove  wheelbase
 
 model4<- lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
                carlength + carheight + curbweight + enginesize + 
               horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + enginetypedohcv + enginetypeohcf + 
               enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + companyaudi + 
               companybmw + companybuick + companychevrolet + companydodge + 
               companyisuzu + companymazda + companymercury + companymitsubishi + 
               companynissan + companyplymouth + companyporsche + companyrenault + 
               companysaab + companytoyota + symboling0 + symboling2 + symboling3, 
             data = train)
 
 summary(model4)
 vif(model4) 
 # doornumber, carheight,drivewheelrwd ,companysaab, symboling0
 #  3.947929    4.503292   5.303010       2.355055     2.880083
 # remove drivewheelrwd
 
 model5<- lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
               carlength + carheight + curbweight + enginesize + 
               horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
               carbodywagon +  enginetypedohcv + enginetypeohcf + 
               enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + companyaudi + 
               companybmw + companybuick + companychevrolet + companydodge + 
               companyisuzu + companymazda + companymercury + companymitsubishi + 
               companynissan + companyplymouth + companyporsche + companyrenault + 
               companysaab + companytoyota + symboling0 + symboling2 + symboling3, 
             data = train)
 
 summary(model5)
 vif(model5) 
 # doornumber, carheight, companysaab  
 #  3.895088    4.009788    2.006345
 # remove carheight
 
 model6<- lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
               carlength + curbweight + enginesize + 
               horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
               carbodywagon +  enginetypedohcv + enginetypeohcf + 
               enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + companyaudi + 
               companybmw + companybuick + companychevrolet + companydodge + 
               companyisuzu + companymazda + companymercury + companymitsubishi + 
               companynissan + companyplymouth + companyporsche + companyrenault + 
               companysaab + companytoyota + symboling0 + symboling2 + symboling3, 
             data = train)
 
 summary(model6)
 vif(model6) 
#  doornumber , companysaab
#   3.873643      1.878354
# remove  doornumber
 
 model7<- lm(formula = price ~ fueltype + aspiration +  enginelocation + 
               carlength + curbweight + enginesize + 
               horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
               carbodywagon +  enginetypedohcv + enginetypeohcf + 
               enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + companyaudi + 
               companybmw + companybuick + companychevrolet + companydodge + 
               companyisuzu + companymazda + companymercury + companymitsubishi + 
               companynissan + companyplymouth + companyporsche + companyrenault + 
               companysaab + companytoyota + symboling0 + symboling2 + symboling3, 
             data = train)
 
 summary(model7)
 vif(model7) 
 # companyrenault, companysaab, symboling0
 #  1.340687         1.878024     2.275551
 # remove symboling0
 
 model8<- lm(formula = price ~ fueltype + aspiration +  enginelocation + 
               carlength + curbweight + enginesize + 
               horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
               carbodywagon +  enginetypedohcv + enginetypeohcf + 
               enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + companyaudi + 
               companybmw + companybuick + companychevrolet + companydodge + 
               companyisuzu + companymazda + companymercury + companymitsubishi + 
               companynissan + companyplymouth + companyporsche + companyrenault + 
               companysaab + companytoyota +  symboling2 + symboling3, 
             data = train)
 
 summary(model8)
 vif(model8) 
 # companyrenault, companysaab
 #  1.301626        1.877734
 # remove companysaab
 
 model9<- lm(formula = price ~ fueltype + aspiration +  enginelocation + 
               carlength + curbweight + enginesize + 
               horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
               carbodywagon +  enginetypedohcv + enginetypeohcf + 
               enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + companyaudi + 
               companybmw + companybuick + companychevrolet + companydodge + 
               companyisuzu + companymazda + companymercury + companymitsubishi + 
               companynissan + companyplymouth + companyporsche + companyrenault + 
                companytoyota +  symboling2 + symboling3, 
             data = train)
 
 summary(model9)
 vif(model9) 
 # aspiration, companyrenault
 #  3.190842      1.298093
 # remove aspiration
 
 model10<- lm(formula = price ~ fueltype +   enginelocation + 
               carlength + curbweight + enginesize + 
               horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
               carbodywagon +  enginetypedohcv + enginetypeohcf + 
               enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + companyaudi + 
               companybmw + companybuick + companychevrolet + companydodge + 
               companyisuzu + companymazda + companymercury + companymitsubishi + 
               companynissan + companyplymouth + companyporsche + companyrenault + 
               companytoyota +  symboling2 + symboling3, 
             data = train)
 
 summary(model10)
 vif(model10) 
 # enginesize ,companyporsche, companyrenault
 #  17.134245    5.306463        1.282423 
 # remove enginesize
 
 model11<- lm(formula = price ~ fueltype +   enginelocation + 
                carlength + curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + enginetypeohcf + 
                enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + companyaudi + 
                companybmw + companybuick + companychevrolet + companydodge + 
                companyisuzu + companymazda + companymercury + companymitsubishi + 
                companynissan + companyplymouth + companyporsche + companyrenault + 
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model11)
 vif(model11) 
 # remove companyrenault
 
 model12<- lm(formula = price ~ fueltype +   enginelocation + 
                carlength + curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + enginetypeohcf + 
                enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + companyaudi + 
                companybmw + companybuick + companychevrolet + companydodge + 
                companyisuzu + companymazda + companymercury + companymitsubishi + 
                companynissan + companyplymouth + companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model12)
 vif(model12) 
 # carlength , fuelsystemmpfi,  companyisuzu, companyporsche 
 # 7.970822     5.690919          1.494357      5.176036
 # remove carlength
 
 model13<- lm(formula = price ~ fueltype +   enginelocation + 
                 curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + enginetypeohcf + 
                enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + companyaudi + 
                companybmw + companybuick + companychevrolet + companydodge + 
                companyisuzu + companymazda + companymercury + companymitsubishi + 
                companynissan + companyplymouth + companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model13)
 vif(model13) 
 # enginetypeohcv, fuelsystemmpfi, companyisuzu
 #   3.186699         4.765524       1.386749
 # remove fuelsystemmpfi
 
 model14<- lm(formula = price ~ fueltype +   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + enginetypeohcf + 
                enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl +  companyaudi + 
                companybmw + companybuick + companychevrolet + companydodge + 
                companyisuzu + companymazda + companymercury + companymitsubishi + 
                companynissan + companyplymouth + companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model14)
 vif(model14) 
 # enginetypeohcv, companyisuzu 
 #  2.977657         1.386367
 # remove enginetypeohcv
 
 model15<- lm(formula = price ~ fueltype +   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl +  companyaudi + 
                companybmw + companybuick + companychevrolet + companydodge + 
                companyisuzu + companymazda + companymercury + companymitsubishi + 
                companynissan + companyplymouth + companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model15)
 vif(model15) 
 # remove companyisuzu 
 
 model16<- lm(formula = price ~ fueltype +   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl +  companyaudi + 
                companybmw + companybuick + companychevrolet + companydodge + 
                 companymazda + companymercury + companymitsubishi + 
                companynissan + companyplymouth + companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model16)
 vif(model16) 
 # remove companychevrolet
 
 model17<- lm(formula = price ~ fueltype +   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl +  companyaudi + 
                companybmw + companybuick + companydodge + 
                companymazda + companymercury + companymitsubishi + 
                companynissan + companyplymouth + companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model17)
 vif(model17) 
 # remove companyplymouth
 
 model18<- lm(formula = price ~ fueltype +   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl +  companyaudi + 
                companybmw + companybuick + companydodge + 
                companymazda + companymercury + companymitsubishi + 
                companynissan +  companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model18)
 vif(model18) 
 #  remove enginetypeohcf
 
 model19<- lm(formula = price ~ fueltype +   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl +  companyaudi + 
                companybmw + companybuick + companydodge + 
                companymazda + companymercury + companymitsubishi + 
                companynissan +  companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model19)
 vif(model19) 
 # fueltype, fuelsystem2bbl, companydodge, companymazda, companynissan 
 #  3.265422    2.840912       1.209978       1.623266     1.424397
 # remove fueltype
 
 model20<- lm(formula = price ~   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl +  companyaudi + 
                companybmw + companybuick + companydodge + 
                companymazda + companymercury + companymitsubishi + 
                companynissan +  companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model20)
 vif(model20) 
 # fuelsystem2bbl, companydodge, companymazda,companymercury,companynissan 
 #  2.050243         1.184578       1.533244    1.233666       1.378981
 # remove fuelsystem2bbl
 
 model21<- lm(formula = price ~   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix +   companyaudi + 
                companybmw + companybuick + companydodge + 
                companymazda + companymercury + companymitsubishi + 
                companynissan +  companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model21)
 vif(model21) 
 # companydodge, companymazda, companymercury,companynissan
 #   1.114333      1.458357      1.228854       1.294897
 # remove companymazda
 
 model22<- lm(formula = price ~   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix +   companyaudi + 
                companybmw + companybuick + companydodge + 
                 companymercury + companymitsubishi + 
                companynissan +  companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model22)
 vif(model22) 
 # companydodge, companymercury,companynissan
 #  1.092048       1.228830       1.274607
 #  remove companynissan
 
 model23<- lm(formula = price ~   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix +   companyaudi + 
                companybmw + companybuick + companydodge + 
                companymercury + companymitsubishi + 
                  companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model23)
 vif(model23) 
 # companydodge, companymercury
 #  1.077480       1.227968
 # remove companymercury
 
 model24<- lm(formula = price ~   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix +   companyaudi + 
                companybmw + companybuick + companydodge + 
                 companymitsubishi + 
                companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model24)
 vif(model24) 
 # remove companydodge 
 
 model25<- lm(formula = price ~   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix +   companyaudi + 
                companybmw + companybuick +  
                companymitsubishi + 
                companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model25)
 vif(model25) 
 # remove companymitsubishi
 
 model26<- lm(formula = price ~   enginelocation + 
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix +   companyaudi + 
                companybmw + companybuick +  
                  companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model26)
 vif(model26) 
 # enginelocation, enginetypedohcv
 #  3.737406          2.561795
 # remove enginelocation
 
 model27<- lm(formula = price ~    
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix +   companyaudi + 
                companybmw + companybuick +  
                companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model27)
 vif(model27) 
 # companyaudi ,symboling2
 # 1.585169       1.316322
 # remove companyaudi
 
 model28<- lm(formula = price ~    
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix +    
                companybmw + companybuick +  
                companyporsche +  
                companytoyota +  symboling2 + symboling3, 
              data = train)
 
 summary(model28)
 vif(model28)
 # remove symboling2
 
 model29<- lm(formula = price ~    
                curbweight +  
                horsepower +  highwaympg + carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix +    
                companybmw + companybuick +  
                companyporsche +  
                companytoyota +   symboling3, 
              data = train)
 
 summary(model29)
 vif(model29)
 # highwaympg , enginetypedohcv, symboling3
 #  5.117516      1.962949         1.901031
 # remove highwaympg
 
 model30<- lm(formula = price ~    
                curbweight +  
                horsepower +   carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix +    
                companybmw + companybuick +  
                companyporsche +  
                companytoyota +   symboling3, 
              data = train)
 
 summary(model30)
 vif(model30)
 # horsepower, enginetypedohcv,cylindernumbersix 
 # 5.571037      1.849057        3.972540
 # remove horsepower
 
 model31<- lm(formula = price ~    
                curbweight +  
                   carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix +    
                companybmw + companybuick +  
                companyporsche +  
                companytoyota +   symboling3, 
              data = train)
 
 summary(model31)
 vif(model31)
 # enginetypedohcv, cylindernumbersix
 #  1.849056           3.871250 
 # remove cylindernumbersix
 
 model32<- lm(formula = price ~    
                curbweight +  
                carbodyhatchback + carbodysedan + 
                carbodywagon +  enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                  companybmw + companybuick +  
                companyporsche +  
                companytoyota +   symboling3, 
              data = train)
 
 summary(model32)
 vif(model32)
 # remove enginetypedohcv 
 
 model33<- lm(formula = price ~    
                curbweight +  
                carbodyhatchback + carbodysedan + 
                carbodywagon +   
                cylindernumberfive + cylindernumberfour + 
                companybmw + companybuick +  
                companyporsche +  
                companytoyota +   symboling3, 
              data = train)
 
 summary(model33)
 vif(model33)
 # remove symboling3
 
 model34<- lm(formula = price ~    
                curbweight +  
                carbodyhatchback + carbodysedan + 
                carbodywagon +   
                cylindernumberfive + cylindernumberfour + 
                companybmw + companybuick +  
                companyporsche +  
                companytoyota , 
              data = train)
 
 summary(model34) 
 vif(model34)
#  The above model has no star and 1 star variables. we will also consider this model, for the final
 #model to be chosen
 #-----------------------------------------------------------------------------
 # carbodysedan, cylindernumberfive, companytoyota
 #  4.310951        1.549605            1.072654
 # remove carbodysedan
 
 model35<- lm(formula = price ~    
                curbweight +  
                carbodyhatchback + 
                carbodywagon +   
                cylindernumberfive + cylindernumberfour + 
                companybmw + companybuick +  
                companyporsche +  
                companytoyota , 
              data = train)
 
 summary(model35)
 vif(model35)
 # carbodyhatchback, companytoyota
 # 1.190087             1.052936
 # remove carbodyhatchback
 
 model36<- lm(formula = price ~    
                curbweight +  
                  carbodywagon +   
                cylindernumberfive + cylindernumberfour + 
                companybmw + companybuick +  
                companyporsche +  
                companytoyota , 
              data = train)
 
 summary(model36)
 vif(model36)
 # carbodywagon, cylindernumberfive, companytoyota
 #  1.060994         1.450683          1.041581
 # remove cylindernumberfive
 
 model37<- lm(formula = price ~    
                curbweight +  
                carbodywagon +   
                 cylindernumberfour + 
                companybmw + companybuick +  
                companyporsche +  
                companytoyota , 
              data = train)
 
 summary(model37)
 vif(model37)
 # cylindernumberfour, companytoyota
 #  2.040708              1.036222
 # remove cylindernumberfour
 
 model38<- lm(formula = price ~    
                curbweight +  
                carbodywagon +   
                  companybmw + companybuick +  
                companyporsche +  
                companytoyota , 
              data = train)
 
 summary(model38)
 vif(model38)
 # remove companytoyota
 
 model39<- lm(formula = price ~    
                curbweight +  
                carbodywagon +   
                companybmw + companybuick +  
                companyporsche   
                 , 
              data = train)
 
 summary(model39)
 vif(model39)
 
 
 #-------------------------------------------------------------------------------------
              ## TWO MODELS AND THEIR EFFICIENCY
 #-----------------------------------------------------------------------------------
                                       # FIRST MODEL
#------------------------------------------------------------------------------------
Predict1 <- predict(model39,test[,-18])
View(Predict1)
test$testprice <- Predict1
View(test$testprice)
# Now, we need to test the r square between actual and predicted sales.
r <- cor(test$price,test$testprice)
rsquared <- cor(test$price,test$testprice)^2
rsquared
# rsqaured is 0.9052641. Thus the efficiency of the model prediction is 90.52641%.
#Adjusted R-squared: 0.8867 .Thus 88.67% of the variation is expalined by the model.
#regression equation analysis
# The regression equation is 
# lm(formula = price ~ -1.591e+04    
#      (1.119e+01)curbweight +  
#      (-3.003e+03)carbodywagon +   
#      (9.325e+03)companybmw + (8.404e+03)companybuick +  
#      (1.330e+04)companyporsche   
    
#1. when the curbweight increases,the average price increases ,keeping all other variables constant
#2. The company bmw has the highest brand value, followed by buick and porsche
#3. Every unit of car body wagon produced has a tendency to decrease the averageprice, keeping all other
# variables constant



#---------------------------------------------------------------------------------------------------
                                       # SECOND MODEL
#----------------------------------------------------------------------------------------------------
#Let us take model 34,where one star is completly removed
Predict2 <- predict(model34,test[,-18])
View(Predict2)
test$testprice <- Predict2
View(test$testprice)
# Now, we need to test the r square between actual and predicted sales.

rsquared1 <- cor(test$price,test$testprice)^2
rsquared1
#rsquared is 88.3465%. Thus the efficiency of the model prediction is 88.3465% 
#Adjusted R-squared:  0.9181. Thus 91.81 % of the variation is explained by the model
#regression equation analysis
# The regression equation is 
# #price ~ -5972.0014+(9.6526)curbweight   
#     (-3605.1275)carbodyhatchback +(-2586.6305) carbodysedan + 
#     (-5160.0586)carbodywagon    
#    (-3667.9702)cylindernumberfive +(-3430.2478) cylindernumberfour + 
#     (7994.7742)companybmw +(7624.7489) companybuick +  
#     (10497.9482)companyporsche +  
#     (-1734.3911)companytoyota
# Model 1 is slightly superior to model 2 in view of higher adjusted r -square

# ________________________________________________________________________

#                   Let us construct a regression model for prices vs company
#                   Impact of company brand on price (simple linear regression)                  
#----------------------------------------------------------------------------------------


#company to dummy variable
company_dummy <- data.frame(model.matrix( ~company, data = car_price7))
View(company_dummy)
company_dummy<- company_dummy[,-1]

# Combine the dummy variable and the price column of car_price7 dataset 
company_car_price <- car_price7[25]

# 
company_car_price1 <- cbind(company_car_price,company_dummy )
View(company_car_price1)

# separate training and testing data
set.seed(100)
company_trainindices= sample(1:nrow(car_price7), 0.7*nrow(company_car_price1))
company_train = company_car_price1[company_trainindices,]
company_test = company_car_price1[-company_trainindices,]

# # Build model 1 containing all variables
company_model1 <-lm(price~.,data=company_train)
summary(company_model1) 

company_step1 <- stepAIC(company_model1, direction="both")
company_step1
company_model2<-lm(formula = price ~ companybmw + companybuick + companychevrolet + 
                     companydodge + companyhonda + companyjaguar + companymazda + 
                     companymitsubishi + companynissan + companyplymouth + companyporsche + 
                     companyrenault + companysubaru + companytoyota + companyvolkswagen + 
                     companyvolvo, data = company_train)

summary(company_model2)
vif(company_model2)

#companyrenault,companyvolvo
#1.045166    , 1.300485 

# so remove companyvolvo

company_model3<-lm(formula = price ~ companybmw + companybuick + companychevrolet + 
                     companydodge + companyhonda + companyjaguar + companymazda + 
                     companymitsubishi + companynissan + companyplymouth + companyporsche + 
                     companyrenault + companysubaru + companytoyota + companyvolkswagen  
                   , data = company_train)

summary(company_model3)
vif(company_model3)

# remove companyrenault

company_model4<-lm(formula = price ~ companybmw + companybuick + companychevrolet + 
                     companydodge + companyhonda + companyjaguar + companymazda + 
                     companymitsubishi + companynissan + companyplymouth + companyporsche + 
                     companysubaru + companytoyota + companyvolkswagen  
                   , data = company_train)

summary(company_model4)
vif(company_model4)

# remove companychevrolet

company_model5<-lm(formula = price ~ companybmw + companybuick +  
                     companydodge + companyhonda + companyjaguar + companymazda + 
                     companymitsubishi + companynissan + companyplymouth + companyporsche + 
                     companysubaru + companytoyota + companyvolkswagen  
                   , data = company_train)

summary(company_model5)
vif(company_model5)

# remove companyvolkswagen

company_model5<-lm(formula = price ~ companybmw + companybuick +  
                     companydodge + companyhonda + companyjaguar + companymazda + 
                     companymitsubishi + companynissan + companyplymouth + companyporsche + 
                     companysubaru + companytoyota   
                   , data = company_train)

summary(company_model5)
vif(company_model5)

# companydodge, companymazda, companyplymouth

# 1.102330         1.228571         1.102330

# remove companymazda

company_model6<-lm(formula = price ~ companybmw + companybuick +  
                     companydodge + companyhonda + companyjaguar +  
                     companymitsubishi + companynissan + companyplymouth + companyporsche + 
                     companysubaru + companytoyota   
                   , data = company_train)

summary(company_model6)
vif(company_model6)

# companydodge, companyplymouth

#  1.067150        1.067150

# remove companydodge 

company_model7<-lm(formula = price ~ companybmw + companybuick +  
                     companyhonda + companyjaguar +  
                     companymitsubishi + companynissan + companyplymouth + companyporsche + 
                     companysubaru + companytoyota   
                   , data = company_train)

summary(company_model7)
vif(company_model7)

# companymitsubishi, companyplymouth, companysubaru

#   1.117021             1.057283        1.078287

# remove companymitsubishi


company_model8<-lm(formula = price ~ companybmw + companybuick +  
                     companyhonda + companyjaguar +  
                     companynissan + companyplymouth + companyporsche + 
                     companysubaru + companytoyota   
                   , data = company_train)

summary(company_model8)
vif(company_model8)

# remove companyplymouth  

company_model9<-lm(formula = price ~ companybmw + companybuick +  
                     companyhonda + companyjaguar +  
                     companynissan +  companyporsche + 
                     companysubaru + companytoyota   
                   , data = company_train)

summary(company_model9)
vif(company_model9)


# companynissan, companysubaru, companytoyota 

#  1.076345           1.048185     1.127451

# remove companytoyota


company_model10<-lm(formula = price ~ companybmw + companybuick +  
                      companyhonda + companyjaguar +  
                      companynissan +  companyporsche + 
                      companysubaru   
                    , data = company_train)

summary(company_model10)
vif(company_model10)

# companynissan, companysubaru

#   1.031689        1.021126

# remove companynissan

company_model11<-lm(formula = price ~ companybmw + companybuick +  
                      companyhonda + companyjaguar +  
                      companyporsche + 
                      companysubaru   
                    , data = company_train)

summary(company_model11)
vif(company_model11)

# remove companysubaru


company_model12<-lm(formula = price ~ companybmw + companybuick +  
                      companyhonda + companyjaguar +  
                      companyporsche 
                    , data = company_train)

summary(company_model12)
vif(company_model12)

# remove companyhonda

company_model13<-lm(formula = price ~ companybmw + companybuick +  
                      companyjaguar +  
                      companyporsche 
                    , data = company_train)

summary(company_model13)
vif(company_model13)

set.seed(100)
company_trainindices= sample(1:nrow(car_price7), 0.7*nrow(company_car_price1))
company_train = company_car_price1[company_trainindices,]
company_test = company_car_price1[-company_trainindices,]


Predict3 <- predict(company_model13,company_test[,-1])

company_test$testprice <- Predict3



r_company <- cor(company_test$price,company_test$testprice)
rsquared_company <- cor(company_test$price,company_test$testprice)^2
rsquared_company

# The prediction efficiency is 69.98%. 
#The top company brand is jaguar followed by buick,then porsch and bmw
 
 #_________________________________END_________________________________________________________________#

 
