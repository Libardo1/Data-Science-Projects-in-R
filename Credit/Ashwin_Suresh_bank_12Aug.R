getwd()
setwd("~/datasetscsvandrfiles")


My.col.df<-function (df){x<-data.frame(cbind(lapply(df,levels),sapply(df,class)))
x$colnum<-1:nrow(x)
names(x)[1]<-c("LEVELS")                
return(x)}

library(dplyr)
library(ggplot2)
library(MASS)
library(car)
library(cowplot)



##------------Bank Marketing Analysis---------------------##

#----------------------------------------------------------
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#-------------------------------------------------------
## Business Understanding:- Prospect Profiling
#-------------------------------------------------------
bank_data<- read.csv("bank_marketing.csv")
str(bank_data)
summary(bank_data)
#-------------------------------------------------------
# Checking response rate of prospect customer
table(bank_data$response)
levels(as.factor(bank_data$response))
response <- 4640/(36548+4640)
response   #11.26542%
# Checking missing values
sum(is.na(bank_data))
#-------------------------------------------------------
# Plotting Age histogram
ggplot(bank_data,aes(x=age))+geom_histogram(fill="#0073C2FF",binwidth=1,color="white",aes(y=..density..),show.legend = FALSE)+
  geom_density(alpha=.2,size=0.7,color="yellow") +
  geom_vline(data=bank_data, aes(xintercept=mean(age)),
             linetype="dashed",color="red",size=0.7)
# Let's check the outlier in the variables 
quantile(bank_data$age,seq(0,1,0.01))
# Box plot 
boxplot(bank_data$age)

# Capping the upper values of age with 71.
bank_data[(which(bank_data$age>71)),]$age <- 71
##########################################################
# Binning the age variable and store it into "binning.age".
bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 71)))
##########################################################
# Change the response value to numbers i.e"yes-no" to "1-0"
bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)
# Check the numeric value of response rate in each bucket
agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 
# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)
# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")
# Round Off the values
agg_age$response_rate <- format(round(agg_age$response_rate, 2))
agg_age
###########################################################
# Let's see the response rate of each age bucket in the plot

 ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
   geom_col(fill = "#0073C2FF",color="red",width=0.8) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
   geom_text(size = 4, vjust = -0.5,color="purple")
# 
# # Let's check the dataset of age less than 20 years. 
 Bank_data_age20 <- subset(bank_data,age <20)
##--------------------------------------------------------  
# Checking structure of dataset
 str(bank_data)
#Next Variable is "job"
# # Checking the levels of the job
 levels(bank_data$job)
#Plotting bar graph for job variable.
# Writing a function "plot_response" to do the same task for each variable
plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
    colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
    ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_col(fill = "#0073C2FF",color="red") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_text(size = 4, vjust = -0.5,color="purple")

}
plot_response(bank_data$job, "job")

##################################
myplot <- function(cat_var, var_name){
  
  a <- aggregate(response~cat_var, bank_data, mean)
  
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  
  agg_response <- mutate(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) +
    geom_col(alpha=0.7,fill ="brown",color="darkgreen") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1,color="navy blue")) +
    geom_label(size = 4, vjust = -0.5,color="purple")
}


myplot(bank_data$job, "job")



#navy blue, dark blue, skyblue,royalblue,rosybrown,gold,pink,purple,violet




############################################  
# # Checking structure of dataset 
 str(bank_data)
# # Checking Marital status
 summary(bank_data$marital)
# # Let's replace Unknown level to married
 levels(bank_data$marital)[4] <- "married"
# # Plotting marital status
 plot_response(bank_data$marital,"marital")
############################################## 
# # Let's see the education variables
 plot_response(bank_data$education,"Education")

##############################################
# Reducing the levels of education variable
##############################################
levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# # Let's again check the education plot
 plot_response(bank_data$education,"Education_levels")
#-------------------------------------------------------
# Let's see the default variable
 table(bank_data$default)
plot_response(bank_data$default, "Default")
 bank_data <- bank_data[,-5]
# #-------------------------------------------------------
# # Let's understand the housing variables 
 summary(bank_data$housing)
 plot_response(bank_data$housing, "Housing")
# #-------------------------------------------------------
# #-- Let's see the next variable which is "loan"
 summary(bank_data$loan)
 plot_response(bank_data$loan, "Loan Status")
# #############################################
 #CAMPAIGN INFORMATION
 ##############################################
# #  Next variable is Contact, Let's see the response rate of each mode 
 summary(bank_data$contact)
 plot_response(bank_data$contact,"Contact_mode")
# #-------------------------------------------------------
# # Next variable is "Month" i.e contact month. 
 plot_response(bank_data$month,"Contact_month")
# #-------------------------------------------------------
# # Let's do the same of "day_of_week" variable
 plot_response(bank_data$day_of_week,"day_of_week")
# #-------------------------------------------------------
# # Now, Let's see the "duration" variable: Which is Quantitative variable
# # Let's check the histogram 
ggplot(bank_data,aes(duration))+geom_histogram(fill="steelblue")
# Let's see the summary of this variable once
summary(bank_data$duration)
# Average duration
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)
bank_data <- bank_data[,-22]#remove response_1

##########################
# ## Definitely the outlier is present in the dataset
# # So let's check the percentile distribution of duration 
# quantile(bank_data$duration,seq(0,1,0.01))
# So, capping the duration seconds at 99% which is 1271.3sec 
bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13
# Now, again plot the histogram 
ggplot(bank_data,aes(duration))+geom_histogram(fill="steelblue",bins=100)

#-------------------------------------------------------
# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)
# So let's check the summay of this variable 
 summary(bank_data$campaign)
# # Let's see the percentile distribution of this variable
 boxplot(bank_data$campaign)
 quantile(bank_data$campaign,seq(0,1,0.01))
# Capping this at 99% which the value is 14
bank_data[which(bank_data$campaign>14),]$campaign <- 14
# Visualizing it with plot
ggplot(bank_data,aes(campaign))+geom_histogram(fill="steelblue")
#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type
bank_data$pdays<- as.factor(bank_data$pdays)
# Checking summary
 summary(bank_data$pdays)
 levels(bank_data$pdays)
# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"

# # Also,lets see the respose rate of each levels. 
 plot_response(bank_data$pday,"Pday")
# # Number of prospects under each category
 table(bank_data$pdays)
#-------------------------------------------------------
# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)
summary(bank_data$previous)
# Max=7, best is to convert this variable to factor
bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"

 summary(bank_data$previous)
 plot_response(bank_data$previous,"Previous_contacts")
# 
# # Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# # (categorical: 'failure','nonexistent','success')
 summary(bank_data$poutcome)
 plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")
#-------------------------------------------------------
#-- social and economic context attributes
############################################################
# # emp.var.rate- :employment variation rate - quarterly indicator (numeric)
 summary(bank_data$emp.var.rate)
# 
# # Histogram of employment variation rate variable
 ggplot(bank_data,aes(emp.var.rate))+geom_histogram(fill="steelblue")
# 
# # cons.price.idx:consumer price index - monthly indicator (numeric) 
 summary(bank_data$cons.price.idx)
# 
# # Histogram of consumer price index variable
 ggplot(bank_data,aes(cons.price.idx))+geom_histogram(fill="steelblue")
# 
# # cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
 summary(bank_data$cons.conf.idx)
# 
# # euribor3m: euribor 3 month rate - daily indicator (numeric)
 summary(bank_data$euribor3m)
# 
# # nr.employed: number of employees - quarterly indicator (numeric)
 summary(bank_data$nr.employed)
 #--------------------------------------------------------------------
#################################################################
#--------------------- Model Building#--------------------------
#################################################################
#---------------------------------------------------------  
# Required Packages

library(caret)
library(caTools)
library(dummies)

#creating dummy variables

bank_data$unique_ID<-c(1:nrow(bank_data))
dim(bank_data)# 41188    22
names(bank_data)
bank_data$cost_per_call_INR<-0.033*bank_data$duration+0.8

names(bank_data)
dim(bank_data) #41188    23

 bank_model1<-bank_data
 
 bank_model1$binning.age<-NULL

dim(bank_model1) #41188    22
bank_model1 <- dummy.data.frame(bank_model1)

bank_model1$response <- as.factor(ifelse(bank_model1$response == 1, "yes","no"))

dim(bank_model1)  # 41188    63
names(bank_model1)
#########################################    
# splitting into train and test data
#######################################
set.seed(1)
split_indices <- sample.split(bank_model1$response, SplitRatio = 0.70)
train <- bank_model1[split_indices, ]

train1<-train[,-c(45,62,63)]             #remove duration , uniqueID and cost_per_call_INR
dim(train1)
test <- bank_model1[!split_indices, ]
test1<-test[,-c(45,62,63)]                #remove duration , uniqueID and cost_per_call_INR
nrow(train1)/nrow(bank_model1)   #0.7000097
nrow(test1)/nrow(bank_model1)   #0.2999903  

#---------------------------------------------------------    

### Model 1: Logistic Regression
##########################################################


logistic_1 <- glm(response ~ ., family = "binomial", data = train1)

summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

 logistic_2 <- stepAIC(logistic_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain


logistic_2 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + jobtechnician + 
                    maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed + `previousMore than_3_times`, family = "binomial", data = train1)


data.frame(sort(vif(logistic_2)))
summary(logistic_2)

# jobadmin. ,maritaldivorced , educationPrimary_Education, educationTertiary_Education,`previousMore than_3_times`
#  1.413710     1.018266           1.411819                         1.321083                    1.078169
# remove jobadmin.
#---------------------------------------------------------    
# removing "previousLess_than_3_times"since vif is high and also the variable is not significant 
logistic_3 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed + `previousMore than_3_times`, family = "binomial", data = train1)

data.frame(sort(vif(logistic_3)))
summary(logistic_3)

# maritaldivorced   ,   `previousMore than_3_times` 
#   1.017974                  1.078180
# remove `previousMore than_3_times`
##################
logistic_4 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train1)

data.frame(sort(vif(logistic_4)))
summary(logistic_4)
# remove maritaldivorced
###################
logistic_5 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                     educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train1)

data.frame(sort(vif(logistic_5)))
summary(logistic_5)
# jobtechnician , educationPrimary_Education, educationTertiary_Education, monthoct 
#  1.099291           1.315569                           1.279827           1.453046
# remove monthoct
####################
logistic_6 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov  + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train1)

data.frame(sort(vif(logistic_6)))
summary(logistic_6)

# jobtechnician , educationPrimary_Education, educationTertiary_Education
#   1.098775            1.314747                     1.278990
# remove educationPrimary_Education
##################
logistic_7 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                     educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov  + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train1)


data.frame(sort(vif(logistic_7)))
summary(logistic_7)
#  remove jobtechnician 
#####################
logistic_8 <- glm(formula = response ~ jobretired + jobstudent + 
                    educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov  + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train1)

data.frame(sort(vif(logistic_8)))
summary(logistic_8)
# jobstudent    educationTertiary_Education     monthapr    monthjul  
# 1.053557           1.052405                    3.189813     3.334678
# remove monthjul
###########################
logistic_9 <- glm(formula = response ~ jobretired + jobstudent + 
                    educationTertiary_Education + 
                    contactcellular + monthapr +  monthjun + monthmar + 
                    monthmay + monthnov  + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train1)

data.frame(sort(vif(logistic_9)))
summary(logistic_9)
# remove day_of_weekfri
#########################
logistic_10 <- glm(formula = response ~ jobretired + jobstudent + 
                    educationTertiary_Education + 
                    contactcellular + monthapr +  monthjun + monthmar + 
                    monthmay + monthnov  +  day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train1)

data.frame(sort(vif(logistic_10)))
summary(logistic_10)
# jobstudent   educationTertiary_Education
#  1.052687       1.051041
# remove jobstudent
##############################
logistic_11 <- glm(formula = response ~ jobretired +  
                     educationTertiary_Education + 
                     contactcellular + monthapr +  monthjun + monthmar + 
                     monthmay + monthnov  +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                     nr.employed , family = "binomial", data = train1)

data.frame(sort(vif(logistic_11)))
summary(logistic_11)
#  remove educationTertiary_Education
##############################
logistic_12 <- glm(formula = response ~ jobretired +  
                      contactcellular + monthapr +  monthjun + monthmar + 
                     monthmay + monthnov  +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                     nr.employed , family = "binomial", data = train1)

data.frame(sort(vif(logistic_12)))
summary(logistic_12)
# jobretired  nr.employed 
#  1.047135     38.630138
# remove nr.employed
##########################
logistic_13 <- glm(formula = response ~ jobretired +  
                     contactcellular + monthapr +  monthjun + monthmar + 
                     monthmay + monthnov  +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx  
                      , family = "binomial", data = train1)

data.frame(sort(vif(logistic_13)))
summary(logistic_13)
# remove monthapr
#############################
logistic_14 <- glm(formula = response ~ jobretired +  
                     contactcellular +   monthjun + monthmar + 
                     monthmay + monthnov  +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx  
                   , family = "binomial", data = train1)

data.frame(sort(vif(logistic_14)))
summary(logistic_14)
# remove jobretired 
########################
logistic_15 <- glm(formula = response ~  contactcellular +   monthjun + monthmar + 
                     monthmay + monthnov  +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx  
                   , family = "binomial", data = train1)

data.frame(sort(vif(logistic_15)))
summary(logistic_15)
final_model1 <- logistic_15
#############################################################################################################   

###################
## Model1 Evaluation
#####################
# Predicting probabilities of responding for the test data
predictions_model1 <- predict(final_model1, newdata = test1[, -60], type = "response")#gives probabilities
# Let's use the probability cutoff of 50%.
predicted_response_model1 <- factor(ifelse(predictions_model1 >= 0.50, "yes", "no"))
# Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(predicted_response_model1, test1$response, positive = "yes")
conf  # Accuracy : 0.8982 Sensitivity : 0.22055 Specificity : 0.98422

#########################################
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response_model1 <- factor(ifelse(predictions_model1 >= cutoff, "yes", "no")) #requires in yes no
  conf <- confusionMatrix(predicted_response_model1, test1$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
} 

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
}  

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=1,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=1)
lines(s,OUT[,3],col=4,lwd=1)
box()
legend(0.26,.13,col=c(2,"darkgreen",4,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))


cutoff_model1 <- s[which(abs(OUT[,1]-OUT[,2])< 0.05540704)]   #0.06939394
predicted_response_optimal <- factor(ifelse(predictions_model1 >= 0.06939394, "yes", "no"))

conf_final_model1 <- confusionMatrix(predicted_response_optimal, test1$response, positive = "yes")


acc <- conf_final_model1$overall[1] 

sens <- conf_final_model1$byClass[1]

spec <- conf_final_model1$byClass[2]

acc  #0.6922143     
sens #0.7413793    
spec #0.6859723   

#############################################################
# MODEL 2 : logistic with binning of Age
#############################################################
#          #The model1 metrics accuracy,sensitivity and specificity are not close to each other .Let
#            us build another model by binning the age with breaks = c(16, 20, 25, 30,35,40,45,50,55,60,65,71) 

bank_model2<-bank_data
bank_model2$binning_age_new <- as.factor(cut(bank_model2$age, breaks = c(16, 20, 25, 30,35,40,45,50,55,60,65,71)))
bank_model2$age<-bank_model2$binning.age<-NULL

dim(bank_model2) #41188    22
bank_model2 <- dummy.data.frame(bank_model2)

bank_model2$response <- as.factor(ifelse(bank_model2$response == 1, "yes","no"))

dim(bank_model2)  # 41188   73



#########################################    
# Model 2---splitting into train and test data
#######################################
set.seed(1)
split_indices <- sample.split(bank_model2$response, SplitRatio = 0.70)
train <- bank_model2[split_indices, ]

train2<-train[,-c(44,61,62)]             #remove duration , uniqueID and cost_per_call_INR
dim(train2)
test <- bank_model2[!split_indices, ]
test2<-test[,-c(44,61,62)]                #remove duration , uniqueID and cost_per_call_INR
nrow(train2)/nrow(bank_model2)   #0.7000097
nrow(test2)/nrow(bank_model2)   #0.2999903 


logistic_1 <- glm(response ~ ., family = "binomial", data = train2)

summary(logistic_1)

###############################################################   
# Using stepwise algorithm for removing insignificant variables 
################################################################
logistic_2 <- stepAIC(logistic_1, direction = "both")


logistic_2 <-glm(formula=response ~ jobretired + jobstudent + jobtechnician + educationPrimary_Education + 
                   educationTertiary_Education + contactcellular + monthaug + 
                   monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                   cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
                   `binning_age_new(25,30]` + `binning_age_new(30,35]` + `binning_age_new(35,40]` + 
                   `binning_age_new(40,45]` + `binning_age_new(45,50]` + `binning_age_new(50,55]` + 
                   `binning_age_new(55,60]` + jobunknown, family = "binomial", data = train2)

data.frame(sort(vif(logistic_2)))

summary(logistic_2)

# jobstudent   jobunknown 

#  1.231227     1.013722

# remove jobstudent


logistic_3 <-glm(formula=response ~ jobretired +  jobtechnician + educationPrimary_Education + 
                   educationTertiary_Education + contactcellular + monthaug + 
                   monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                   cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
                   `binning_age_new(25,30]` + `binning_age_new(30,35]` + `binning_age_new(35,40]` + 
                   `binning_age_new(40,45]` + `binning_age_new(45,50]` + `binning_age_new(50,55]` + 
                   `binning_age_new(55,60]` + jobunknown, family = "binomial", data = train2)


summary(logistic_3)

data.frame(sort(vif(logistic_3)))

# educationTertiary_Education  jobunknown 

#  1.277679                      1.011625

# remove educationTertiary_Education

######################## 
logistic_4 <-glm(formula=response ~ jobretired +  jobtechnician + educationPrimary_Education + 
                   contactcellular + monthaug + 
                   monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                   cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
                   `binning_age_new(25,30]` + `binning_age_new(30,35]` + `binning_age_new(35,40]` + 
                   `binning_age_new(40,45]` + `binning_age_new(45,50]` + `binning_age_new(50,55]` + 
                   `binning_age_new(55,60]` + jobunknown, family = "binomial", data = train2)



summary(logistic_4)

data.frame(sort(vif(logistic_4)))

# jobtechnician            jobunknown 

#    1.067423                1.011034

# remove  jobtechnician
###################################
logistic_5 <-glm(formula=response ~ jobretired + educationPrimary_Education + 
                   contactcellular + monthaug + 
                   monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                   cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
                   `binning_age_new(25,30]` + `binning_age_new(30,35]` + `binning_age_new(35,40]` + 
                   `binning_age_new(40,45]` + `binning_age_new(45,50]` + `binning_age_new(50,55]` + 
                   `binning_age_new(55,60]` + jobunknown, family = "binomial", data = train2)


summary(logistic_5)

data.frame(sort(vif(logistic_5)))

# euribor3m   nr.employed  `binning_age_new(30,35]`   `binning_age_new(50,55]` `binning_age_new(55,60]` 

# 93.928472    77.695735      2.780988                        1.679694              1.484211

# remove euribor3m

###############################
logistic_6 <-glm(formula=response ~ jobretired + educationPrimary_Education + 
                   contactcellular + monthaug + 
                   monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                   cons.price.idx + cons.conf.idx +  nr.employed + 
                   `binning_age_new(25,30]` + `binning_age_new(30,35]` + `binning_age_new(35,40]` + 
                   `binning_age_new(40,45]` + `binning_age_new(45,50]` + `binning_age_new(50,55]` + 
                   `binning_age_new(55,60]` + jobunknown, family = "binomial", data = train2) 


summary(logistic_6)

data.frame(sort(vif(logistic_6)))

# remove jobunknown
##########################
logistic_7 <-glm(formula=response ~ jobretired + educationPrimary_Education + 
                   contactcellular + monthaug + 
                   monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                   cons.price.idx + cons.conf.idx +  nr.employed + 
                   `binning_age_new(25,30]` + `binning_age_new(30,35]` + `binning_age_new(35,40]` + 
                   `binning_age_new(40,45]` + `binning_age_new(45,50]` + `binning_age_new(50,55]` + 
                   `binning_age_new(55,60]` , family = "binomial", data = train2)  

summary(logistic_7)

data.frame(sort(vif(logistic_7)))

# `binning_age_new(30,35]`  `binning_age_new(50,55]`   `binning_age_new(55,60]`

#   2.773673                     1.677549                    1.483323

# remove `binning_age_new(30,35]`

###################################
logistic_8 <-glm(formula=response ~ jobretired + educationPrimary_Education + 
                   contactcellular + monthaug + 
                   monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                   cons.price.idx + cons.conf.idx +  nr.employed + 
                   `binning_age_new(25,30]` +  `binning_age_new(35,40]` + 
                   `binning_age_new(40,45]` + `binning_age_new(45,50]` + `binning_age_new(50,55]` + 
                   `binning_age_new(55,60]` , family = "binomial", data = train2)   


summary(logistic_8)

data.frame(sort(vif(logistic_8)))

# `binning_age_new(25,30]`   `binning_age_new(50,55]`   `binning_age_new(55,60]`

#   1.268092                         1.143134                    1.152648  

# remove  `binning_age_new(25,30]`

###############################
logistic_9 <-glm(formula=response ~ jobretired + educationPrimary_Education + 
                   contactcellular + monthaug + 
                   monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                   day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                   pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                   cons.price.idx + cons.conf.idx +  nr.employed + 
                   `binning_age_new(35,40]` + `binning_age_new(40,45]` + `binning_age_new(45,50]` + `binning_age_new(50,55]` + 
                   `binning_age_new(55,60]` , family = "binomial", data = train2)    


summary(logistic_9)

data.frame(sort(vif(logistic_9)))

# `binning_age_new(50,55]`    `binning_age_new(55,60]`

#   1.076978                         1.111833

# remove `binning_age_new(55,60]`

###########################
logistic_10 <-glm(formula=response ~ jobretired + educationPrimary_Education + 
                    contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed + 
                    `binning_age_new(35,40]` + `binning_age_new(40,45]` + `binning_age_new(45,50]` + `binning_age_new(50,55]` 
                  , family = "binomial", data = train2) 

summary(logistic_10)

data.frame(sort(vif(logistic_10)))

# remove `binning_age_new(50,55]`

##################################
logistic_11 <-glm(formula=response ~ jobretired + educationPrimary_Education + 
                    contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed + 
                    `binning_age_new(35,40]` + `binning_age_new(40,45]` + `binning_age_new(45,50]`  
                  , family = "binomial", data = train2)  

summary(logistic_11)

data.frame(sort(vif(logistic_11))) 

# remove `binning_age_new(40,45]`

#####################################
logistic_12 <-glm(formula=response ~ jobretired + educationPrimary_Education + 
                    contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed + 
                    `binning_age_new(35,40]` + `binning_age_new(45,50]`  
                  , family = "binomial", data = train2)   

summary(logistic_12)

data.frame(sort(vif(logistic_12))) 

# remove `binning_age_new(35,40]` 

##############################
logistic_13 <-glm(formula=response ~ jobretired + educationPrimary_Education + 
                    contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed + 
                    `binning_age_new(45,50]`  
                  , family = "binomial", data = train2)    

summary(logistic_13)

data.frame(sort(vif(logistic_13))) 

# remove  `binning_age_new(45,50]`  
##################################
logistic_14 <-glm(formula=response ~ jobretired + educationPrimary_Education + 
                    contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed , family = "binomial", data = train2)     

summary(logistic_14)

data.frame(sort(vif(logistic_14)))

# monthdec     day_of_weekfri

# 1.068461       1.063135

# remove monthdec
#############################

logistic_15 <-glm(formula=response ~ jobretired + educationPrimary_Education + 
                    contactcellular + monthaug + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed , family = "binomial", data = train2)      


summary(logistic_15)

data.frame(sort(vif(logistic_15))) 

# remove day_of_weekfri
#########################
logistic_16 <-glm(formula=response ~ jobretired + educationPrimary_Education + 
                    contactcellular + monthaug + monthjun + monthmar + monthmay + monthnov + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed , family = "binomial", data = train2)       

summary(logistic_16)

final_model2 <- logistic_16 
################################
#Model 2 evaluation and metrics
###############################
# Predicting probabilities of responding for the test data

predictions_logit <- predict(final_model2, newdata = test2[, -59], type = "response")
summary(predictions_logit) 

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test2$response, positive = "yes")

conf 

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  10802  1079
# yes   162   313
# 
# Accuracy : 0.8996          
# 95% CI : (0.8941, 0.9048)
# No Information Rate : 0.8873          
# P-Value [Acc > NIR] : 6.831e-06       
# 
# Kappa : 0.2949          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Sensitivity : 0.22486         
# Specificity : 0.98522         
# Pos Pred Value : 0.65895         
# Neg Pred Value : 0.90918         
# Prevalence : 0.11266         
# Detection Rate : 0.02533         
# Detection Prevalence : 0.03844         
# Balanced Accuracy : 0.60504         
# 
# 'Positive' Class : yes    


# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test2$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
} 

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
}  

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=1,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=1)
lines(s,OUT[,3],col=4,lwd=1)
box()
legend(0.26,.13,col=c(2,"darkgreen",4,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.034)]



# Let's choose a cutoff value of 0.073  for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.073, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test2$response, positive = "yes")

acc <- conf_final$overall[1] 

sens <- conf_final$byClass[1]


spec <- conf_final$byClass[2]

acc  #Accuracy =0.7253156    
sens #Sensitivity = 0.720546    
spec #Specificity  : 0.7259212  


                          #####################################################################
                          #Accuracy =0.7253156 Sensitivity = 0.720546 Specificity  : 0.7259212#                                                                                      #
                          ##################################################################### 

##########################################################
         #-----------KS-statstic------
##########################################################
#install.packages("ROCR")
library(ROCR)

#predicted probability of response on test
predictions_prob_test <- predict(final_model2, newdata = test2[, -59], type = "response")
#predicted yes-no response on test
test_cutoff_response <- factor(ifelse(predictions_prob_test >= 0.073, "yes", "no"))
#predicted one-zero response on test
test_cutoff_response1<- ifelse(test_cutoff_response=="yes",1,0)
#actual one-zero response in test
test_actual_response <- ifelse(test2$response =="yes",1,0)

pred_object <- prediction(test_cutoff_response1, test_actual_response)
performance_measures_test<- performance(pred_object, "tpr", "fpr")
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

#0.4464672
# Since the accuracy , sensitivity and specificity are not only high but also
# very close to each other , we shall take this model2 for furthur analysis.
# The values obtained also indicates that it is STABLE since there is no significant difference
# The ks_statistic is 45%. This reinforces that model 2 is more generilizable.



##(Another model with breaks 16,20,30,40,50,60,70,80 gave the following results and the metrics
#obtained were acc=0.7282292,sens=0.7140805,spec=0.7300255 ,ks=0.444106.As this model is not superior
#to model 2 , mentioned above , it was not considered)
###################################################################################################
#3.	Create a data frame with the variables prospect ID, actual response, predicted response, 
#predicted probability of response, duration of call in seconds, and cost of call
###################################################################################################
### creating data frame for TEST


df_test<-data.frame(unique_ID=test$unique_ID,actual_response=test_actual_response,test_cutoff_response1,
                    predictions_prob_test,duration=test$duration,cost_per_call_INR=test$cost_per_call_INR)

df_test<- df_test[order(-predictions_prob_test),]



#########################################################################
###################
#Lift-Gain Analysis 
###################

lift <- function(labels , predicted_prob,groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

response_decile_test <- data.frame(lift(test_actual_response, test_cutoff_response1, groups = 10))

View(response_decile_test)
#80% of the prospects are covered in the sixth decile
################################################

##############################################################################


###################################################
#4.	Find the number of top X% prospects you should target to meet the business objective
#o	Report the average call duration for targeting the top X% prospects to the CMO 
#(report this as a comment in the R file)
#############################
# From the gain_lift chart we see that we can target the top 80% of the prospects by taking the 5 th decile
  #Total number of prospects 1392
   #From the above GAIN chart, we can infer that by focussing on the top 50% of the prospects (after sorting them by probailities)
    # We can focus on top 80.81897% of the prospects who are likely to make deposit 
     #By targetting only 50% of 12356(nrow(test)) i.e, 7414 , we are able to get 80% of the  
     #1392(total prospects)1.e.,1114(number of prospects to be called to meet the business objective)

top_eighty_percent_prospects<-head(df_test,round(nrow(df_test)*0.50)) 
nrow(top_eighty_percent_prospects)# 7414

round(mean(top_eighty_percent_prospects$duration)) 

# The average duration of calls is 263 seconds for targeting top 80% of the prospects
#------------------------------------

round(mean(top_eighty_percent_prospects$cost_per_call_INR),2)#9.47

# If one targets all the persons in test data to achieve 80% acquisition then
#  the total cost will be more.Furthur,reduction in cost using the model2 is  42546.92
 cost_reduction<-sum(test$cost_per_call_INR)-sum(top_eighty_percent_prospects$cost_per_call_INR)

###########
#GAIN CHART
###########

ggplot(data = response_decile_test, aes(x=bucket,y=Gain))+ 
  geom_line(color="orange",size=0.6)+geom_point(color="red",shape= 8)+ 
  ggtitle("Gain Chart") + labs(y = "Gain%") + labs(x = "Decile") + scale_x_continuous(breaks = c(1,2,3, 4, 6 , 8 , 10)) +
  geom_hline(yintercept = 80.81897,linetype="dashed",color="blue",size=0.7) +
  geom_vline(xintercept = 6,linetype="dashed",color="blue",size=0.7) +
  box()+panel_border(colour = "green",size=1.5) 



##############################

#5.	Create a lift chart
#	The x-axis contains the number of prospects contacted; the y-axis contains the ratio:
#  response rate using the model/ response rate without using the model

# From the lift-chart we see that the lift corresponding to 6 th decile is 1.346983

###########
#LIFT CHART
###########

ggplot(data = response_decile_test, aes(x=bucket,y=Cumlift))+ 
  geom_line(color="green")+geom_point(color="red",shape= 8,size=2)+ 
  ggtitle("Lift Chart") + labs(y = "Lift") + labs(x = "Decile") + scale_x_continuous(breaks = c(seq(1:10)))+
  geom_hline(yintercept = 1.346983,linetype="dashed",color="blue",size=0.7) +
  geom_vline(xintercept = 6,linetype="dashed",color="blue",size=0.7) +
  box()+panel_border(colour = "blue",size=1.5,linetype =1 ) 

#Our original response rate is about 11.7% (called the baseline);  at the 6th decile on the x-axis, we get
#a lift of 1.346983 . It means that we can get 1.346983 x 11.7% the response rate by just targeting 60% prospects
#using the model2
################################################################################################
#                                         END                                                  #
#################################################################################################
 
 
 
 
 
