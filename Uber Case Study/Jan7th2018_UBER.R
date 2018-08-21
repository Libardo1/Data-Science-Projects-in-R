getwd()
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
uber<-read.csv("Uber Request Data.csv",stringsAsFactors = F)
View(uber)
#uber has 6745 observations of 8 variables
# Request_date_time and Drop_date_time are character vectors 
#---------------------------------------------------------------------------------------
#Data preparation and cleaning#
#----------------------------------------------------------------------------------------
# Show the repeat entries if any
uber[duplicated(uber),] # there are no duplicate rows
# Show unique repeat entries (row names may differ, but values are the same)
unique(uber[duplicated(uber),]) # no duplicate rows
sum(is.na(uber$Driver.id)) # Number of NA's is 2650
#NA's can be kept as it is ,as they do not affect the calculations to be done

#Request.timestamp andDrop.timestamp are to be formatted into POSIXct
Request_date_time<-parse_date_time(uber$Request.timestamp, c("dmy HM","dmy HMS"))
Drop_date_time<-parse_date_time(uber$Drop.timestamp, c("dmy HM","dmy HMS"))
uber<-uber%>%select(-Request.timestamp,-Drop.timestamp)%>%
  mutate(Request_date_time,Drop_date_time)

#------------------------------------------------------------------------------------
# Derived metrics 

Request_df1<-data.frame(uber$Request_date_time)
#create times_slots
Request_df1$times_slots <- cut(as.POSIXlt(Request_df1$uber.Request_date_time)$hour, 
                               breaks=c(0,6,12,18,24), 
                               labels=c("Earlmorn_0to6HRS","morn_6to12HRS","aftrn_12to18HRS","latEvn_18to24HRS"),
                               include.lowest=T)
colnames(Request_df1)[1]<-"Request_date_time"
uber_time_slots<-Request_df1%>%
  mutate(Driver.id=uber$Driver.id,Pickup_point=uber$Pickup.point,
         Status=uber$Status,Drop_date_time=uber$Drop_date_time)
View(uber_time_slots)

#--------------------------------------------------------------------------------------------
                                 #UNIVARIATE#
#---------------------------------------------------------------------------------------------
#1.Pickup_point
uber_time_slots%>%select(Pickup_point)%>%
       group_by(Pickup_point)%>%summarise (Total=n())

#2.number of Airport records is 3238 and  City is 3507
uber_time_slots%>%filter(times_slots=="Earlmorn_0to6HRS")%>%
       select(times_slots)%>%summarise(n=n()) #number of rows of is "Earlmorn_0to6HRS"is1421

#3.
uber_time_slots%>%filter(times_slots=="morn_6to12HRS")%>%
       select(times_slots)%>%summarise(n=n()) #number of rows of is "morn_6to12HRS"is1858

#4.
uber_time_slots%>%filter(times_slots=="aftrn_12to18HRS")%>%
       select(times_slots)%>%summarise(n=n()) #number of rows of is "aftrn_12to18HRS"is 1554

#5.
uber_time_slots%>%filter(times_slots=="latEvn_18to24HRS")%>%
       select(times_slots)%>%summarise(n=n()) #number of rows of is"latEvn_18to24HRS" 1912
#--------------------------------------------------------------------------------------------
                                 #BIVARIATE ANALYSIS#
#-------------------------------------------------------------------------------------------
#1."Airport" & "Status"
uber_time_slots%>%filter(Pickup_point=="Airport")%>%
       group_by(Status)%>%summarise (Total=n())%>%mutate(percent=Total*100/sum(Total))

# ("Trips completed","No Cars Available","Cancelled") = (1327,1713,198)
# The supply demand gap nearly 59% ,  critical area = "No Cars Available"

#2."City" & "Status"
uber_time_slots%>%filter(Pickup_point=="City")%>%
       group_by(Status)%>%summarise (Total=n())%>%mutate(percent=Total*100/sum(Total))

# ("Trips completed","No Cars Available","Cancelled") = (1504,937,1066)
# The supply demand gap nearly 43% ,  critical area = "both due to No Cars Available & Cancelled(nearly equal)"
#---------------------------------------------------------------------------------------
#

#------------------------------------------------------------------------------------
                                #Time slots with Status#
#------------------------------------------------------------------------------------
#3.time_slots "Earlmorn_0to6HRS"& "Status
uber_time_slots%>%filter(times_slots=="Earlmorn_0to6HRS")%>%
       group_by(Status)%>%summarise (Total=n())%>%mutate(percent=Total*100/sum(Total))

# ("Trips completed","No Cars Available","Cancelled") = (566,469,386)
# The supply demand gap nearly 60% ,  critical area = "No Cars Available"


#4.time_slots "morn_6to12HRS"& "Status

uber_time_slots%>%filter(times_slots=="morn_6to12HRS")%>%
       group_by(Status)%>%summarise (Total=n())%>%mutate(percent=Total*100/sum(Total))

# ("Trips completed","No Cars Available","Cancelled") = (854,386,618)
# The supply demand gap nearly 56% ,  critical area = "Cancelled"

#5.time_slots "aftrn_12to18HRS"& "Status

uber_time_slots%>%filter(times_slots=="aftrn_12to18HRS")%>%
       group_by(Status)%>%summarise (Total=n())%>%mutate(percent=Total*100/sum(Total))

# ("Trips completed","No Cars Available","Cancelled") = (685,738,131)
# The supply demand gap nearly 56% ,  critical area = "No Cars Available"


#6.time_slots "latEvn_18to24HRS"& "Status

uber_time_slots%>%filter(times_slots=="latEvn_18to24HRS")%>%
       group_by(Status)%>%summarise (Total=n())%>%mutate(percent=Total*100/sum(Total))

# ("Trips completed","No Cars Available","Cancelled") = (726,1057,129)
# The supply demand gap nearly 62% ,  critical area = "No Cars Available"
#-------------------------------------------------------------------------------------------
                             #Airport and time_slots & Status#
#--------------------------------------------------------------------------------------------
#7.
uber_time_slots  %>% 
       filter(times_slots=="Earlmorn_0to6HRS"&Pickup_point=="Airport")%>% 
       group_by(Status)%>%
       summarise(Total=n())%>%mutate(percent=Total*100/sum(Total)) 
# ("Trips completed","No Cars Available","Cancelled") = (269,155,10)
# The supply demand gap nearly 38% ,  critical area = "No Cars Available"

#8
uber_time_slots  %>% 
       filter(times_slots=="morn_6to12HRS"&Pickup_point=="Airport")%>% 
       group_by(Status)%>%
       summarise(Total=n())%>%mutate(percent=Total*100/sum(Total)) 
# ("Trips completed","No Cars Available","Cancelled") = (381,51,39)
# The supply demand gap nearly 19% ,  critical area = "No Cars Available"

#9
uber_time_slots  %>% 
       filter(times_slots=="aftrn_12to18HRS"&Pickup_point=="Airport")%>% 
       group_by(Status)%>%
       summarise(Total=n())%>%mutate(percent=Total*100/sum(Total)) 
# ("Trips completed","No Cars Available","Cancelled") = (317,574,74)
# The supply demand gap nearly 67% ,  critical area = "No Cars Available"

#10
uber_time_slots  %>% 
       filter(times_slots=="latEvn_18to24HRS"&Pickup_point=="Airport")%>% 
       group_by(Status)%>%
       summarise(Total=n())%>%mutate(percent=Total*100/sum(Total))

# ("Trips completed","No Cars Available","Cancelled") = (360,933,75)
# The supply demand gap nearly 74% ,  critical area = "No Cars Available"


                         #"Airport_Total_supply_demandGap"

uber_time_slots %>%
       filter(Pickup_point=="Airport")%>%
       group_by(Status) %>%
       summarise(Total = n())%>%mutate(percent=round(Total*100/sum(Total)))
# ("Trips completed","No Cars Available","Cancelled") = (1327,1713,198)
# The supply demand gap is nearly 59% ,  critical area =" No Cars Available"

#-------------------------------------------------------------------------------------------
#                           City and time_slots & Status
#-------------------------------------------------------------------------------------------
#11

uber_time_slots  %>% 
       filter(times_slots=="Earlmorn_0to6HRS"&Pickup_point=="City")%>% 
       group_by(Status)%>%
       summarise(Total=n())%>%mutate(percent=Total*100/sum(Total)) 
# ("Trips completed","No Cars Available","Cancelled") = (297,314,376)
# The supply demand gap is nearly 70%  ,  critical area ="Cancelled" 

#12

uber_time_slots  %>% 
       filter(times_slots=="morn_6to12HRS"&Pickup_point=="City")%>% 
       group_by(Status)%>%
       summarise(Total=n())%>%mutate(percent=Total*100/sum(Total)) 
# ("Trips completed","No Cars Available","Cancelled") = (297,335,579)
# The supply demand gap is nearly 66 % ,  critical area = "Cancelled"

#13

uber_time_slots  %>% 
       filter(times_slots=="aftrn_12to18HRS"&Pickup_point=="City")%>% 
       group_by(Status)%>%
       summarise(Total=n())%>%mutate(percent=Total*100/sum(Total)) 
# ("Trips completed","No Cars Available","Cancelled") = (368,164,57)
# The supply demand gap is nearly 38 % ,  critical area = "No Cars Available"

#14

uber_time_slots  %>% 
       filter(times_slots=="latEvn_18to24HRS"&Pickup_point=="City")%>% 
       group_by(Status)%>%
       summarise(Total=n())%>%mutate(percent=Total*100/sum(Total)) 

# ("Trips completed","No Cars Available","Cancelled") = (366,124,54)
# The supply demand gap is nearly 33 % ,  critical area = "No Cars Available"

                           #"City_Total_supply_demandGap"

uber_time_slots %>%
       filter(Pickup_point=="City")%>%
       group_by(Status) %>%
       summarise(Total = n())%>%mutate(percent=round(Total*100/sum(Total)))

# ("Trips completed","No Cars Available","Cancelled") = (1504,937,1066)
# The supply demand gap is nearly 57% ,  critical area =" Cancelled"

#-----------------------------------------------------------------------------------------
                                #VISUAL ANALYSIS-ggplots#
#-----------------------------------------------------------------------------------------
#Question1(a) most pressing problem at Airport 
plot_data1<- uber_time_slots %>%
  select(Pickup_point,Status,times_slots)%>%
  filter (Pickup_point=="Airport", Status=="Cancelled"|Status=="No Cars Available")%>%
  group_by(Status,times_slots)%>%
  summarise(Total=n())%>%mutate(percent=Total*100/sum(Total))

ggplot(plot_data1,aes(x=times_slots,y=Total,fill=Status))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(
    aes(label = Total, group = Status),
    position = position_dodge(0.8),
    vjust = -0.3, size = 3.5
  )  +
  ggtitle("most-pressing-problem-Airport")

#ANSWER: Most pressing problem at Airport is "No cars available" between latEvn_18to24HRS

#-----------------------------------------------------------------------------
                   # Description of the layers of ggplot of 1(a)#
#fill=status is used to show "Trip completed", "No cars available", "Cancelled"in
#different colors.
# The grouped bar plot is chosen since times_slots is a categorical variable
# In the aes:Total gives the count,fill=Status gives different colours for
#"cancelled" and "No Cars Available"
#dodge is used so that it DOES NOT PRODUCE stack bar 

#------------------------------------------------------------------------------------- 
#Question1(b) most pressing problem at City


critical_area_city<-uber_time_slots %>%
  select(Pickup_point,Status,times_slots)%>%
  filter (Pickup_point=="City", Status=="Cancelled"|Status=="No Cars Available")%>%
  group_by(Status,times_slots)%>%
  summarise(Total=n())%>%mutate(percent=Total*100/sum(Total))


ggplot(critical_area_city,aes(x=times_slots,y=Total,fill=Status))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(
    aes(label = Total, group = Status),
    position = position_dodge(0.8),
    vjust = -0.3, size = 3.5
  ) +
  ggtitle("most-pressing-problem-City")

#ANSWER: Most pressing problem at City is "Cancelled" between morn_6to12HRS
#-----------------------------------------------------------------------------
                  # Description of the layers of ggplot of 1(b)#
#fill=status is used to show "Trip completed", "No cars available", "Cancelled"in
#different colors.
# The grouped bar plot is chosen since times_slots is a categorical variable
# In the aes:Total gives the count,fill=Status gives different colours for
#"cancelled" and "No Cars Available"
#dodge is used so that it DOES NOT PRODUCE stack bar 

#------------------------------------------------------------------------------
#_______________________________________________________________________________ 
#Question2 Find out the gap between supply and demand and show using plots.
# 2(a)Find the time slots when the highest gap exists

supply_demand_gap<- uber_time_slots%>%
  group_by(times_slots,Status)%>%summarise (Total=n())%>%
  mutate(percent=round(Total*100/sum(Total)))


ggplot(supply_demand_gap,aes(x=times_slots,y=percent,fill=Status))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(
    aes(label = percent, group = Status),
    position = position_dodge(0.8),
    vjust = -0.3, size = 3.5
  ) +
  ggtitle("supply-demand-gap")

#Answer{
#The supply demand gap is the highest when percent of "Trip completed" is the least.
#In the plot the height of the "trip completed"in each of the times_slot examined gives the 
#least in "latEvn18to-24HRS".}

#-----------------------------------------------------------------------------
                     # Description of the layers of ggplot of 2(a)#
#fill=status is used to show "Trip completed", "No cars available", "Cancelled"in
#different colors.
# The grouped bar plot is chosen since times_slots is a categorical variable
# In the aes:Since supply-demand gap is needed,percent is chosen for y axis;
#fill=Status gives different colours for"cancelled" and "No Cars Available" and "No cars Available"
#dodge is used so that it DOES NOT PRODUCE stack bar.

#------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
#Question2 Find out the gap between supply and demand and show using plots.
#2(b)Find the type of requets for which the gap is most severe in the identified time
#            slots.

Pickup_Point_gap<-uber_time_slots %>% filter(times_slots=="latEvn_18to24HRS")%>%
  select(Pickup_point,Status)%>%group_by(Pickup_point,Status)%>%summarise(Total=n())%>%
  mutate(percent=round(Total*100/sum(Total)))



ggplot(Pickup_Point_gap,aes(x=Pickup_point,y=percent,fill=Status))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(
    aes(label = percent, group = Status),
    position = position_dodge(0.8),
    vjust = -0.3, size = 3.5
  ) + ggtitle("latEvn_18to24HRS-supply-demand-gap") 

#Answer{
#The supply demand gap is the highest when percent of "Trip completed" is the least.
#In the plot the height of the "Trip complted"in "Airport" and "City" examined gives the 
#least at "Airport" in "latEvn18to-24HRS".}

#-----------------------------------------------------------------------------
# Description of the layers of ggplot of 2(b)#
#fill=status is used to show "Trip completed", "No cars available", "Cancelled"in
#different colors.
# The grouped bar plot is chosen since times_slots is a categorical variable
# In the aes:Since supply-demand gap is needed,percent is chosen for y axis;
#fill=Status gives different colours for"cancelled" and "No Cars Available" and "No cars Available"
#dodge is used so that it DOES NOT PRODUCE stack bar.

#------------------------------------------------------------------------------
                            #Reasons for the supply-demand gap#
#------------------------------------------------------------------------------
#Airport:

#In time slots 0-6HRS,6-12HRS,12-18HRS,18-24HRS, the supply demand gap are 
# respectively 60%,56%,56%,62%.The critical issue in each of the slots is
# "No Cars Available".Further the most pressing problem occurs during 18-24HRS.

# HYPOTHESIS OF THE PROBLEMS:
# 1.Large amount of down time for the drivers at the airport.
# 2.Insuffcient number of drivers/cars operating leading to no cars available.
# 3.Most drivers don't schedule themselves.

# City:


#In time slots 0-6HRS,6-12HRS,12-18HRS,18-24HRS, the supply demand gap are
#respectively 70%,66%,38%,33%.The critical issues in each of the slots are
#"Cancelled","Cancelled","No Cars Available","No Cars Available".
#The most pressing problem occurs during 0-6HRS slot.

#HYPOTHESIS OF THE PROBLEMS:
#1.Cancellations may be because of large downtime resulting at the airport and hence
#  the drivers are reluctant to provide service
#2.Insuffcient number of drivers/cars operating leading to no cars available.
#3.Most drivers don't schedule themselves.

#------------------------------------------------------------------------------
                        #Ways to resolve the supply-demand gap#
#------------------------------------------------------------------------------
#Uber is the top most company among taxi service providers and will see a lot
# of competition from other's like OLA and others.
#The companie's objective will be to stay at the top of their game and take steps 
# that will lead to higher growth and profitability in long run. To acheive this here
# are some solutions:

#1.The company should recruit more number of drivers to beat the "No Cars
# Available" issue.
#2.The company should give extra incentives to the drivers to work in Time
# slots where the problem is most pressing.
#3.The company should think of providing the different segment of cars like
# "Mini","Micro","luxury" etc., to beat the competitors and beneficial to the
# consumers.
#4.The company also can think of suitable pricing in a limited manner especially
# in the slots where the problem is most pressing.(The most pressing issue at the
# Airport is "No Cars Available" between latEvn_18to24HRS.The most pressing issue
# at the City is"Cancelled" between morn_6to12HRS).
#5.Prior booking may be allowed by the consumers so that the drivers commit to the
# trip.

#--------------------------------------------------------------------------------




