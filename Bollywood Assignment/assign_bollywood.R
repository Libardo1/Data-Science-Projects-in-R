             ############## ASSSIGNMENT___ BOLLYWOOD ###############
#	Import the Bollywood data set in Rstudio in a variable named bollywood
getwd()             # get and then set working directory,using setwd()                        
bollywood <- read.csv("bollywood.csv")
View(bollywood)



#	When the data setis imported, R stores character vectors as factors (by default).
# This can be  checked by str() command
  str(bollywood)               

# To change the attribute 'Movie' from factor to character type .
  bollywood$Movie <- as.character(bollywood$Movie)
  str(bollywood)            #Again check srructure to confirm.        
  
 #Q1.
#	  Access the last 10 movies from the bottom of the Bollywood data frame.

   last_10<-bollywood$Movie[(nrow(bollywood)-9):nrow(bollywood)]     
   
	 last_10                  #Store the output
	 
	  
#Q2.
  na_bollywood <- sum(is.na(bollywood))    #Total number of missing values (NA) in the bollywood data frame.
	
	na_bollywood                             # Store the result
	
#Q3
#	The movie  that tops the list in terms of Total Collections
  
  top_movie<-bollywood[(which.max((bollywood$Tcollection))),"Movie"]
  top_movie                                # Store the result
  
#Q4
#	The movie which comes second on the list in terms of Total Collections

  top_2_movie<-bollywood[order(-bollywood$Tcollection),"Movie"][2]
  top_2_movie                             # Store the movie name in variable named top_2_movie
  
# The movies shot by Shahrukh, Akshay and Amitabh separately,using subset()command

	shahrukh <- subset(bollywood, Lead == "Shahrukh")
	akshay <- subset(bollywood, Lead == "Akshay")
	amitabh <- subset(bollywood, Lead  == "Amitabh")
	
#  view  the above data frames 
	View(shahrukh)
	View(akshay)
	View(amitabh)
		   
#Q5
#	 The total collection(T_collection) of Shahrukh, Akshay and Amitabh movies individually
   
	
	sum(shahrukh$Tcollection)
	sum(akshay$Tcollection)
	sum(amitabh$Tcollection)
	
#Q6  
# Number of movies that are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.
  
 c(Flop=nrow(subset(bollywood,Verdict=="Flop")),Average=nrow(subset(bollywood,Verdict=="Average")),
   Hit=nrow(subset(bollywood,Verdict=="Hit")), Super_Hit = nrow(subset(bollywood,Verdict=="Super Hit")))

# To find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply
# (Avoid using the for loop)  

  sapply(list( bollywood$Ocollection,bollywood$Wcollection,
               bollywood$Fwcollection,bollywood$Tcollection ),max, na.rm=TRUE )
  
#Q7 
#  The names of the movies which have the maximum Ocollection, Wcollection, Fwcollecion & Tcollection

  movie_result <-bollywood[sapply(c(which.max(bollywood$Ocollection),which.max(bollywood$Wcollection),
                                    which.max(bollywood$Fwcollection), which.max(bollywood$Tcollection)),
                                  "["),"Movie" ]
  
  movie_result                # Store the names of 4 movies in same sequence in movie_result vector

 ##########################################################################################################
  
  
  
   
   
  
  
  
  
  
  
   
    