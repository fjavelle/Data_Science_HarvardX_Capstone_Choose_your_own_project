title: "Hotel Recommendation System"
author: "Florian Javelle"
date: "18/05/2021"


#######################################################################################################
#                                        INTRODUCTION                                                 #
#######################################################################################################

# AIM: This report aims to create a recommendation system for hotel selection based on data scraped from 
# Booking.com. 
# The secondary aim of this report is to compare the accuracy of a regression-based versus PCA recommendation 
# system. 

# Predicted variable: the review score given by the reviewer after his/her stay at the hotel. 
# I will first explore the data set, reshape it if necessary, and then determine potential predictors to 
# include in my recommendation system. Then, I will construct a recommendation system for hotel selection.
# I will continue by comparing the accuracy of a regular (regression-based) recommendation system and one 
# based on PCA. I will finish by discussing the differences between the two systems. 

#################################################################
#                    Workspace preparation                      #
#################################################################

### To clear workspace
rm(list = ls())
### To clear console
cat("\014")

### To install and charge the libraries  
if(!require(colorspace)) install.packages("colorspace", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(colorspace)
library(ggplot2)
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)

### To download the data from Dropbox
temp <- tempfile()
download.file("https://www.dropbox.com/s/lkxeeb7kp0ryma8/Hotel_Reviews.csv?dl=1",temp)
Data<-read.csv(temp, header=TRUE, sep=";")
unlink(temp)

### To select the useful variables 
Data<-Data%>%
  select(Hotel_Address, Additional_Number_of_Scoring, Review_Date, Average_Score, 
         Hotel_Name, Reviewer_Nationality, Review_Total_Negative_Word_Counts, 
         Total_Number_of_Reviews, Review_Total_Positive_Word_Counts, 
         Total_Number_of_Reviews_Reviewer_Has_Given, Reviewer_Score, days_since_review,
         lat, lng)

### To split the data set into Hotel and validation sets 
set.seed(1, sample.kind="Rounding")  
test_index <- createDataPartition(y = Data$Reviewer_Score, times = 1, p = 0.1, list = FALSE)
Hotel <- Data[-test_index,]
temp <- Data[test_index,]

### To be sure that all Hotels, reviewer nationalities, and total number of reviews per reviewer
### are in the "validation" and "Hotel" sets. Total_Number_of_Reviews_Reviewer_Has_Given and 
### "Reviewer_Nationality" were added after the first analysis. 
### We will see later that it is important to have all the levels of these 2 variables in both sets.
validation <- temp %>% 
  semi_join(Hotel, by = "Hotel_Name")%>% 
  semi_join(Hotel, by = "Total_Number_of_Reviews_Reviewer_Has_Given")%>%
  semi_join(Hotel, by = "Reviewer_Nationality")


# To split the Hotel set into training and testing sets 
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = Hotel$Reviewer_Score, times = 1, p = 0.1, list = FALSE)
train_set <- Data[-test_index,]
test_set <- Data[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "Hotel_Name")%>% 
  semi_join(train_set, by = "Total_Number_of_Reviews_Reviewer_Has_Given")%>%
  semi_join(train_set, by = "Reviewer_Nationality")

#################################################################
#                    Data set description                       #
#################################################################

### To see how train_set is constructed 
glimpse(train_set)

### To see the dimensions of train_set
dim(train_set)

### To see the mean, median, max and min values of Reviewer_Score 
summary(train_set$Reviewer_Score)

### To see the mean, median, max and min values of Average_Score
summary(train_set$Average_Score)

### To see how many different hotels there are in the data set
n_distinct(train_set$Hotel_Name)

### To see how many different reviewer nationalities there are in the data set
n_distinct(train_set$Reviewer_Nationality)


#___ Important points____
# => The data set has been pre-analyzed==> "Total_Number_of_Reviews", "Total_Number_of_reviews_Reviewer_has_given"
# => No User ID  
# => The information provided by the variables "Review_Date" and "days_since_review" overlap.
# => Same thing for "Hotel_Address" and "lat"/"lng"
# => Many nationalities (227)==> maybe range per continent, but keep the original for the recommendation system
# => The date does not have a usable format 

#######################################################################################################
#                                            METHODS                                                  #
#######################################################################################################
# In this section, I will adapt the parts of the data set that are currently not usable. 
# Then, I will explore the data set and consider potential useful predictors. 

#################################################################
#                    Data set transformation                    #
#################################################################

### To change the date format
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate)

train_set$Review_Date_correct<-mdy(train_set$Review_Date)
train_set$Review_Date_correct<-as_date(train_set$Review_Date_correct)

### To create a new variable entitled "Reviewer_continent" based on "Reviewer_Nationality"
if(!require(countrycode)) install.packages("countrycode", repos = "http://cran.us.r-project.org")
library(countrycode)

train_set$Reviewer_continent <- countrycode(sourcevar = train_set[, "Reviewer_Nationality"],
                            origin = "country.name",
                            destination = "continent")

### Some country abbreviations are not recognized. Thus, I am classifying them manually.  
train_set$Reviewer_continent <- as.character(train_set$Reviewer_continent)
train_set$Reviewer_continent[train_set$Reviewer_Nationality==
                               " Central Africa Republic "]<-"Africa"
train_set$Reviewer_continent[train_set$Reviewer_Nationality==" Antarctica "]<-"Antarctica"
train_set$Reviewer_continent[train_set$Reviewer_Nationality==" Cocos K I "]<-"Oceania"
train_set$Reviewer_continent[train_set$Reviewer_Nationality==" Cura ao "]<-"Americas"
train_set$Reviewer_continent[train_set$Reviewer_Nationality==" Crimea "]<-"Europe"
train_set$Reviewer_continent[train_set$Reviewer_Nationality==" Kosovo "]<-"Europe"
train_set$Reviewer_continent[train_set$Reviewer_Nationality==" Saint Barts "]<-"Americas"
train_set$Reviewer_continent[train_set$Reviewer_Nationality==" Saint Martin "]<-"Americas"
train_set$Reviewer_continent[train_set$Reviewer_Nationality==
                               " United States Minor Outlying Islands "]<-"Americas"

### To extract the city of each hotel based on "Hotel_Address"
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
library(plyr)
library(maps)

### To split the variable Hotel_Address in different sections based on the space between words
train_set$Hotel_Address<-as.character(train_set$Hotel_Address)
addresses<-strsplit(train_set$Hotel_Address, " ")

### Match on cities in "world.cities"
train_set$Hotel_city<- llply(addresses, function(x)x[max(which(x %in% world.cities$name))])
train_set$Hotel_city<-as.character(train_set$Hotel_city)
n_distinct(train_set$Hotel_city)
length(train_set$Hotel_city)

# The packages plyr and dplyr do not work very well when charged together. They overlap a lot. 
# To avoid issues, I discharge plyr after use. 
detach("package:plyr", unload=TRUE)

#################################################################
#                    Data exploration                           #
#################################################################
### To see the "Reviewer_Score" distribution
train_set %>%
  group_by(Reviewer_Score)%>%
  ggplot(aes(Reviewer_Score)) +
  geom_histogram(bins = 20, fill="grey", colour="black")+ 
  labs(x="Reviewer score", y="Number of reviews")+  
  ggtitle("Number of reviews per grading score")


### To see the average "Reviewer_Score" distribution
train_set %>%
  group_by(Hotel_Name)%>% 
  summarize (avg_rating=mean(Reviewer_Score), n=n())%>%
  ggplot(aes(avg_rating, n))+ geom_point() + labs(x="Average reviewer score",
                                                  y="Total number of reviews (manual)")+  
  ggtitle("Total number of reviews per average reviewer score")


# The graphic shows that the best and the worst reviewer average scores have only a few ratings 
# ==> regularization?  

# Considering that there is a variable reporting the total number of reviews (i.e., Total_Number_of_Reviews), 
# ==> I am going to verify if I obtain the same results. 


### To see the average "Reviewer_Score" distribution (using the pre-processed variable "Total_Number_of_Reviews")
train_set %>% group_by(Hotel_Name)%>% 
  summarize (avg_rating=mean(Reviewer_Score), avg_n=mean(Total_Number_of_Reviews))%>%
  ggplot(aes(avg_rating, avg_n))+ geom_point()+ labs(x="Average reviewer score",
                                                  y="Total number of reviews (pre-processed)")+  
  ggtitle("Total number of reviews per average review score")


# Even though the graphics look very similar, they are not the same.
# ==> the range of "Total_number_of_Reviews" is three times bigger than when I've computed it manually. 

# By curiosity, I am going to check if it changes the top 10 of hotels the most rated.

### To see the most rated hotels (manual)
train_set %>% 
  group_by(Hotel_Name)%>% 
  summarize (n=n())%>% 
  top_n(10) %>%
  arrange(desc(n))

### To see the most rated hotels (pre-processed)
train_set %>% 
  group_by(Hotel_Name)%>% 
  summarize (Total_Number_of_Reviews=mean(Total_Number_of_Reviews))%>% 
  arrange(desc(Total_Number_of_Reviews))

# The two variables analysed (i.e., Total number of reviews manual or pre-existing) are different.
# => The pre-existing variable "Total_Number_of_reviews" reports between 3 and 4 times more reviews 
# per hotel than when I compute it manually. 
# => The top 10 also changes.   

# This might be the same thing for the other variable that I was thinking pre-processed (i.e., Average_score). 
# Thus, I am also going to print the top 10 of average reviewer scores for both cases 
# (i.e., manually computed vs pre-existing variable)  


### To see the best rated hotels (manual)
train_set %>% 
  group_by(Hotel_Name)%>% 
  summarize (avg_Reviewer_score=mean(Reviewer_Score))%>% 
  arrange(desc(avg_Reviewer_score))

### To see the best rated hotels (manual)
train_set %>% 
  group_by(Hotel_Name)%>% 
  summarize (avg_Reviewer_score=mean(Average_Score))%>% 
  arrange(desc(avg_Reviewer_score))

# => The tops 10 are relatively different.  

# I am now going to assess the distribution of the number of reviews givens per reviewer. 

### To see the distribution of "Total Number of Reviews a reviewers has given" 
train_set %>% 
  filter(!is.na(Total_Number_of_Reviews_Reviewer_Has_Given))%>%
  group_by(Total_Number_of_Reviews_Reviewer_Has_Given)%>%
  ggplot(aes(Total_Number_of_Reviews_Reviewer_Has_Given)) +
  geom_histogram(bins = 30, fill="grey", colour="black")+ 
  labs(x="Total number of reviews a reviewer has given", y="Number of reviewers")+
  ggtitle("Distribution of total number of reviews per reviewer") + 
  scale_x_continuous(trans = "log10")


# A very large portion of users that gave only one review. 
# => I am going to assess if it does influence the average reviewer score.


### To see the average reviewer score in function of the total number of review a reviewer has given
train_set %>% 
  group_by(Total_Number_of_Reviews_Reviewer_Has_Given)%>% 
  summarize (avg_rating=mean(Reviewer_Score),
             avg_n=mean(Total_Number_of_Reviews_Reviewer_Has_Given))%>%
  ggplot(aes(avg_rating, avg_n))+ geom_point()+ labs(x="Average reviewer score",
                                                     y="Total number of reviews a reviewer has given")+  
  ggtitle("Average reviewer score per total number of reviews a reviewer has given")


# When the total number of reviews a reviewer has given is low, the average score is consistent around 8.4. 
# When the total number of reviews a reviewer has given goes over 50, the average score becomes very 
# heterogeneous (from 5 to 10) with some reviewers being very grumpy and others being very positive.  
# ==> I am going to use the variable "Total_Number_of_Reviews_Reviewer_Has_Given" as a potential predictor.  

# I am now going to evaluate the distribution of the total number of reviews per hotel.

### To see the distribution of Total Number of Reviews per hotel
train_set %>%
  group_by(Hotel_Name)%>% 
  summarize(Total_Number_of_Reviews_avg=mean(Total_Number_of_Reviews))%>%
  ggplot(aes(Total_Number_of_Reviews_avg)) +
  geom_histogram(bins=25, fill="grey", colour="black")+ 
  labs(x="Total number of reviews per hotel", y="Number of hotels")+ 
  ggtitle("Distribution of total number of reviews per hotel")+ 
  scale_x_continuous(trans = "log10") 


# There are only a few hotels with a very small or very large number of reviews. 
# ==> Regularization might be important.

# Next, I am going to evaluate if the number of reviews per hotel influences the average score.

### To see the distribution of the average reviewer score per hotel
train_set %>%
  group_by(Hotel_Name)%>% 
  summarize(avg_rating=mean(Reviewer_Score))%>%
  ggplot(aes(avg_rating)) +
  geom_histogram(bins=25, fill="grey", colour="black")+ 
  labs(x="Average rating per hotel", y="Number of hotels")+ 
  ggtitle("Distribution of the average rating per hotel")

train_set %>% 
  group_by(Hotel_Name, Total_Number_of_Reviews)%>% 
  summarize (avg_rating=mean(Reviewer_Score), n=n()) %>%
  arrange(desc(avg_rating))

# The best and the worst hotels do not have many reviews. 
# => Implement regularization to the recommendation system. 

# Now, I give interest to the reviewing period. I am going to see if the average reviewer score changes 
# over the assessed reviewing period. 

### To see the average reviewer score in function of time 
train_set%>% 
  group_by(Review_Date_correct)%>% 
  summarize (avg_rating=mean(Reviewer_Score),Review_Date_correct)%>%
  ggplot(aes(Review_Date_correct, avg_rating))+geom_point()+
  stat_smooth()+ labs(x="Review date", y="Average reviewer score") + 
  ggtitle("Average hotel reviewer score between 2015 and 2017") 

# The average reviewer score is fluctuating across the year. 
# It seems that the average rating during the winter period is better than during the summer period.  

# I am going to do the same thing but using the number of reviews to see if some periods provided more 
# reviews than others did. 

### To see the number of reviews in function of time 
train_set%>% 
  group_by(Review_Date_correct)%>% 
  summarize (n=n(),Review_Date_correct)%>%
  ggplot(aes(Review_Date_correct, n))+geom_point()+
  stat_smooth()+ labs(x="Review date", y="Number of reviews per day") + 
  ggtitle("Number of review per day between 2015 and 2017")


# The average number of reviews over the two years analyzed is relatively stable with a small increase 
# at the end of the summer periods. 
# => The date of review may influence the accuracy of the recommendation system but its effect size is 
# likely to be small. 

# Now, I am going to evaluate if there is a relationship between the average reviewer score and the 
# number of words in the negative reviews.   

### To add the equation of the regression curve to the graph
if(!require(ggpmisc)) install.packages("ggpmisc", repos = "http://cran.us.r-project.org")
library(ggpmisc)
my_formula <- y ~ x

### To see the average reviewer score in function of the number of words in the negative review 
train_set %>%
  group_by(Hotel_Name)%>% 
  summarize (avg_rating=mean(Reviewer_Score),
             avg_Review_Total_Negative_Word_Counts=mean(Review_Total_Negative_Word_Counts))%>%
  ggplot(aes(avg_rating, avg_Review_Total_Negative_Word_Counts)) +
  geom_point()+stat_smooth()+labs(x="Average reviewer score per hotel", 
                                  y="Average number of words in the negative review")+ 
  ggtitle("Average number of words in the negative review in function of the average reviewer score") +
  stat_poly_eq(formula = my_formula, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               rr.digits = 2, parse = TRUE)

# There is a negative relationship with a large effect size between the average reviewer score and 
# the number of words in the negative reviews. In other words, the more words there are in 
# the negative review the worst will be the average score.     
# I am going to do the same thing but using the total number of positive words. 

### To see the average reviewer score in function of the number of words in the positive reviews 
train_set %>%
  group_by(Hotel_Name)%>% 
  summarize (avg_rating=mean(Reviewer_Score),
             avg_Review_Total_Positive_Word_Counts=mean(Review_Total_Positive_Word_Counts))%>%
  ggplot(aes(avg_rating, avg_Review_Total_Positive_Word_Counts)) + geom_point()+stat_smooth()+
  labs(x="Average reviewer score per hotel", y="Average number of words in the positive review")+ 
  ggtitle("Average number of words in the positive reviews 
          in function of the average reviewer score") + stat_poly_eq(formula = my_formula, 
                                                                     aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), rr.digits = 2,
                                                                     parse = TRUE)


# There is a large positive relationship between the two variables, also with a large effect size. 
# Nevertheless, the effect size is not as strong as for the negative reviews. 

# I am now going to classify the reviewers per nationality and see if some nationalities have reported 
# more reviews than others have. 
# I am also going to compare the average reviewer score per nationality. 

### To see a list presenting average reviewer score and number of reviews per reviewer nationality 
### (ranged based on average reviewer score)
train_set %>%
  group_by(Reviewer_Nationality)%>% 
  summarize (avg_rating=mean(Reviewer_Score), n=n())%>% 
  arrange(desc(avg_rating))

### To see a list presenting average reviewer score and number of reviews per reviewer nationality 
### (ranged based on number of reviews)
train_set %>% 
  group_by(Reviewer_Nationality)%>% 
  summarize (avg_rating=mean(Reviewer_Score), n=n())%>%
  arrange(desc(n))

# There are 227 different nationalities. The countries with the best reviewer scores also have just a 
# few reviews. 
# When considering the total number of reviews, United Kingdom is largely first, followed by the United
# States and Australia. 

# Just by curiosity, I am going to see if these differences can be summarized by grouping reviewers 
# per continent of origin. 

### To see a list presenting average reviewer score and number of reviews per reviewer continent 
### (ranged based on average reviewer score)
train_set %>%
  group_by(Reviewer_continent)%>% 
  summarize (avg_rating=mean(Reviewer_Score), n=n())%>%
  arrange(desc(avg_rating))

### To plot the average reviewer score per continent 
plot1<-train_set%>% 
  filter(Reviewer_continent!="") %>% 
  group_by(Reviewer_continent)%>% 
  summarize(count=n(), means=mean(Reviewer_Score), std=sd(Reviewer_Score)/sqrt(count)) %>%
  mutate(Reviewer_continent=reorder(Reviewer_continent, means))%>%
  ggplot(aes(x=Reviewer_continent, y=means, ymin=means-std, ymax=means+std ))+
  geom_point()+geom_errorbar() +
  labs(x="Reviewer continent", y="Average reviewer score")+ 
  ggtitle("Average reviewer score per continent")

plot1+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# 471 missing values. 
# Antarctica only has 3 reviews. 
# There is a clear difference in average reviewer scores between continents. 
# Even though I think that the variable "reviewer_nationality" will provide more information to the 
# recommendation system, it is interesting to see that differences between countries still exist when grouping 
# the reviewer per continent.   `

# Finally, I know from earlier analyses that the hotels are only coming from 6 distinct cities. 
# => I am going to evaluate if the city itself influences the average reviewer score. 

### To plot the average reviewer score per hotel location city
plot<-train_set%>% group_by(Hotel_city)%>% 
  summarize(count=n(), means=mean(Reviewer_Score), std=sd(Reviewer_Score)/sqrt(count)) %>%
  mutate(Hotel_city=reorder(Hotel_city, means))%>% ggplot(aes(x=Hotel_city, y=means, 
                                                              ymin=means-std, ymax=means+std ))+
  geom_point()+geom_errorbar()+ labs(x="Hotel location", y="Average reviewer score")+ 
  ggtitle("Average reviewer score per hotel location")

plot+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# There is a clear difference between cities. The hotels from Vienna and Barcelona have the best average 
# reviewer score, while the hotels from London and Milan have the worst.  

#################################################################
#      Machine-learning techniques used in this report          #
#################################################################

#____Root Mean Squared Error (RMSE)____  

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#__Regularization__  

##### Penalized least square  
# To choose the proper lambda for each model, I will use cross-validation. In other words, I will test several lambdas 
# and select the one giving the smallest RMSE.   

#____Predictors____  
# The predictors put in evidence in the data exploration step are the following:  
# - Number of reviews per hotel  
# - Number of reviews a reviewer has given   
# - Reviewer nationality  
# - City in which the hotel is located   
# - Review date   

# The variables "Review_Total_Negative_Word_Counts", and "Review_Total_Positive_Word_Counts" cannot be 
# included in the recommendation system because they are part of the reviewing process. Indeed, they 
# are realized by the customers after their stay in a hotel, during the rating process where the score 
# is emitted.  


#____Principal Component Analysis (PCA)____  
# Theoretically, a PCA based on Hotel name and user ID would provide the best outcome. But as the user ID 
# is not available, this is not possible. I could replace the user ID with another predictor to construct 
# the scoring matrix. Nonetheless, all the detected predictors have multiple reviewer scores for each 
# level forcing the creation of a matrix with three dimensions. Therefore, I am going to chose a simpler 
# option based on multiple predictors. Such PCA is, however, not suitable for categorical predictors 
# (i.e., not accurate and very long computation time) [6]. Thus, I am only going to use the numeric 
# predictors detected in the data exploration step (i.e., review date, the total number of reviews the 
# reviewer has given, and the total number of review per hotel). To perform such analysis, I am going 
# to apply the concept of PCA to regression. Additionally, I will use k_fold cross folding validation to 
# have more robust results. 

# Finally, to have an objective comparison between the accuracy of PCA and regression-based recommendation
# systems, I am going to do a second regression-based model only using the three numeric predictors. 
# I will, then, compare the RMSE of both techniques.  

#######################################################################################################
#                                            RESULTS                                                  #
#######################################################################################################

#################################################################
#                    Training the model                         #
################################################################# 

# To begin, I am going to create a naive model that I will use as a reference. 
# In this model, I assume that all hotels are rated the same and that the random error can explain all the variance in the results. 
# Thus, the only predictor in this model is the average reviewer score from the training set. 

### ___MODEL 1 (m1) - reference rmse (naive)
mean_RS<-mean(train_set$Reviewer_Score)
ref_rmse<-RMSE(test_set$Reviewer_Score, mean_RS)
print(ref_rmse)
### To save the results in a table
rmse_results <-data_frame(method="Just the average reviewer score", RMSE=ref_rmse)

# As displayed in the data exploration section, this naive model does not properly consider the total reviewer 
# score variance. Indeed, I have displayed potential predictors that could be used to give a better fit to my model.  
# As displayed in the data exploration section, some hotels were rated higher than others. 
# Therefore, I am going to add a bias term (i.e., effect in statistical textbooks [1]) for hotels (b_h) 
# to the recommendation system.

### ___MODEL 2 (m2) - average rating + hotel bias (b_h)
b_h<-train_set%>%
  group_by(Hotel_Name)%>%
  summarize(b_h=mean(Reviewer_Score - mean_RS))

predicted_scores<-mean_RS+test_set%>% 
  left_join(b_h, by="Hotel_Name")%>%
  .$b_h

rmse_2<-RMSE(test_set$Reviewer_Score, predicted_scores)
rmse_results<-data_frame(method=c("Just the average reviewer score", "m1 + Hotel bias"),
                         RMSE=c(ref_rmse, rmse_2))
format.data.frame(rmse_results, digits=6) 



# The hotels with the highest and the lowed average reviewer scores also had the lowest number of reviews.
# => I am going to use the regularization technique over the recommendation system. 
# ==> I am, first, going to create a function to determine the lambda term giving the lowest RMSE 
# and then apply it to the current model.  

### ___MODEL 3 (m3) - regularization ==> hotel bias (b_h)
lambdas <- seq(0, 20, 0.25)#Will be used in all the subsequent models 

rmse_lambda <- sapply(lambdas, function(l){
  mean_RS<-mean(train_set$Reviewer_Score)
  
  b_h <- train_set %>%
    group_by(Hotel_Name) %>%
    summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+l))
  
  predicted_scores <- test_set %>%
    left_join(b_h, by = "Hotel_Name") %>%
    mutate(pred = mean_RS + b_h) %>%
    pull(pred)
  return(RMSE(test_set$Reviewer_Score, predicted_scores))
})

### To plot rmse against lambda
qplot(lambdas, rmse_lambda)

# To determine what lambda corresponds to the smallest RMSE
lambda<-lambdas[which.min(rmse_lambda)]

### RMSE with the selected lambda 
b_h <- train_set %>%
  group_by(Hotel_Name) %>%
  summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+lambda))

predicted_scores <- test_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  mutate(pred = mean_RS + b_h) %>%
  .$pred

rmse_3 <- RMSE(test_set$Reviewer_Score, predicted_scores)
rmse_results <- data_frame(method = c("Just the average rating", "m1 + Hotel bias",
                                      "m2 + Regularization"),
                           RMSE = c(ref_rmse, rmse_2, rmse_3))

format.data.frame(rmse_results, digit=6)

# I am going to continue by adding the variable "Total_Number_of_Reviews_Reviewer_Has_Given". 

### ___MODEL 4 (m4) - regularization ==> hotel  + number of reviews per reviewer (b_r)
rmse_lambda <- sapply(lambdas, function(l){
  
  mean_RS<-mean(train_set$Reviewer_Score)
  
  b_h <- train_set %>%
    group_by(Hotel_Name) %>%
    summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+l))
  
  b_r <-train_set %>%
    left_join(b_h, by='Hotel_Name') %>%
    group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
    summarize(b_r = sum(Reviewer_Score - b_h - mean_RS)/(n()+l))
  
  predicted_scores <- test_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    mutate(pred = mean_RS + b_h + b_r) %>%
    .$pred
  return(RMSE(predicted_scores, test_set$Reviewer_Score))
})

### To determine what lambda corresponds to the smallest RMSE
lambda<-lambdas[which.min(rmse_lambda)]

### RMSE with the selected lambda 
b_h <- train_set %>%
  group_by(Hotel_Name) %>%
  summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+lambda))

b_r <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
  summarize(b_r = sum(Reviewer_Score - b_h - mean_RS)/(n()+lambda))

predicted_scores <- test_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  mutate(pred = mean_RS + b_h + b_r) %>%
  .$pred

rmse_4 <- as.numeric(RMSE(predicted_scores, test_set$Reviewer_Score))
rmse_results <- data_frame(method = c("Just the average reviewer score", "m1 + Hotel bias", 
                                      "m2+ Regularization", "m3 + Reviewer bias" ),
                           RMSE = c(ref_rmse, rmse_2, rmse_3, rmse_4))

format.data.frame(rmse_results, digit=6)


# I am going to add an error term for the date of the review. 
# As shown in the data exploration step, this predictor is likely to have only a small effect. 

### ___MODEL 5 - regularization ==> hotel  + number of reviews per reviewer + date of review (b_d)
### To put the date on a usable format in the test set
test_set$Review_Date_correct<-mdy(test_set$Review_Date)
test_set$Review_Date_correct<-as_date(test_set$Review_Date_correct)

rmse_lambda <- sapply(lambdas, function(l){
  mean_RS<-mean(train_set$Reviewer_Score)
  
  b_h <- train_set %>%
    group_by(Hotel_Name) %>%
    summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+l))
  
  b_r <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
    summarize(b_r = sum(Reviewer_Score - b_h - mean_RS)/(n()+l))
  
  b_d <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    group_by(Review_Date_correct) %>%
    summarize(b_d = sum(Reviewer_Score - b_h - b_r - mean_RS)/(n()+l))
  
  predicted_scores <- test_set %>%
    left_join(b_h, by = "Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    left_join(b_d, by = "Review_Date_correct") %>%
    mutate(pred = mean_RS + b_h + b_r + b_d) %>%
    pull(pred)
  return(RMSE(predicted_scores, test_set$Reviewer_Score))
})

### To determine what lambda corresponds to the smallest RMSE
lambda<-lambdas[which.min(rmse_lambda)]

### RMSE with the selected lambda 
b_h <- train_set %>%
  group_by(Hotel_Name) %>%
  summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+lambda))

b_r <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
  summarize(b_r = sum(Reviewer_Score - b_h - mean_RS)/(n()+lambda))

b_d <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  group_by(Review_Date_correct) %>%
  summarize(b_d = sum(Reviewer_Score - b_h - b_r - mean_RS)/(n()+lambda))

predicted_scores <- test_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by = "Review_Date_correct") %>%
  mutate(pred = mean_RS + b_h + b_r + b_d) %>%
  .$pred

rmse_5 <- RMSE(predicted_scores, test_set$Reviewer_Score)
rmse_results <- data_frame(method = c("Just the average reviewer score", "m1 + Hotel bias", 
                                      "m2 +   Regularization", "m3 + Reviewer bias", 
                                      "m4 + Date bias"),
                           RMSE = c(ref_rmse, rmse_2, rmse_3, rmse_4, rmse_5))

format.data.frame(rmse_results, digit=6)

# I am going to continue by adding the city in which the hotel is located as a predictor. 

### ___MODEL 6 - regularization ==> hotel + date of review + number of reviews per reviewer + hotel city (b_c)
### To get the city from each hotel in the test set
test_set$Hotel_Address<-as.character(test_set$Hotel_Address)
addresses_test<-strsplit(test_set$Hotel_Address, " ")

### Match on country in world cities
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
library(plyr)
test_set$Hotel_city<- llply(addresses_test, function(x)x[max(which(x %in% world.cities$name))])
test_set$Hotel_city<-as.character(test_set$Hotel_city)
detach("package:plyr", unload=TRUE)

rmse_lambda <- sapply(lambdas, function(l){
  mean_RS<-mean(train_set$Reviewer_Score)
  
  b_h <- train_set %>%
    group_by(Hotel_Name) %>%
    summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+l))
  
  b_r <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
    summarize(b_r = sum(Reviewer_Score - b_h - mean_RS)/(n()+l))
  
  b_d <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    group_by(Review_Date_correct) %>%
    summarize(b_d = sum(Reviewer_Score - b_h - b_r - mean_RS)/(n()+l))
  
  b_c <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    left_join(b_d, by = "Review_Date_correct") %>%
    group_by(Hotel_city) %>%
    summarize(b_c = sum(Reviewer_Score - b_h - b_r -b_d - mean_RS)/(n()+l))
  
  
  predicted_scores <- test_set %>%
    left_join(b_h, by = "Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    left_join(b_d, by = "Review_Date_correct") %>%
    left_join(b_c, by = "Hotel_city") %>%
    mutate(pred = mean_RS + b_h + b_r + b_d + b_c) %>%
    pull(pred)
  return(RMSE(predicted_scores, test_set$Reviewer_Score))
})

### To determine what lambda corresponds to the smallest RMSE
lambda<-lambdas[which.min(rmse_lambda)]

### RMSE with the selected lambda 
b_h <- train_set %>%
  group_by(Hotel_Name) %>%
  summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+lambda))

b_r <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
  summarize(b_r = sum(Reviewer_Score - b_h - mean_RS)/(n()+lambda))

b_d <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  group_by(Review_Date_correct) %>%
  summarize(b_d = sum(Reviewer_Score - b_h - b_r - mean_RS)/(n()+lambda))

b_c <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by = "Review_Date_correct") %>%
  group_by(Hotel_city) %>%
  summarize(b_c = sum(Reviewer_Score - b_h - b_r -b_d - mean_RS)/(n()+lambda))

predicted_scores <- test_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by = "Review_Date_correct") %>%
  left_join(b_c, by = "Hotel_city") %>%
  mutate(pred = mean_RS + b_h + b_r + b_d + b_c) %>%
  .$pred

rmse_6 <- RMSE(predicted_scores, test_set$Reviewer_Score)
rmse_results <- data_frame(method = c("Just the average reviewer score", "m1 + Hotel bias", 
                                      "m2+ Regularization", 
                                      "m3 + Reviewer bias", "m4 + Date bias", 
                                      "m5 + City bias"),
                           RMSE = c(ref_rmse, rmse_2, rmse_3, rmse_4, rmse_5, rmse_6))

format.data.frame(rmse_results, digit=6)

# As generalizing reviewer nationality into a more constraint variable (i.e., reviewer continent) is also 
# losing information on the variance of specific groups of reviewers, I expect that the reviewer 
# nationality will be a better predictor than reviewer continent. Nevertheless, for demonstrative 
# purposes, I am going to test both. I begin with the reviewer continent. 

### ___MODEL 7- regularization ==> hotel + date of review + number of reviews per reviewer 
### + hotel city + reviewer continent (b_r_c)

### To create a variable entitled Review_continent in the test set
test_set$Reviewer_continent <- countrycode(sourcevar = test_set[, "Reviewer_Nationality"],
                                           origin = "country.name", destination = "continent")
test_set$Reviewer_continent <- as.character(test_set$Reviewer_continent)
test_set$Reviewer_continent[test_set$Reviewer_Nationality==" Central Africa Republic "]<-"Africa"
test_set$Reviewer_continent[test_set$Reviewer_Nationality==" Antarctica "]<-"Antarctica"
test_set$Reviewer_continent[test_set$Reviewer_Nationality==" Cocos K I "]<-"Oceania"
test_set$Reviewer_continent[test_set$Reviewer_Nationality==" Cura ao "]<-"Americas"
test_set$Reviewer_continent[test_set$Reviewer_Nationality==" Crimea "]<-"Europe"
test_set$Reviewer_continent[test_set$Reviewer_Nationality==" Kosovo "]<-"Europe"
test_set$Reviewer_continent[test_set$Reviewer_Nationality==" Saint Barts "]<-"Americas"
test_set$Reviewer_continent[test_set$Reviewer_Nationality==" Saint Martin "]<-"Americas"
test_set$Reviewer_continent[test_set$Reviewer_Nationality==" United States Minor Outlying Islands "]<-"Americas"

rmse_lambda <- sapply(lambdas, function(l){
  mean_RS<-mean(train_set$Reviewer_Score)
  
  b_h <- train_set %>%
    group_by(Hotel_Name) %>%
    dplyr::summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+l))
  
  b_r <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
    summarize(b_r = sum(Reviewer_Score - b_h - mean_RS)/(n()+l))
  
  b_d <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    group_by(Review_Date_correct) %>%
    summarize(b_d = sum(Reviewer_Score - b_h - b_r - mean_RS)/(n()+l))
  
  b_c <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    left_join(b_d, by = "Review_Date_correct") %>%
    group_by(Hotel_city) %>%
    summarize(b_c = sum(Reviewer_Score - b_h - b_r -b_d - mean_RS)/(n()+l))
  
  b_r_c <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    left_join(b_d, by = "Review_Date_correct") %>%
    left_join(b_c, by = "Hotel_city") %>%
    group_by(Reviewer_continent) %>%
    summarize(b_r_c = sum(Reviewer_Score - b_h - b_r -b_d - b_c - mean_RS)/(n()+l))
  
  predicted_scores <- test_set %>%
    left_join(b_h, by = "Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    left_join(b_d, by = "Review_Date_correct") %>%
    left_join(b_c, by = "Hotel_city") %>%
    left_join(b_r_c, by = "Reviewer_continent") %>%
    mutate(pred = mean_RS + b_h + b_r + b_d + b_c + b_r_c) %>%
    pull(pred)
  return(RMSE(predicted_scores, test_set$Reviewer_Score))
})

### To determine what lambda corresponds to the smallest RMSE
lambda<-lambdas[which.min(rmse_lambda)]

### RMSE with the selected lambda 
b_h <- train_set %>%
  group_by(Hotel_Name) %>%
  summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+lambda))

b_r <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
  summarize(b_r = sum(Reviewer_Score - b_h - mean_RS)/(n()+lambda))

b_d <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  group_by(Review_Date_correct) %>%
  summarize(b_d = sum(Reviewer_Score - b_h - b_r - mean_RS)/(n()+lambda))

b_c <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by = "Review_Date_correct") %>%
  group_by(Hotel_city) %>%
  summarize(b_c = sum(Reviewer_Score - b_h - b_r -b_d - mean_RS)/(n()+lambda))

b_r_c <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by = "Review_Date_correct") %>%
  left_join(b_c, by = "Hotel_city") %>%
  group_by(Reviewer_continent) %>%
  summarize(b_r_c = sum(Reviewer_Score - b_h - b_r -b_d - b_c - mean_RS)/(n()+lambda))

predicted_scores <- test_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by = "Review_Date_correct") %>%
  left_join(b_c, by = "Hotel_city") %>%
  left_join(b_r_c, by = "Reviewer_continent") %>%
  mutate(pred = mean_RS + b_h + b_r + b_d + b_r_c + b_c) %>%
  .$pred

rmse_7 <- RMSE(predicted_scores, test_set$Reviewer_Score)
rmse_results <- data_frame(method = c("Just the average reviewer score", "m1 + Hotel bias", 
                                      "m2 + Regularization", 
                                      "m3 + Reviewer bias", "m4 + Date bias", 
                                      "m5 + City bias",
                                      "m6 + rev. Continent error"),
                           RMSE = c(ref_rmse, rmse_2, rmse_3, rmse_4, rmse_5, rmse_6, rmse_7))

format.data.frame(rmse_results, digit=6)

# Now, I am going to replace the predictor "Reviewer_continent" with "Reviewer_Nationality". 

### MODEL 7 bis - regularization ==> hotel + date of review + number of reviews per reviewer + 
### hotel city + reviewer nationality (b_r_n)

rmse_lambda <- sapply(lambdas, function(l){
  mean_RS<-mean(train_set$Reviewer_Score)
  
  b_h <- train_set %>%
    group_by(Hotel_Name) %>%
    summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+l))
  
  b_r <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
    summarize(b_r = sum(Reviewer_Score - b_h - mean_RS)/(n()+l))
  
  b_d <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    group_by(Review_Date_correct) %>%
    summarize(b_d = sum(Reviewer_Score - b_h - b_r - mean_RS)/(n()+l))
  
  b_c <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    left_join(b_d, by = "Review_Date_correct") %>%
    group_by(Hotel_city) %>%
    summarize(b_c = sum(Reviewer_Score - b_h - b_r -b_d - mean_RS)/(n()+l))
  
  b_r_n <-train_set %>%
    left_join(b_h, by="Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    left_join(b_d, by = "Review_Date_correct") %>%
    left_join(b_c, by = "Hotel_city") %>%
    group_by(Reviewer_Nationality) %>%
    summarize(b_r_n = sum(Reviewer_Score - b_h - b_r - b_d - b_c - mean_RS)/(n()+l))
  
  predicted_scores <- test_set %>%
    left_join(b_h, by = "Hotel_Name") %>%
    left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
    left_join(b_d, by = "Review_Date_correct") %>%
    left_join(b_c, by = "Hotel_city") %>%
    left_join(b_r_n, by = "Reviewer_Nationality") %>%
    mutate(pred = mean_RS + b_h + b_r + b_d + b_r_n + b_c) %>%
    pull(pred)
  return(RMSE(predicted_scores, test_set$Reviewer_Score))
})

### To determine what lambda corresponds to the smallest RMSE
lambda_val1<-lambdas[which.min(rmse_lambda)]

### RMSE with the selected lambda 
b_h <- train_set %>%
  group_by(Hotel_Name) %>%
  summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+lambda_val1))

b_r <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
  summarize(b_r = sum(Reviewer_Score - b_h - mean_RS)/(n()+lambda_val1))

b_d <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  group_by(Review_Date_correct) %>%
  summarize(b_d = sum(Reviewer_Score - b_h - b_r - mean_RS)/(n()+lambda_val1))

b_c <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by = "Review_Date_correct") %>%
  group_by(Hotel_city) %>%
  summarize(b_c = sum(Reviewer_Score - b_h - b_r -b_d - mean_RS)/(n()+lambda_val1))

b_r_n <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by = "Review_Date_correct") %>%
  left_join(b_c, by = "Hotel_city") %>%
  group_by(Reviewer_Nationality) %>%
  summarize(b_r_n = sum(Reviewer_Score - b_h - b_r -b_d - b_c - mean_RS)/(n()+lambda_val1))

predicted_scores <- test_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by = "Review_Date_correct") %>%
  left_join(b_r_n, by = "Reviewer_Nationality") %>%
  left_join(b_c, by = "Hotel_city") %>%
  mutate(pred = mean_RS + b_h + b_r + b_d + b_r_n +b_c) %>%
  .$pred

rmse_8 <- RMSE(predicted_scores, test_set$Reviewer_Score)
rmse_results <- data_frame(method = c("Just the average reviewer score", "m1 + Hotel bias", 
                                      "m2 + Regularization", 
                                      "m3 + Reviewer bias", "m4 + Date bias", 
                                      "m5 + City bias",
                                      "m6 + rev. Continent bias",
                                      "m6 + rev. Nationality bias"),
                           RMSE = c(ref_rmse, rmse_2, rmse_3, rmse_4, rmse_5, rmse_6, rmse_7, rmse_8))

format.data.frame(rmse_results, digit=6)


# As expected, using reviewer nationality is better than using reviewer continent. 
# Model 7 has a RMSE of 1.511. It is the best model produced in this training step and therefore will be 
# tested using the validation set. 
# The lambda term from this model has been saved under the name "lambda_val1". 

#################################################################
#                    Testing the model                          #
################################################################# 
# First, I have to adapt the predictors from the validation set (as for the two other sets). 

### To adapt the date format 
validation$Review_Date_correct<-mdy(validation$Review_Date)
validation$Review_Date_correct<-as_date(validation$Review_Date_correct)
### To create a variable entitled "Reviewer_continent"
validation$Reviewer_continent <- countrycode(sourcevar = validation[, "Reviewer_Nationality"],
                                             origin = "country.name", destination = "continent")


validation$Reviewer_continent <- as.character(validation$Reviewer_continent)
validation$Reviewer_continent[validation$Reviewer_Nationality==
                                " Central Africa Republic "]<-"Africa"
validation$Reviewer_continent[validation$Reviewer_Nationality==" Antarctica "]<-"Antarctica"
validation$Reviewer_continent[validation$Reviewer_Nationality==" Cocos K I "]<-"Oceania"
validation$Reviewer_continent[validation$Reviewer_Nationality==" Cura ao "]<-"Americas"
validation$Reviewer_continent[validation$Reviewer_Nationality==" Crimea "]<-"Europe"
validation$Reviewer_continent[validation$Reviewer_Nationality==" Kosovo "]<-"Europe"
validation$Reviewer_continent[validation$Reviewer_Nationality==" Saint Barts "]<-"Americas"
validation$Reviewer_continent[validation$Reviewer_Nationality==" Saint Martin "]<-"Americas"
validation$Reviewer_continent[validation$Reviewer_Nationality==
                                " United States Minor Outlying Islands "]<-"Americas"
### To get the city from each hotel 
validation$Hotel_Address<-as.character(validation$Hotel_Address)
addresses_validation<-strsplit(validation$Hotel_Address, " ")

### Match on country in world cities
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
library(plyr)
validation$Hotel_city<- llply(addresses_validation, 
                              function(x)x[max(which(x %in% world.cities$name))])
validation$Hotel_city<-as.character(validation$Hotel_city)
detach("package:plyr", unload=TRUE)


# The lambda term has been saved from the previous section (i.e., "lambda_val1), and therefore do not need to be re-computed. 

### MODEL FINAL - regularization ==> hotel + date of review + 
### number of reviews per reviewer + hotel city + reviewer nationality

b_h <- train_set %>%
  group_by(Hotel_Name) %>%
  summarize(b_h = sum(Reviewer_Score - mean_RS)/(n()+lambda_val1))

b_r <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
  summarize(b_r = sum(Reviewer_Score - b_h - mean_RS)/(n()+lambda_val1))

b_d <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  group_by(Review_Date_correct) %>%
  summarize(b_d = sum(Reviewer_Score - b_h - b_r - mean_RS)/(n()+lambda_val1))

b_c <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by = "Review_Date_correct") %>%
  group_by(Hotel_city) %>%
  summarize(b_c = sum(Reviewer_Score - b_h - b_r -b_d - mean_RS)/(n()+lambda_val1))

b_r_n <-train_set %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by = "Review_Date_correct") %>%
  left_join(b_c, by = "Hotel_city") %>%
  group_by(Reviewer_Nationality) %>%
  summarize(b_r_n = sum(Reviewer_Score - b_h - b_r -b_d - b_c - mean_RS)/(n()+lambda_val1))

predicted_scores <- validation %>%
  left_join(b_h, by="Hotel_Name") %>%
  left_join(b_r, by = "Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by = "Review_Date_correct") %>%
  left_join(b_c, by = "Hotel_city") %>%
  left_join(b_r_n, by = "Reviewer_Nationality") %>%
  mutate(pred = mean_RS + b_h + b_r + b_d + b_c + b_r_n) %>%
  .$pred

VALIDATION_RMSE <- RMSE(predicted_scores, validation$Reviewer_Score)
rmse_results <- data_frame(method = c("Just the average reviewer score", "m1 + Hotel bias", 
                                      "m2 + Regularization", 
                                      "m3 + Reviewer bias", "m4 + Date bias", 
                                      "m5 + City bias",
                                      "m6 + rev. Continent bias",
                                      "m6 + rev. Nationality bias",  
                                      "VALIDATION"),
                           RMSE = c(ref_rmse, rmse_2, rmse_3, rmse_4, rmse_5, rmse_6, 
                                    rmse_7, rmse_8, VALIDATION_RMSE))


format.data.frame(rmse_results, digit=6)

# The validation RMSE is 1.510. It is significantly better than the naive model. 
# However, there is still room for improvements. 
# In the next section to apply another machine learning technique to try to enhance the accuracy 
# of this recommendation system.  

#################################################################
#  Comparison of PCA and regression-based recommendation model  #
#################################################################

# ____Regression-based recommendation model____  
# In this section, I am going to test the effect of PCA in a recommendation system and compare its results
# to an equivalent regression-based system. 
# I am going to include only the numerical predictors (i.e., Total number of reviews a reviewer has given, 
# review date and the total number of review per hotel) in the PCA. The number of reviews per hotel was 
# covered in the previous recommendation system by the hotel bias and regularization however as do not 
# want to include categorical variables in the PCA, I am going to use the variable "Total_Number_of_Reviews"
# already existing in the data set. Additionally, to ease the PCA computation I also going to use another 
# format for the reviewing date. As one variable equivalent variable directly reporting the number of days
# since the review already exists in the data set, I am just going to switch to this variable. However, 
# this variable also has the words "day" or "days" after each number in all cells. Thus, I will 
# have to do a small pre-processing step to isolate the number.   


### To split the column "days_since_review" based on the space between the number and the word "day" 
train_set <- train_set%>% 
  extract(days_since_review, c("n_days", "rest"), regex = "^(\\S+)\\s+(.*)", convert = TRUE) 
test_set <- test_set%>% 
  extract(days_since_review, c("n_days", "rest"), regex = "^(\\S+)\\s+(.*)", convert = TRUE) 
validation <- validation %>% 
  extract(days_since_review, c("n_days", "rest"), regex = "^(\\S+)\\s+(.*)", convert = TRUE) 

# I am going to first create the regression-based recommendation system using only the numeric predictors.  

### ___MODEL 1 (m1 - 2nd sys) - already exists ==> mean_RS
### ___MODEL 2 (m2 - 2nd sys) - average rating + Total_Number_of_Reviews_Reviewer_Has_Given (b_r)
b_r<-train_set%>%
  group_by(Total_Number_of_Reviews_Reviewer_Has_Given)%>%
  summarize(b_r=mean(Reviewer_Score - mean_RS))

predicted_scores<-mean_RS+test_set%>% 
  left_join(b_r, by="Total_Number_of_Reviews_Reviewer_Has_Given")%>%
  .$b_r

rmse_2<-RMSE(test_set$Reviewer_Score, predicted_scores)

### ___MODEL 3 (m3 - 2nd sys) - average rating + Total_Number_of_Reviews_Reviewer_Has_Given + Date (b_d)
b_d<-train_set%>%
  left_join(b_r, by="Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  group_by(n_days)%>%
  summarize(b_d=mean(Reviewer_Score - mean_RS - b_r))

predicted_scores<- test_set%>% 
  left_join(b_r, by="Total_Number_of_Reviews_Reviewer_Has_Given")%>%
  left_join(b_d, by="n_days")%>%
  mutate(pred = mean_RS + b_r + b_d) %>%
  .$pred

rmse_3<-RMSE(test_set$Reviewer_Score, predicted_scores)

### ___MODEL 4 (m4 - 2nd sys) - average rating + Total_Number_of_Reviews_Reviewer_Has_Given + Date + 
### number of reviews (b_n_r)
b_n_r<-train_set%>%
  left_join(b_r, by="Total_Number_of_Reviews_Reviewer_Has_Given") %>%
  left_join(b_d, by="n_days") %>%
  group_by(Total_Number_of_Reviews)%>%
  summarize(b_n_r=mean(Reviewer_Score - mean_RS - b_r - b_d))

predicted_scores<- test_set%>% 
  left_join(b_r, by="Total_Number_of_Reviews_Reviewer_Has_Given")%>%
  left_join(b_d, by="n_days")%>%
  left_join(b_n_r, by="Total_Number_of_Reviews")%>%
  mutate(pred = mean_RS + b_r + b_d + b_n_r) %>%
  .$pred

rmse_4<-RMSE(test_set$Reviewer_Score, predicted_scores)

### MODEL 4 (m4 - 2nd sys) - VALIDATION
predicted_scores<-validation%>% 
  left_join(b_r, by="Total_Number_of_Reviews_Reviewer_Has_Given")%>%
  left_join(b_d, by="n_days")%>%
  left_join(b_n_r, by="Total_Number_of_Reviews")%>%
  mutate(pred = mean_RS + b_d + b_r + b_n_r) %>%
  .$pred

rmse_5<-RMSE(validation$Reviewer_Score, predicted_scores)
rmse_results<-data_frame(method=c("Just the average reviewer score", "m1 - 2nd sys + Reviewer bias", "m2 - 2nd sys + Date bias", "m3 - 2nd sys + Review bias", "VALIDATION"),
                         RMSE=c(ref_rmse, rmse_2, rmse_3, rmse_4, rmse_5))
format.data.frame(rmse_results, digits=6)

# The validation RMSE is 1.536 and the number of reviews is the predictor with the best effect size. 
# => I am now going to build the equivalent PCA recommendation system.  

# As there is no "tuning" of the algorithm, I only need two sets; the training set and the validation set.
# So first, to avoid losing power in not using the test set, I am going to merge it with the training set 
# (i.e., train_set). Furthermore, the split 90%/10% is perfectly adapted to maximize the accuracy of the 
# training and leaving enough data to mimic real-world results' variance.  

### To merge the training and the test sets 
train_set<-rbind(train_set, test_set)

# Second, I create the correlation matrix to assess if my predictors are highly correlated.  

### Required libraries for the PCA
if(!require(chillR)) install.packages("chillR", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(pls)) install.packages("pls", repos = "http://cran.us.r-project.org")
library(chillR)
library(factoextra)
library(pls)

### To create a data set with the variables of interest 
train_PCA<-train_set%>% 
  select (Total_Number_of_Reviews_Reviewer_Has_Given, n_days, Total_Number_of_Reviews,
          Reviewer_Score)

### To remove the variable that is going to be predicted 
predictor_PCA<- train_PCA[,-4]

### To change the names that are a bit long for the correlation matrix
colnames(predictor_PCA)<- c("n rev. per Reviewer", "days since review", "n Reviews" )

### To do the correlation matrix
cor(predictor_PCA)

# That's excellent. The predictors are independent and do not correlate at all.   
# Third, I going to analyse the principal variance components. 

### PCA
rec_pca<-prcomp(predictor_PCA, center=TRUE, scale. =TRUE)

### To summarize the results 
summary(rec_pca)

### To do a scree plot with the different components 
fviz_eig(rec_pca)

# The scree plot displays that the three components each explain approximately 30% of the total results' 
# variance.  

# Fourth, I am not going to apply the concept of PCA on a regression. To do so, I am going to use 
# 10-fold cross-validation to be sure to have robust results.  

### Regression PCA
pcr_fit <- pcr(Reviewer_Score ~ Total_Number_of_Reviews_Reviewer_Has_Given + n_days + 
                 Total_Number_of_Reviews, data = train_PCA, segments=10, validation="CV")

rmse_CV <- RMSEP(pcr_fit, estimate="CV")

### To do the scree plot
plot(rmse_CV, xaxt="n", main="Predicted RMSE by number of components",  xlab="Number of components", 
     ylab="Predicted RMSE") 

### To put levels on the x axis 
axis(1, at=c(0,1,2,3), labels = c(0,1,2,3))

### To see how many components are required to have the smallest RMSE
n_comp <- which.min(rmse_CV$val) 

# The results show that using three components would provide the best RMSE. Nevertheless, 
# the graph shows that the first component is the most important. It also appear that the RMSE 
# is not good (close to the reference RMSE). Finally, I am going to test this approach on the 
# validation set.  

### To predict the score in the validation set 
fit <- predict(pcr_fit, validation, ncomp=(as.numeric(n_comp)-1))
### To compare the predicted scores and the real ones 
rmse_PCA <- RMSE(validation$Reviewer_Score, fit)
rmse_results<-data_frame(method=c("Just the average reviewer score", "m1 - 2nd sys + Reviewer bias", 
                                  "m2 - 2nd sys + Date bias", "m3 - 2nd sys + Review bias", 
                                  "VALIDATION", "PCA"),
                         RMSE=c(ref_rmse, rmse_2, rmse_3, rmse_4, rmse_5, rmse_PCA ))
format.data.frame(rmse_results, digits=6)

# The table displays that with this data set and the selected predictors, the regression-based 
# recommendation system provides the best RMSE, and therefore can be considered as the most accurate. 
# The RMSE for the PCA (1.635) is even higher than the one from the naive model that 
# only used the average reviewer score. PCA was not a suitable approach for this data set and predictors. 


#######################################################################################################
#                                          CONCLUSION                                                #
#######################################################################################################

# This manuscript reports the creation of a recommendation system using different machine learning techniques 
# (i.e., regressions-based, regularization, and PCA). 
# ==> The reference RMSE value in the naive model was 1.630. 
# ==> When including all potential predictors in the approach combining regression-based and regularization 
# techniques, the best RMSE was 1.510. 
# ==> The most important predictor was the hotel bias.

# In a second step, to answer the requirements of the assignment I did compare a PCA recommendation 
# system (including only numeric predictors) and its regression_based equivalent. 
# ==> The RMSE for the PCA was 1.635 while it was 1.536 for the regression-based approach. 
# ==> In this data set, the regression-based approach was the most suitable to obtain the smallest RMSE. 
 

# Nevertheless, several limitations are to be mentioned. 
# The data set used in this report had missing information (i.e., no user ID) that have affected the 
# results, especially in the PCA and in the regularization. 
# ==> The absence of user ID pushed me to use a predictors-based approach of PCA. 
# ==> Without user ID, a two dimensions score matrix was impossible to construct.
# ==> Replacing user ID by another predictor (e.g., Total number of reviews reviewer has given) 
# would have led to a three dimensions matrix. 

# Therefore, it cannot be concluded that a regression-based approach is better than PCA overall. 
# It is very likely that with the proper variables, the PCA would have obtained better results than the ones 
# reported in this report. 


#######################################################################################################
#                                          REFERENCES                                                 #
#######################################################################################################
#[1] https://rafalab.github.io/dsbook/   
#[2] https://learning.edx.org/course/course-v1:HarvardX+PH125.9x+1T2021/home  
#[3] https://www.kaggle.com/jiashenliu/515k-hotel-reviews-data-in-europe  
#[4] https://www.dropbox.com/s/pehhztgk97ftvsu/Hotel.zip?dl=0  
#[5] Subasi A, Gursoy MI. EEG signal classification using PCA, ICA, LDA and support vector machines. Expert Syst Appl. 2010;37:8659-8666.  
#[6] https://towardsdatascience.com/pca-is-not-feature-selection-3344fb764ae6  
#[7] https://jbhender.github.io/Stats506/F17/Projects/G18.html