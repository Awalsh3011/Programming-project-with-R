library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(tidyverse)


#make function called import, when it is called, it will import the files into a folder
import <- function() {
  #unzip the folder
  zipF<-file.choose() #choose a zip folder that you want to be unzipped
  outDir<- "C:\\Users\\awals\\OneDrive\\Documents\\3rd year\\Statistical Programming\\Assignment 2\\assignment 2\\CA2 -R-" # this is the area where you want it to be unzipped to 
  unzip(zipF,exdir=outDir) # function that actually unzips the folder
}
imported<-import() # calling the function to unzip the folder

# using pipes to read each file and then changing them into a data frame so as the data can be more accessible
clicks_df<- read_tsv('clicks.tsv') %>% data.frame()
impressions_df <- read_tsv('impressions.tsv') %>% data.frame()
advertiser_df <- read_csv("advertiser.csv") %>% data.frame()
campaigns_df <-  read_csv("campaigns.csv") %>% data.frame()



# print out each one to see it and make sure it is correct
print(clicks_df)
print(impressions_df)
print(advertiser_df)
print(campaigns_df)


#Transform
#converting time zones
#21:35 Monday, Coordinated Universal Time (UTC) is 13:35 Monday, Pacific Time (PT)- UTC is 8 hours ahead of pacific
#21:37 Monday, Coordinated Universal Time (UTC) is 16:37 Monday, Eastern Time (ET) - UTC is 5 hours ahead of pacific



clicks_df<- read_tsv('clicks.tsv') %>% data.frame()
print(clicks_df)
#Time change function

#making a timezone converter function which uses table_n as the table that is passed through so that it can be used for both

convert_timezone <- function(table_n) { 
  # set a to be the length of the first column of the table rather than direct value so as it can be used for many tables
  a<-length(table_n[,1])
  my_sequence = 1:a
  # use for loop to search through the table
  for(var in my_sequence){
    #using if statement to check the timezone since they are the main differences in this problem
    if (table_n$timezone[var] == ('Eastern time')){
      print('old eastern time')
      print(table_n$time[var])
      
      #setting the original time as a old eastern time variable       
      old_e_time<-as.POSIXct(table_n$time[var])
      # setting seconds eastern as 5 hours x 60 mminutes x 60 seconds in irder to add it on the the time variables
      seconds_eastern<- 5 *60*60
      #adding the two times together as eastern time is 5 hours behind UTC
      new_time <- (old_e_time + seconds_eastern )
      #using the format to get it in correct format
      new_t<- format(as.POSIXlt(new_time), format = "%H:%M:%S")
      print('new eastern time')
      print(new_t)
      # changing the values in the table to the new values
      pos<- var
      table_n$time[[var]][table_n$time[[pos]] %in% old_e_time] <- new_t

      day_seconds <- 24*60*60
      #use an if statement below to see if the time is now greater than 24 hours -> needing the date to be changed as well
      # and the time would need to be changed also
      if (new_time > day_seconds){
        # taking away the seconds in 24 hours so as it would be an actual time
        p <- new_time - day_seconds
        n <- format(as.POSIXct(p), format = "%H:%M:%S")
        print("time after removing the 24 hours")
        print(n)
        
        # replace the new time in the table
        # replace(table_n$time, table_n$time[var], p )
        
        # adding a day onto the date variable as changing time zones requires it to go into the next day
        original_date<-as.Date(table_n$date[var],format="%d/%m/%Y")
        #adding one on to the date as the 24 hours has just been taken off the time 
        new_date<- original_date + 1
        new_date<- as.Date(new_date)
        print(new_date)
        
        # using the same format as above to replace the date in the table
        pos<- var
        table_n$date[[var]][table_n$date[[pos]] %in% original_date] <- new_date
        
        
        
      #using else statement in case the time doesnt go over the 24 hours and therefore the date doesnt change
      } else{
        print('date doesnt change')
      }
    }
    # All of the above is repeated for the Pacific time, it the exact same except it is eight hours ahead so the seconds_pacific
    # will be 8 X60 X60
    else if (table_n$timezone[var] == ('Pacific time')){
      print('old pacific time')
      print(table_n$time[var])
      old_pacific_time<- table_n$time[var]
      old_p_time<- as.POSIXct(old_pacific_time)
      seconds_pacific<-8 *60*60
      print('new pacific time')
      new_time <-(old_p_time + seconds_pacific)
      new_t<- format(as.POSIXct(new_time), format = "%H:%M:%S")
      print(new_t)
      
      n_t<- (table_n$time[var] +seconds_pacific)
      old<- (table_n$time[var])
      pos<- var
      table_n$time[[var]][table_n$time[[pos]] %in% old_p_time] <- new_t

      
      if (new_time > day_seconds){
        new_time_2 <- (new_time - day_seconds)
        print("time after removing the 24 hours")
        new_time_2<- format(as.POSIXct(new_time), format = "%H:%M:%S")
        print(new_time_2)
        
        n_t<- (table_n$time[var] +seconds_pacific)
        old<- (table_n$time[var])
        pos<- var
        table_n$time[[var]][table_n$time[[pos]] %in% new_t] <- new_time_2
        
        # adding a day onto the date variable as changing time zones requires it to go into the next day
        original_date<-as.Date(table_n$date[var],format="%d/%m/%Y")
        new_date<- original_date + 1
        new_date<-as.Date(new_date)
        print('new date')
        print(new_date)
        
        pos<- var
        table_n$date[[var]][table_n$date[[pos]] %in% original_date] <- new_date
        
      } else{
        print('date doesnt change')
      }
    }
    #Using a final else statement incase the time is already in a UTC timezone and nothing needs to c=be changed
    else{
      print('It is already UTC timezone')
    }
  }
  #replacing all the timezones to saying UTC as they now are chnaged
  table_n[table_n == 'Eastern time'] <- 'UTC'
  table_n[table_n == 'Pacific time'] <- 'UTC'
  
  # printing the final table and returning it back out of the function so as it can be used again
  print(table_n)
  return(table_n)
}


#calling the convert_timezone function for the clicks and impressions dataframes and storing them
converted_clicks<- convert_timezone(clicks_df)
converted_impressions<- convert_timezone(impressions_df)

print(converted_impressions)
print(converted_clicks)

print(clicks_df)

#making them into two two tables
#use two different functions to make the joins so as the tables can be returned as a function can only return one thing
# use a left join in both tables as it takes the impressions and clicks data and links it to the campaign and advertisers data 
#it leaves out the unnecessary values from the advertisers and campaign data

make_impression_join <- function() {  
  # make the join where campaign id in impressions where the id in campaign are equal
  new_impressions_df <- left_join(converted_impressions, campaigns_df, by = c('campaign_id' = 'id'))
  print(new_impressions_df)
  #make a join from the new impressions table where advertiser id is equal to the id in advertisers
  new_impressions_table<- left_join(new_impressions_df, advertiser_df, by = c('advertiser_id' = 'ID'))
  print(new_impressions_table)
  #return the table so as it can be used further
  return(new_impressions_table)
}

# do this again wiht the click tables in the same way
make_click_join<- function(){
  new_clicks_df <- left_join(converted_clicks, campaigns_df, by = c('campaign_id' = 'id'))
  print(new_clicks_df)
  new_clicks_table<- left_join(new_clicks_df, advertiser_df, by = c('advertiser_id' = 'ID'))
  print(new_clicks_table)
  return(new_clicks_table)
}


#call the functions to join the tables
click_join_table<- make_click_join()
impression_join_table<-make_impression_join()

print(impression_join_table)

print(click_join_table)


# exporting them out as a csv
#using a function to write the csv


making_csvs<- function() {
  write.csv(impression_join_table, "C:\\Users\\awals\\OneDrive\\Documents\\3rd year\\Statistical Programming\\Assignment 2\\assignment 2\\CA2 -R-\\impressions_processed.csv" , row.names = FALSE)
  write.csv(click_join_table, "C:\\Users\\awals\\OneDrive\\Documents\\3rd year\\Statistical Programming\\Assignment 2\\assignment 2\\CA2 -R-\\clicks_processed.csv" , row.names = FALSE)
}

# call the fucntion to make the csv
making_csvs()
