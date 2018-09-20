#TO DO:
# get names of authors into lists by conference
#for each conference:
# read through each list of authors and find their gender in the gender lists 
# - create counts of women, men, and N/A, graph line
#graph fraction of women?

#Reading in JSON files

library(jsonlite)
library(dplyr)
library(stringr)
library(stringr)
library(readr)
library(ggplot2)

setwd("~/Desktop/Thesis/CODE")
inf_gender_data <- fromJSON(read_file("inferred_gender_mapping.json"))
ver_gender_data <- fromJSON(read_file("verified_gender_mapping.json"))
gender_data <- rbind(inf_gender_data, ver_gender_data)

#COMBINE GENDER DATA

#loop to get PC chairs by conference
conf_files <- list.files("/conf")
counter = 0
df_pc <- data.frame(conf = c(), total_pc_chairs = c(), women_pc_chairs = c(), ratio_women_pc_chairs = c())

for (file in conf_files) {
    #grab a conference
    conference <- fromJSON(read_file(file))
    
    pc_chair_list_with_institution = conference$pc_chairs
    
    counter = 0
    pc_chair_list = list()
    #removing institution from names in list of PC Chairs
    for (chair in pc_chair_list_with_institution){
    	name = gsub("(.*) \\(.*\\)$", "\\1", chair)
    	last_first_name = gsub("(.*) (.*)$", "\\2, \\1" )
    	pc_chair_list[counter] <- last_first_name
    	counter = counter + 1
    }
    
    pc_chair_list = 
    
    total = 0
    women_total = 0
    #counting up the totals
    for (person in pc_chair_list) {
    	
        gender = gender_data[person]
        
        if (gender != "N/A") {
        	if (gender == "F") {
            	women_total = women_total + 1
        	total = total + 1
        	}
        }
    }
           
    ratio = women_total/total
    
    #add values to data frame
    rbind(df_pc, conf = conference$key, total_pc_chairs = total, women_pc_chairs = women_total, ratio_women_pc_chairs = ratio)
}

pc_data <- table(df_pc$ratio_women_pc_chairs)

barplot(pc_data, main = "PC Chair Gender Distribution", horiz = TRUE, names.arg=conf)

#distilling data for graph
#for each file in author_data, grab name (first variable) and make list of those
#MAKE EVERYTHING DATAFRAMES < NOT LISTS 
#df <- data.frame(conf = c(), total = c(), female=c()) ---> sets columns
#for ...
#    rbind(df, conf=,  )
#df$conf <- list_of_conf_names

#creating graph

# Simple Horizontal Bar Plot with Added Labels 
#counts <- table(mtcars$gear)
#barplot(counts, main="Car Distribution", horiz=TRUE,
#  names.arg=c("3 Gears", "4 Gears", "5 Gears"))

