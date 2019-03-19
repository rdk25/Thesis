library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(plyr)
library(rquery)
library(wordcloud)
library(tm)
library(xtable)

#helper functions for reading survey files
json_survey_reader <- function(filename){
  file <- fromJSON(read_file(filename))
  unlisted_file <- unlist(file)
  df <- data.frame(t(unlisted_file))
  return(df)
}

#reading in 
setwd("~/Desktop/Thesis Survey Data")
survey_dataframe1 <- json_survey_reader("38.json")

systems <- list.files("~/Desktop/Thesis Survey Data/systems_surveys")
setwd("~/Desktop/Thesis Survey Data/systems_surveys")
for (file in systems){
  file_df <- json_survey_reader(file)
  survey_dataframe1 <- rbind(survey_dataframe1,file_df)
}

onetwo <- list.files("~/Desktop/Thesis Survey Data/121_surveys")
setwd("~/Desktop/Thesis Survey Data/121_surveys")
for (file in onetwo){
  file_df <- json_survey_reader(file)
  survey_dataframe1 <- rbind(survey_dataframe1,file_df)
  
}

twotwo <- list.files("~/Desktop/Thesis Survey Data/221_surveys")
setwd("~/Desktop/Thesis Survey Data/221_surveys")
for (file in twotwo){
  file_df <- json_survey_reader(file)
  survey_dataframe1 <- rbind(survey_dataframe1,file_df)
  
}

AI <- list.files("~/Desktop/Thesis Survey Data/AI_surveys")
setwd("~/Desktop/Thesis Survey Data/AI_surveys")
AI_data <- lapply(lapply(AI,read_file),fromJSON)
for (file in AI){
  file_df <- json_survey_reader(file)
  survey_dataframe1 <- rbind(survey_dataframe1,file_df)
}

compcomp <- list.files("~/Desktop/Thesis Survey Data/CompComp_surveys")
setwd("~/Desktop/Thesis Survey Data/CompComp_surveys")
for (file in compcomp){
  file_df <- json_survey_reader(file)
  survey_dataframe1 <- rbind(survey_dataframe1,file_df)
}

# function for number of observations for plots
give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

survey_dataframe1$gender <- as.factor(survey_dataframe1$gender)
survey_dataframe1$class <- as.factor(survey_dataframe1$class)
survey_dataframe1$ordered_classes  = factor(survey_dataframe1$class, levels=c("121", "221", "compcomp","AI","systems"))
survey_dataframe1$ordered_classes <- as.factor(survey_dataframe1$ordered_classes)

survey_dataframe1$is_male <- survey_dataframe1$gender == "m"
survey_dataframe1$gender[survey_dataframe1$gender == "nb"] <- "o"
survey_dataframe1 <- survey_dataframe1[-c(72,82),]

a <- data.frame(apply(survey_dataframe1[,!names(survey_dataframe1) %in% c("is_male","gender","class","role_model","like","want_change", "ordered_classes")],2,function (a) as.numeric(as.character(a))))
b <- data.frame(is_male=survey_dataframe1$is_male)
c <- data.frame(class=survey_dataframe1$class)
w <- data.frame(ordered_classes=survey_dataframe1$ordered_classes)
d <- cbind(a,b,c)
e <- group_by(d, class, is_male) %>% add_tally() %>% ungroup()
f <- na.omit(data.frame(gender = survey_dataframe1$gender))
g <- cbind(a,w,f)
g$ordered_classes <- as.factor(g$ordered_classes)

#Self-assessed ability by Gender boxplot
ggplot(g, aes(x=factor(gender),y=my_ability))+geom_boxplot()+labs(title = "Self-Assessed CS Ability by Gender")+xlab("Gender")+ylab ("CS Passion")+scale_x_discrete(labels = c("Male","Female","Non-Binary/Other"))+stat_summary(fun.data = give.n, geom = "text", fun.y = median,position = position_dodge(width = 0.75))

#passion boxplot
ggplot(g, aes(x=factor(gender),y=my_passion))+geom_boxplot()+labs(title = "CS Passion by Gender")+xlab("Gender")+ylab ("CS Passion")+scale_x_discrete(labels = c("Male","Female","Non-Binary/Other"))+stat_summary(fun.data = give.n, geom = "text", fun.y = median,position = position_dodge(width = 0.75))

#confidence boxplot
ggplot(g, aes(x=factor(gender),y=confident_solve_problem))+geom_boxplot()+labs(title = "Confidence to Solve a CS Problem by Gender")+xlab("Gender")+ylab("Confidence Level")+scale_x_discrete(labels = c("Male","Female","Non-Binary/Other"))+stat_summary(fun.data = give.n, geom = "text", fun.y = median,position = position_dodge(width = 0.75))

#encouraged to major boxplot
ggplot(g, aes(x=factor(gender),y=encouraged_to_major))+geom_boxplot()+labs(title = "Perceived Encouragement to Major in CS by Gender")+xlab("Gender")+ylab ("Encouraged to Major")+scale_x_discrete(labels = c("Male","Female","Non-Binary/Other"))+stat_summary(fun.data = give.n, geom = "text", fun.y = median,position = position_dodge(width = 0.75))

#ggplot(e, aes(x=class,y=my_ability, fill=is_male),stat = "count")+geom_boxplot()+labs(title = "Self-Assessed CS Ability by Course")+xlab("Course")+ylab ("CS Ability")+geom_text(y=5.5,label=n)

#CS Ability by Class and Gender (w/numbers!)
ggplot(g, aes(x=ordered_classes,y=my_ability, fill=factor(gender,labels = c("Male","Female","Non-Binary/Other"))),stat = "count")+geom_boxplot()+labs(fill = "Gender")+labs(title = "Self-Assessed CS Ability by Course")+xlab("Course")+ylab ("CS Ability")+stat_summary(fun.data = give.n, geom = "text", fun.y = median,position = position_dodge(width = 0.75))+scale_x_discrete(labels = c('121','221','Computability & Complexity',"Artificial Intelligence","Systems"))
#Passion for CS ggplot
ggplot(g, aes(x=ordered_classes,y=my_passion, fill=factor(gender,labels = c("Male","Female","Non-Binary/Other"))),stat = "count")+geom_boxplot()+labs(fill = "Gender")+labs(title = "CS Passion by Course")+xlab("Course")+ylab ("Passion for CS")+stat_summary(fun.data = give.n, geom = "text", fun.y = median,position = position_dodge(width = 0.75))+scale_x_discrete(labels = c('121','221','Computability & Complexity',"Artificial Intelligence","Systems"))
#confidence ggplot
ggplot(g, aes(x=ordered_classes,y=confident_solve_problem, fill=factor(gender,labels = c("Male","Female","Non-Binary/Other"))),stat = "count")+geom_boxplot()+labs(fill = "Gender")+labs(title = "Confidence to Solve a CS Problem by Course")+xlab("Course")+ylab ("Confidence Level")+stat_summary(fun.data = give.n, geom = "text", fun.y = median,position = position_dodge(width = 0.75))+scale_x_discrete(labels = c('121','221','Computability & Complexity',"Artificial Intelligence","Systems"))
#encouraged to major ggplot
ggplot(g, aes(x=ordered_classes,y=encouraged_to_major, fill=factor(gender,labels = c("Male","Female","Non-Binary/Other"))),stat = "count")+geom_boxplot()+labs(fill = "Gender")+labs(title = "Perceived Encouragement to Major in CS by Course")+xlab("Course")+ylab ("Encouraged to Major in CS Rank")+stat_summary(fun.data = give.n, geom = "text", fun.y = median,position = position_dodge(width = 0.75))+scale_x_discrete(labels = c('121','221','Computability & Complexity',"Artificial Intelligence","Systems"))

#Profs like me vs. CS Passion scatterplot w/regression line
ggplot(g, aes(x=profs_like_me, y=my_passion, color = gender))+geom_smooth(method=lm, se=T)+geom_rug(alpha=0.2)+geom_jitter()


#WORD CLOUDS!
#all students like word cloud
survey_dataframe1$like[survey_dataframe1$like == "NA"] <- NA
survey_dataframe1$want_change[survey_dataframe1$want_change == "NA"] <- NA
like_words <- na.omit(data.frame(like = survey_dataframe1$like))
like_char_string <- toString(like_words$like)
like_char_string <- tolower(gsub("[[:punct:]]", " ",like_char_string))
like_char_string <- gsub("profs", "professors",like_char_string)
#uncomment for cloud!
wordcloud(like_char_string, type="text", lang="english",scale=c(4,.5),random.color = TRUE, random.order = FALSE)

#all students want change word cloud
want_change_words <- na.omit(data.frame(want_change = survey_dataframe1$want_change))
want_change_char_string <- toString(want_change_words$want_change)
want_change_char_string <- tolower(gsub("[[:punct:]]", " ",want_change_char_string))
want_change_char_string <- gsub("profs", "professors",want_change_char_string)
want_change_char_string <- gsub("like", " ",want_change_char_string)
want_change_char_string <- gsub("don", " ",want_change_char_string)
g_like_char_string <- gsub("way", " ",g_like_char_string)
g_like_char_string <- gsub("prof", "professor",g_like_char_string)
#uncomment for cloud!
#wordcloud(want_change_char_string, type="text", lang="english",scale=c(3,.5),random.color = TRUE)

#GeM like cloud
g_like <- survey_dataframe1$like[survey_dataframe1$gender != 'm']
g_like_words <- na.omit(data.frame(g_like = survey_dataframe1$like))
g_like_char_string <- toString(g_like_words$g_like)
g_like_char_string <- tolower(gsub("[[:punct:]]", " ",g_like_char_string))
g_like_char_string <- gsub("profs", "professors",g_like_char_string)
g_like_char_string <- gsub("just", "",g_like_char_string)
#uncomment for cloud!
#wordcloud(g_like_char_string, type="text", lang="english",scale=c(4,.5),random.color = TRUE)

#Male like cloud
m_like <- survey_dataframe1$like[survey_dataframe1$gender == 'm']
m_like_words <- na.omit(data.frame(m_like = survey_dataframe1$like))
m_like_char_string <- toString(m_like_words$m_like)
m_like_char_string <- tolower(gsub("[[:punct:]]", " ",m_like_char_string))
m_like_char_string <- gsub("profs", "professors",m_like_char_string)
m_like_char_string <- gsub("just", "",m_like_char_string)
#uncomment for cloud!
#wordcloud(m_like_char_string, type="text", lang="english",scale=c(4,.5),random.color = TRUE)

#GeM change cloud
g_change <- survey_dataframe1$want_change[survey_dataframe1$gender != 'f']
g_change_words <- na.omit(data.frame(g_change = survey_dataframe1$want_change))
g_change_char_string <- toString(g_change_words$g_change)
g_change_char_string <- tolower(gsub("[[:punct:]]", " ",g_change_char_string))
g_change_char_string <- gsub("profs", "professors",g_change_char_string)
g_change_char_string <- gsub("just", "",g_change_char_string)
g_change_char_string <- gsub("professors", "",g_change_char_string)
g_change_char_string <- gsub("like", "",g_change_char_string)
g_change_char_string <- gsub("classes", "",g_change_char_string)
g_change_char_string <- gsub("students", "",g_change_char_string)
#uncomment for cloud!
#uncomment for cloud!
#uncomment for cloud!
wordcloud(g_change_char_string, type="text", lang="english",scale=c(4,.5),random.color = TRUE, max.words = 20)

#Male change cloud
m_change <- survey_dataframe1$want_change[survey_dataframe1$gender == 'm']
m_change_words <- na.omit(data.frame(m_change = survey_dataframe1$want_change))
m_change_char_string <- toString(m_change_words$m_change)
m_change_char_string <- tolower(gsub("[[:punct:]]", " ",m_change_char_string))
m_change_char_string <- gsub("profs", "",m_change_char_string)
m_change_char_string <- gsub("just", "",m_change_char_string)
m_change_char_string <- gsub("professors", "",m_change_char_string)
m_change_char_string <- gsub("like", "",m_change_char_string)
m_change_char_string <- gsub("classes", "",m_change_char_string)
m_change_char_string <- gsub("students", "",m_change_char_string)
#uncomment for cloud!
#uncomment for cloud!
wordcloud(m_change_char_string, type="text", lang="english",scale=c(4,.5),random.color = TRUE, max.words = 20)

#NUMBERS!!!

# %men think their ability is above average
m <- filter(h, gender == "m")
c <- count(m$my_ability >= 4)
#True 33
#False 17
#66% of men think they're above average

# %women think their ability above average
w <- filter(h, gender == "f")
d <- count(w$my_ability >= 4)
#True 7
#False 17
#29% of women think they're above average

#just for 121
m <- filter(g, gender == "m")
q <- filter(m, class == "121")
c <- count(q$my_ability >= 4)
#True 10
#False 1
#91% true
w <- filter(g, gender == "f")
x <- filter(w, class == "121")
d <- count(x$my_ability >= 4)
#True 0
#False 11
#0% true 

#perception of "Profs like me" male
m <- filter(h, gender == "m")
c <- count(m$profs_like_me >= 4)
#True 25
#False 25

#perception of "Profs like me" female
w <- filter(h, gender == "f")
c <- count(w$profs_like_me >= 4)
#True 12
#False 13

#Checking correlations w/t-test and p-values 
#ability women
w <- filter(h, gender == "f")
m <- filter(h, gender == "m")
x <- w$my_ability 
y <- m$my_ability
a = t.test(x,y)
#Results
#Welch Two Sample t-test
#data:  x and y
#t = -2.4992, df = 55.72, p-value = 0.01542
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#-1.1890841 -0.1309159
#sample estimates:
#mean of x mean of y 
#3.00      3.66 

x1 <- w$my_passion 
y1 <- m$my_passion
a1 = t.test(x1,y1)
#Welch Two Sample t-test
#data:  x1 and y1
#t = -1.1299, df = 43.469, p-value = 0.2647
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.8399460  0.2366127
#sample estimates:
#  mean of x mean of y 
#3.240000  3.541667 

x2 <- w$confident_solve_problem
y2 <- m$confident_solve_problem
a2 = t.test(x2,y2)
#Welch Two Sample t-test
#data:  x2 and y2
#t = -3.6497, df = 50.243, p-value = 0.0006247
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.6432809 -0.4767191
#sample estimates:
#  mean of x mean of y 
#3.12      4.18