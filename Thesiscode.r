#TO DO:
# get names of authors into lists by conference
#for each conference:
# read through each list of authors and find their gender in the gender lists 
# - create counts of women, men, and N/A, graph line
#graph fraction of women?

library(jsonlite)
library(dplyr)
library(stringr)
library(stringr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ltm)
library(data.table)

#NOTES
#is_over_80 <- diabetes$age >= 80
#which_over_80 <- which(is_over_80)

#Colors!
COLORS = rep(c("red","orange","yellow","green","blue","purple","violet"),8)
#set location
setwd("~/Desktop/Thesis/CODE")

#Read in and combine gender data
#how to read csv: data <- read.csv("<filename>")
gender_data <- read.csv("persons.csv", na.strings = "")
gender_map <- data.frame(name=gender_data$name, gender = gender_data$gender)
gender_map <- unique(gender_map)
row.names(gender_map) <- tolower(gender_map$name) #<- uncomment after removing duplicates
#gender_data['Abadi, Martin',]$gender

#create dataframe of papers and topics
#confs1 <- unlist(conferences, recursive = F)$"papers"
#cd = unlist(conferences, recursive = F)
#a <- cd[names(cd) == "papers"]
#
#confs2 <- data.frame(confs1$key,confs1$topics)
#w <- lapply(modelset, function (x) x["papers"])

#useless junk
# confpapers <- lapply(conferences, function(c) c$papers)
# 
# map_df(unlist(conferences, recursive = F), function(c) c$papers)
# papersdf <- lapply(confpapers, function(c) c$topics)
# topics <- unlist(papersdf, recursive = F)
# papersdf <- lapply(confpapers, function(c) c$key)
# keys=unlist(papersdf)
# keys <- confpapers$key
# #topics=confpapers$topics)
# topics=unlist(papersdf, recursive=F)
# t <- lapply(topics, function(x) paste0(x,collapse = ","))
# dfp <- data.frame(keys = keys, topics = t)
# dfp <- data.frame(keys = keys, topics = unlist(t))
# dfp <- data.frame(keys = keys, topics = strsplit(unlist(t),","))
# dfp <- data.frame(keys = keys, topics = unlist(t))
# dfp$ts <= strsplit(dfp$topics, ",")
# str(dfp$topics)
# dfp <- data.frame(keys = keys, topics = strsplit(as.character(unlist(t)),","))
# as.character(dfp$topics)
# dfp$tts <- strsplit(as.character(dfp$topics), ",")
# unnest(dfp)


#read in topic info
topics <- read.csv("~/Desktop/Thesis/CODE/topics.csv", colClasses = c("factor", "character", "character", "character"))
row.names(topics) <- topics$tag
topic_list = list(topics$tag)

#ACTUAL NICE USEFUL CODE BELOW HERE

#reset location
setwd("~/Desktop/Thesis/CODE/confs")

#make list of conference names (w/.json) and conference files
conf_files <- list.files("~/Desktop/Thesis/CODE/confs/")
conf_names <- gsub(".json", "",conf_files)
conferences <- lapply(lapply(conf_files,read_file),fromJSON)
listed_confs <- unlist(conferences, recursive=FALSE,use.names = TRUE)

#Actual code for subtopic graph!!!

#create dataframe separated by papers (w/duplicate author rows for each paper they wrote)
tmp <- gender_data %>% filter(!is.na(papers))
tmp$papers <- as.character(tmp$papers) 
tmp$plist <- strsplit(tmp$papers,",")
tmp2 <- unnest(tmp)
tmp3 <- data.frame(name = tmp2$name,gender = tmp2$gender,key = tmp2$plist)
tmp3$key <- as.character(tmp3$key)

#create dataframe of papers and topics (one for each topic w/duplicate papers)
unlisted_confs <- unlist(conferences, recursive = F)
conf_paper_dfs <- unlisted_confs[names(unlisted_confs) == "papers"]
paperdf <- rbindlist(conf_paper_dfs,fill=T)
paperdf <- dplyr::select(paperdf,-content_tags)
paperdf <- dplyr::select(paperdf,key,topics)
paperdf <- paperdf[lapply(paperdf$topics,length)>0]
paperdf <- unnest(paperdf)

#dataframe with names, gender, paper, topic ready for calcs/graphing
subtopic_df <- right_join(tmp3,paperdf)
subtopic_df$topics <- as.factor(subtopic_df$topics)
subtopic_df <- group_by(subtopic_df,topics)
count(subtopic_df, gender == "F")
%>% count(subtopic_df$gender == "F")/count()
)          
#group by topic
#calc percent women for each

#CODE FROM EITAN NEEDS TO BE MODIFIED
#what is toplevel?????
#sys_confs? How to initialize data.Rmd (w/sys_conf and topics)???

#convert confs to dataframe
conf_data <- list2df(conferences)
d <- lapply(conferences, function(x) data.frame(name=x["key"],db=x["double_blind"],npapers=nrow(x$papers)))
d <- do.call("rbind",d)
row.names(d) <- gsub("_17", "",d$key)


topic_counts <- function(conf) {
  papers <- jsonlite::fromJSON(txt="~/Desktop/Thesis/CODE/confs/", conf, ".json")$papers
  df <- data.frame(row.names = papers$key)
  df$conf <- conf
  df <- cbind(df, mtabulate(papers$topics))
  df
}

### Read in all 50 conferences and create a joined matrix of topic ocurrence:
# Assumes sys_confs and topics were previously initialized in data.Rmd
sys_conf <- filter(conferences,conferences$field == "systems")
all_topic_counts <- gsub('_\\d\\d', '', sys_confs$conf) %>% map_df(topic_counts)
all_topic_counts[is.na(all_topic_counts)] <- 0
all_topic_counts <- all_topic_counts[, order(names(all_topic_counts))]   # Sort columns alphabetically
overall_counts <- data.frame(Count=colSums(select(all_topic_counts, -"conf")))
overall_counts$Topic <- row.names(overall_counts)

ggplot(overall_counts, aes(reorder(Topic, -Count), Count, fill = Topic)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Topic") +
  scale_fill_manual(breaks = as.character(topics$tag), values = topics$color) +
  guides(fill = FALSE)

#END EITAN CODE


#finds number of papers accepted in each conference
d <- lapply(conferences, function(x) data.frame(name=x["key"],db=x["double_blind"],npapers=nrow(x$papers)))
d <- do.call("rbind",d)
row.names(d) <- gsub("_17", "",d$key)

#% women by paper topic graph code

#for each topic: 
#for each conference:
#grab all authors who are listed on a
#paper w/that topic


#Function that takes a list of  <last first (Institution)> formatted names and returns the percentage of women in it
name_list_to_percent_women <- function(list_of_names) {
  names = unlist(list_of_names)
  no_inst_names = lapply(names, function(name) {gsub("(.*) \\(.*\\)$", "\\1", name)})
  last_first_names = lapply(no_inst_names, function(name) {tolower(gsub("(.*) (.*)$", "\\2, \\1", name))})
  #fixed_names = list(last_first_names)
  total <- length(last_first_names)
  if (total == 0) {
    return(NA)
  }
  #print(unlist(lapply(last_first_names, function(p) {gender_data[p,]$gender})))
  women <- sum(unlist(lapply(last_first_names, function(person) {tolower(gender_map[person,]$gender) =="f"})), na.rm=TRUE)
  percent_women = women/total
  return(percent_women)
}

#Function that takes a conference category string (e.g. "pc_chairs") and a plot label (e.g. "Percent Women PC Chairs") 
#and creates a plot of the thing
plot_the_thing <- function(category,label,func = (function(ls) {ls[category]})) {
  people <- lapply(conferences,func)
  #people <- lapply(peoples, function(ls) {ls[1]})
  names(people) <- conf_names
  percents <- list(lapply(people, name_list_to_percent_women))
  df_pct <- data.frame(confs = names(people), percent = unlist(percents))

  g <- ggplot(df_pct, aes(reorder(confs,percent), percent))
  g + geom_bar(stat="identity",fill="slateblue")+coord_flip()+xlab("Conference")+ylab(label)
  #g + scale_fill_brewer(palette = "") #choose palette here!!! choose divergent 
}

#function for percentage category correlated to diversity effort
cor_the_thing <- function(category, fun = (function(ls) {ls[category]})) {
  people <- lapply(conferences,fun)
  diversity_effort <<- unlist(lapply(conferences, (function(ls) {ls["diversity_effort"]})))
  names(people) <- conf_names
  percents <- list(lapply(people, name_list_to_percent_women))
  df_pct <<- data.frame(confs = names(people), percent = unlist(percents), diversity_effort)
  df_pct[is.na(df_pct)] <- 0
  correl <<- cor(df_pct$percent,df_pct$diversity_effort)
}

#Correlation between percent authors and diversity effort
cor_the_thing("authors",(function(ls) {ls$papers["authors"]}))
#result: -0.05199191

#Correlation between percent pc_members and diversity effort
cor_the_thing("pc_members")
#result: 0.223366

#Correlation between percent first authors and diversity effort
cor_the_thing("authors", function(conf) {unlist(lapply(conf$papers$authors, function (ls) { ls[1] }))})
#result: -0.0428815

#Correlation between percent keynote speakers and diversity effort
cor_the_thing("keynote_speakers")
#result: 0.1703789

#Correlation between percent session chairs and diversity effort
cor_the_thing("session_chairs")
#result: 0.2619387

#Correlation between percent panelists and diversity effort
cor_the_thing("panelists")
#result: 0.09403957 

#diff between author and first author percentages 
#FINISH THIS
As <- lapply(conferences, (function(ls) {ls$papers["authors"]}))
FAs <- lapply(conferences, function(conf) {unlist(lapply(conf$papers$authors, function (ls) { ls[1] }))} )
diversity_effort <<- unlist(lapply(conferences, (function(ls) {ls["diversity_effort"]})))
names(As) <- conf_names
names(FAs) <- conf_names
Apercents <- list(lapply(As, name_list_to_percent_women))
FApercents <- list(lapply(FAs, name_list_to_percent_women))
df_AFA <- data.frame(confs = names(As), p_authors = unlist(Apercents), p_first_authors = unlist(FApercents))
AFAtest <- t.test(df_AFA$p_authors,df_AFA$p_first_authors)

#mean authors: 0.1024
#mean first authors: 0.1058
#No diff between these means! 

#AUTHOR GRAPHER (uncomment line below to run)
#plot_the_thing("authors","Percent Women Authors",(function(ls) {ls$papers["authors"]}))

#PC MEMBER GRAPHER (uncomment line below to run)
#plot_the_thing("pc_members","Percent Women PC Members")

#PC CHAIR GRAPHER (uncomment line below to run)
#plot_the_thing("pc_chairs","Percent Women PC Chairs")

#SESSION CHAIR GRAPHER (uncomment line below to run)
#plot_the_thing("session_chairs","Percent Women Session Chairs")

#KEYNOTE SPEAKER GRAPHER (uncomment line below to run)
#plot_the_thing("keynote_speakers","Percent Women Keynote Speakers")

#PANELIST GRAPHER (uncomment line below to run)
#plot_the_thing("panelists","Percent Women Panelists")

#FIRST AUTHOR GRAPHER (uncomment line below to run)
#plot_the_thing("authors","Percent Women First Authors", function(conf) {unlist(lapply(conf$papers$authors, function (ls) { ls[1] }))})

#PAPERS BY GENDER
#papers_by_gender <- function(category,label,func = (function(ls) {ls$papers["authors"]})) {
#people <- unlist(lapply(conferences,function(ls) {ls$papers["authors"]}))
#names <- unlist(people)
#no_inst_names = lapply(names, function(name) {gsub("(.*) \\(.*\\)$", "\\1", name)})
#last_first_names = lapply(no_inst_names, function(name) {tolower(gsub("(.*) (.*)$", "\\2, \\1", name))})
#men_paper_numbers = c()
#women_paper_numbers = c()
#number_of_men_total = 0
#number_of_women_total = 0
#i = 1

#DPLYR R PACKAGE <- USE THIS AND BE GOOD AT R AND STUFF

#counts number of authors
filter(gender_data, as_author > 0) %>%
  count(tolower(gender) == "f")

#mean number of papers per woman
filter(gender_data, tolower(gender) == "f" & as_author > 0) %>%
  summarise(mean_papers = mean(as_author))

#mean number of papers per man
filter(gender_data, tolower(gender) == "m" & as_author > 0) %>%
  summarise(mean_papers = mean(as_author))

#t.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 0, 
#       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

#New Numbers w/right data and stuff!!
#women authors: 873
#men authors: 7050
#NA authors: 273
#men average papers:1.331773
#women average papers: 1.260023


#OLD NUMBERS FROM BROKEN CODE
#women authors: 933
#men authors: 7162
#NA authors: 116
#men average papers: 1.322815
#women average papers: 1.270096
#DO A T-test!!!!!!!!!!!

#listing women and men paper numbers to run T-test
w <- filter(gender_data, as_author > 0,tolower(gender) == "f") 
w_a <- w$as_author
m <- filter(gender_data, as_author > 0,tolower(gender) == "m") 
m_a <- m$as_author
t.test(w_a,m_a)
#Welch Two Sample t-test
#data:  w_a and m_a
#t = -2.8223, df = 1226.4, p-value = 0.004846
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.12162754 -0.02187274
#sample estimates:
#  mean of x mean of y 
#1.260023  1.331773 

#percent women authors vs pc_chairs t-test???
wa <- filter(gender_data, as_author > 0,tolower(gender) == "f") 
w_a <- wa$as_author
wp <- filter(gender_data, as_pc > 0,tolower(gender) == "f") 
w_p <- wp$as_pc
t.test(w_a,w_p) 

#percent women first author
#fa <- filter(gender_data, as_first_author > 0, tolower(gender) == "f")
plot_the_thing("authors","Percent Women First Authors", function(conf) {unlist(lapply(conf$papers$authors, function (ls) { ls[1] }))})
lapply(people, name_list_to_percent_women)

unnest_confs <- function(people, role, field) {
  df <- select(people, name, gender, country, sector)
  df$role <- role
  df$conf <- strsplit(gsub("_\\d+", "", as.character(people[[field]])), ",")
  unnest(df)
}

by_role <- data.frame()
by_role <- rbind(by_role, unnest_confs(filter(gender_data, as_pc_chair > 0), "CHAIR", "pc_chairs"))
by_role <- rbind(by_role, unnest_confs(filter(gender_data, as_pc > 0), "PC", "pcs"))
by_role <- rbind(by_role, unnest_confs(filter(gender_data, as_author > 0), "AUTHOR", "papers"))
by_role$role <- as.factor(by_role$role)
by_role$conf <- as.factor(by_role$conf)

plot_data <- na.omit(by_role) %>% group_by(conf) %>% count(role,gender) %>% group_by(conf,role) %>% mutate(pct=n/sum(n))
#p <- plot_data[order(plot_data$pct)]
pd <- filter(plot_data, gender == "F", role != "CHAIR") 
pd <- arrange(pd,desc(pct))

conf_order <- pd %>%
filter(role=="AUTHOR", gender=="F") %>%
arrange(pct)
DB <- c(FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)
conf_order["double_blind"] <- DB

#adding col of numbers of papers accepted to pd
pd$num <- d[pd$conf,]$npapers
pd$dbn <- d[pd$conf,]$double_blind

pd$sorted_conf <- factor(pd$conf, levels=conf_order$conf)
pd$double_blind <- ifelse(which(sorted_conf, conf)) 
pd$db <- unlist(lapply(pd$conf, function(c) { DB[57-which(conf_order$conf==c)]}))
#MAKE MANUAL LIST OF BOOLEANS FOR WHETHER CONF IS DOUBLE BLIND)
#check a few cases manually to verify
#make double blind ones textured? or a different color?
#plot background empty black outlined box on top of double-blind ones

#does the thing! (needs double-blind status noted)

pd$num[pd$role == "PC"] <- 0
pd$dbn[pd$role == "PC"] <- FALSE
pd$dbn <- factor(pd$dbn, levels = c(TRUE,FALSE))
d = rep(c('bold','plain'),28)
pd$num_include <- which(pd$num, pd$num != 0)

ggplot(pd,aes(x=conf))+aes(x=sorted_conf, y=pct , fill=factor(role,labels = c("Percent Women Authors","Percent Women on Program Committee")), color = pd$dbn)+
theme(legend.title = element_blank())+theme(legend.position = c(.85,0.13))+theme(legend.direction = "vertical")+
geom_bar(stat="identity",position = "dodge")+ylab("Percent Women in Role")+xlab("Conference")+coord_flip()+
geom_text(label = pd$num, size = 3,nudge_y = 0.02, color = ifelse(pd$num != 0,"black","gray90")) + scale_color_manual(pd$dbn,values = c("black",'gray90'), guide = FALSE)
#+theme(axis.text.x = element_text(face = d))
#  facet_wrap(pd$dbn)+

sd <- filter(pd,role == "AUTHOR")
dban <- filter(sd, dbn == TRUE)
#percent ==

sb <- filter(pd,role == "AUTHOR")
sban <- filter(sb,dbn == FALSE)
  
a = filter(pd, role=="AUTHOR")
cor(a$pct,a$dbn)

a$double=as.numeric(a$dbn)
cor(a$pct,a$double)

#total percentage of women across conferences w/double-blind: 10.1% 
#total percentage of women across conferences w/single-blind: 11%

#g + geom_bar(stat = "identity",position = "dodge",colour = ifelse(pd$db,"black","gray"))
#g + geom_bar(stat = "identity",position = "dodge",aes(fill = pd$db),alpha=0, size=1, color="black")+scale_color_manual(scale_size_manual(values=c(0.5, 1),guide = "none")
#geom_bar(aes(x = sorted_conf, fill=sorted_conf, size=db), color="black") +scale_size_manual(values=c(0.5, 1), guide = "none")+coord_flip()#+facet_wrap(~ cut)
#g + geom_bar(data=pd[(pd$db=="TRUE"),],aes(db), alpha=0, size=1, color="black") #+ facet_wrap(~ cut)

#compute correlation:
#v2 <- filter(pd, role="PC",gender=="F)$pct
#v1 <- filter(plot_data,role=="AUTHOR",gender=="F")$pct

#gender_data, aes(x = conf, y = )) + geom_bar(stat="identity",fill="slateblue")+coord_flip()+xlab("Conference")+ylab(label)

#n <- filter(gender_data, aes(x = conf))+geom_bar(aes(y = (..count..)/sum(..count..)))+scale_y_continuous

