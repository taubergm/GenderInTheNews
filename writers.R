if (!require(wordcloud)) {
  install.packages("wordcloud", repos="http://cran.us.r-project.org")
}
if (!require(tm)) {
  install.packages("tm", repos="http://cran.us.r-project.org")
}
if (!require(slam)) {
  install.packages("slam", repos="http://cran.us.r-project.org")
}
if (!require(SnowballC)) {
  install.packages("SnowballC", repos="http://cran.us.r-project.org")
}
if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}
if (!require(xts)) {
  install.packages("xts", repos="http://cran.us.r-project.org")
}
if (!require(textclean)) {
  install.packages("textclean", repos="http://cran.us.r-project.org")
}
if (!require(tidytext)) {
  install.packages("tidytext", repos="http://cran.us.r-project.org")
}
library(slam)
library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)
library(xts)
library(textclean)
library(tidytext)
library(syuzhet)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(reshape2)

sources = c( "politico", "cnn", "npr",
         "nytimes", "wsj", "latimes", "usatoday", "washingtonpost", "bostonglobe",
         "foxnews", "dailycaller", "infowars", "breitbart", "guardian",
         "slate", "businessinsider"   )

# fix buzzfeed

workingDir = '/Users/michaeltauberg/projects/NewsScraper'
setwd(workingDir)
data_name = "writers"
#csv_name = "writers_all_to_july2.csv"
#writers = read.csv(file=csv_name)
#writers = scan(csv_name, what="", sep="\n")
#writers = unique(writers)

all_writers_gender = read.csv("all_writers_gender_outlet3.csv")
all_writers_gender = all_writers_gender[!duplicated(all_writers_gender[,c('writer')], fromLast=FALSE),]
#all_writers_gender = all_writers_gender[!grepl("vox", all_writers_gender$last_url),]
#all_writers_gender = all_writers_gender[!grepl("bbc", all_writers_gender$last_url),]

buzzfeed = all_writers_gender[grepl("buzzfeed", all_writers_gender$last_url),]
cnn = all_writers_gender[grepl("cnn", all_writers_gender$last_url),]
nytimes = all_writers_gender[grepl("nytimes", all_writers_gender$last_url),]
foxnews = all_writers_gender[grepl("foxnews", all_writers_gender$last_url),]
usa = all_writers_gender[grepl("usatoday", all_writers_gender$last_url),]

male = all_writers_gender[all_writers_gender$gender == "male", ]
female = all_writers_gender[all_writers_gender$gender == "female", ]
unknown = all_writers_gender[all_writers_gender$gender == "unknown", ]

male_writers = nrow(male)
female_writers= nrow(female)
unknown_writers = nrow(unknown)

male_stories = sum(male$num_articles)
female_stories = sum(female$num_articles)
unknown_stories = sum(unknown$num_articles)

###################
# make a pie chart 
################
data_name = "writer_gender"
gender_palette <- c("pink","#81FFF4","#C8CFCE")
writers_gender = c(female_writers, male_writers, unknown_writers)
writers_gender = as.data.frame(writers_gender)
writers_gender$group = c("female", "male", "unknown")
p = ggplot(writers_gender, aes(x="", y=writers_gender, fill=group)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) + scale_fill_manual(values=gender_palette) 
p = p  + theme_void() 
p = p + ggtitle(sprintf("Gender of News Writers in 2019", data_name))
ggsave(filename = sprintf("./2019_%s_piechart.png", data_name), plot=p, width=6, height=6)

###################
# make  waffle charts
###################
vals = c(male_writers, female_writers, unknown_writers)/100
val_names  =  sprintf("%s (%s)", c("Male", "Female", "Unknown"), scales::percent(round(vals/sum(vals), 2)))
names(vals)  =  val_names

waffle::waffle(vals,
               colors = c("light blue", "pink", "grey"), 
               title = "Every Hundred News Writers in 2019 by Gender")
ggthemes::scale_fill_tableau(name=NULL)

vals = c(female_stories, male_stories, unknown_stories)/1000
val_names  =  sprintf("%s (%s)", c("Female", "Male", "Unknown"), scales::percent(round(vals/sum(vals), 2)))
names(vals)  =  val_names

waffle::waffle(vals,
               colors = c("pink","light blue","grey"), 
               title = "Every Thousand News Stories in 2019 by Gender")
ggthemes::scale_fill_tableau(name=NULL)



#####################################
## GENDER SPLIT OF WRITERS BY SOURCE
#####################################
gender_source_writers = data.frame(source=rep("", length(sources)), 
                                    num_male_writers=rep(0, length(sources)), 
                                    num_female_writers=rep(0, length(sources)), 
                                    num_unknown_writers=rep(0, length(sources)),
                                    total_writers=rep(0, length(sources)),
                                    percent_male_writers=rep(0, length(sources)),
                                    percent_female_writers=rep(0, length(sources)),
                                    percent_unknown_writers=rep(0, length(sources)),
                                    stringsAsFactors=FALSE) 

#####################################
## GENDER SPLIT OF BY NUM ARTICLES BY SOURCE
#####################################
gender_source_articles = data.frame(source=rep("", length(sources)), 
                                    num_male_articles=rep(0, length(sources)), 
                                    num_female_articles=rep(0, length(sources)), 
                                    num_unknown_articles=rep(0, length(sources)),
                                    total_articles=rep(0, length(sources)),
                                    percent_male_articles=rep(0, length(sources)),
                                    percent_female_articles=rep(0, length(sources)),
                                    percent_unknown_articles=rep(0, length(sources)),
                                    stringsAsFactors=FALSE) 

#####################################
## collect stats
#####################################
i = 1
for (source in sources) {
  print(source)
  source_writers = all_writers_gender[grep(source, all_writers_gender$last_url),]
  male_writers = source_writers[source_writers$gender == "male", ]
  female_writers = source_writers[source_writers$gender == "female", ]
  unknown_writers = source_writers[source_writers$gender == "unknown", ]
  gender_source_writers[i,] = c(source, nrow(male_writers), nrow(female_writers), nrow(unknown_writers), 
                                nrow(source_writers), nrow(male_writers)/nrow(source_writers),  nrow(female_writers)/nrow(source_writers),
                                nrow(unknown_writers)/nrow(source_writers))
  gender_source_articles[i,] = c(source, sum(male_writers$num_articles), sum(female_writers$num_articles), sum(unknown_writers$num_articles),
                                 sum(source_writers$num_articles), sum(male_writers$num_articles)/sum(source_writers$num_articles),  
                                 sum(female_writers$num_articles)/sum(source_writers$num_articles),
                                 sum(unknown_writers$num_articles)/sum(source_writers$num_articles))
  i = i + 1
}
gender_source_articles$percent_male_articles=as.numeric(as.character(gender_source_articles$percent_male_articles))
gender_source_articles$percent_female_articles=as.numeric(as.character(gender_source_articles$percent_female_articles))
gender_source_writers$percent_male_writers=as.numeric(as.character(gender_source_writers$percent_male_writers))
gender_source_writers$percent_female_writers=as.numeric(as.character(gender_source_writers$percent_female_writers))

gender_source_writers$source = factor(gender_source_writers$source, levels = gender_source_writers$source[order(gender_source_writers$percent_female_writers, decreasing=FALSE)])
gender_source_articles$source = factor(gender_source_articles$source, levels = gender_source_articles$source[order(gender_source_articles$percent_female_articles, decreasing=FALSE)])

#####################################
## create single plots
#####################################
data_name = "percent_female_writers"
p = ggplot(gender_source_writers, aes(x=source, y=percent_female_writers, fill="#003b9b")) + geom_bar(stat="identity") 
p = p + ggtitle("percent female writers")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=14,face="bold"))
p = p + xlab("percent") + guides(fill=FALSE)
p = p + coord_flip()
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=8, height=6)

data_name = "percent_male_writers"
gender_source_writers$source = factor(gender_source_writers$source, levels = gender_source_writers$source[order(gender_source_writers$percent_male_writers, decreasing=TRUE)])
p = ggplot(gender_source_writers, aes(x=source, y=percent_male_writers, fill="#003b9b")) + geom_bar(stat="identity") 
p = p + ggtitle("percent male writers")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=14,face="bold"))
p = p + xlab("percent") + guides(fill=FALSE)
p = p + coord_flip()
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=8, height=6)

data_name = "percent_female_articles"
p = ggplot(gender_source_articles, aes(x=source, y=percent_female_articles, fill="#003b9b")) + geom_bar(stat="identity") 
p = p + ggtitle("percent female articles")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=14,face="bold"))
p = p + xlab("percent") + guides(fill=FALSE)
p = p + coord_flip()
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=8, height=6)

data_name = "percent_male_articles"
gender_source_articles$source = factor(gender_source_articles$source, levels = gender_source_articles$source[order(gender_source_articles$percent_male_articles, decreasing=TRUE)])
p = ggplot(gender_source_articles, aes(x=source, y=percent_male_articles, fill="#003b9b")) + geom_bar(stat="identity") 
p = p + ggtitle("percent male articles")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=14,face="bold"))
p = p + xlab("percent") + guides(fill=FALSE)
p = p + coord_flip()
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=8, height=6)

#####################################
## create split plots
#http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-4-stacked-bar-plots.html
#####################################
########
# articles
#########
gender_source_articles$total_articles = as.integer(gender_source_articles$total_articles)
gender_source_articles = gender_source_articles[gender_source_articles$total_articles > 150,]
genders = melt(gender_source_articles, id=c("source"))
genders = genders[grep("percent", genders$variable), ]
genders$value = as.numeric(genders$value) * 100
data_name = "gender_composition_articles"
genders = ddply(genders, .(variable), transform, pos = value)
p = ggplot() + geom_bar(aes(y = value, x = source, fill = variable), data = genders, stat="identity")
p = p + coord_flip()
p = p + labs(y="Percent", x="News Source")
p = p + ggtitle("Composition of Major News Sites By Gender (articles)")
fill = c("#81FFF4", "pink","#C8CFCE")
p = p + scale_fill_manual(values=fill)
#p = p + theme_fivethirtyeight() 
genders$value = round(gender$value)
p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.border = element_blank(), panel.background = element_blank())

ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=8, height=6)


#########
# writers
#########
gender_source_writers$total_writers = as.integer(gender_source_writers$total_writers)
gender_source_writers = gender_source_writers[gender_source_writers$total_writers > 90,]
genders = melt(gender_source_writers, id=c("source"))
genders = genders[grep("percent", genders$variable), ]
genders$value = as.numeric(genders$value) * 100
data_name = "gender_composition_writers"


p = ggplot() + geom_bar(aes(y = value, x = source, fill = variable), data = genders,
                        stat="identity")
p = p + coord_flip()
p = p + labs(y="Percent", x="News Source")
p = p + ggtitle("Composition of Gender of Writers from Major News Sources (%)")
fill = c("#81FFF4", "pink","#C8CFCE")
p = p + scale_fill_manual(values=fill)
#p = p + theme_fivethirtyeight() 
p = p + theme(
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_blank(), panel.background = element_blank())
#p = p + geom_text(data=genders, aes(x=source, y=pos, label = paste0(value,"%")), size=2)
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=8, height=6)



###########
# Now remove unknown and redo
###########

####
# writers
######
gender_source_writers$percent_unknown_writers = NULL
gender_source_writers$num_male_writers = as.numeric(gender_source_writers$num_male_writer)
gender_source_writers$num_female_writers = as.numeric(gender_source_writers$num_female_writer)
gender_source_writers$percent_male_writers = gender_source_writers$num_male_writers/(gender_source_writers$num_male_writers+gender_source_writers$num_female_writers)
gender_source_writers$percent_female_writers = 1 - gender_source_writers$percent_male_writers
genders = melt(gender_source_writers, id=c("source"))
genders = genders[grep("percent", genders$variable), ]
genders$value = as.numeric(genders$value) * 100
data_name = "gender_composition_writers_nounknown"
#genders = ddply(genders, .(variable), transform, pos = 0.75*value)
genders$pos = 10
genders[genders$variable == "percent_male_writers",]$pos = 90
genders$value = round(genders$value )
p = ggplot() + geom_bar(aes(y = value, x = source, fill = variable), data = genders,
                        stat="identity")
p = p + coord_flip()
p = p + labs(y="Percent", x="News Source")
p = p + ggtitle("Composition of Major News Sites By Gender (writers) Unknown gender removed")
fill = c("#81FFF4", "pink","#C8CFCE")
p = p + scale_fill_manual(values=fill)
#p = p + theme_fivethirtyeight() 
p = p + theme(
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_blank(), panel.background = element_blank())
p = p + geom_text(data=genders, aes(x=source, y=pos, label = paste0(value,"%")), size=2)

ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=8, height=6)

####
## articles
#####
gender_source_articles$percent_unknown_articles = NULL
gender_source_articles$num_male_articles = as.numeric(gender_source_articles$num_male_writer)
gender_source_articles$num_female_articles = as.numeric(gender_source_articles$num_female_writer)
gender_source_articles$percent_male_articles = gender_source_articles$num_male_articles/(gender_source_articles$num_male_articles+gender_source_articles$num_female_articles)
gender_source_articles$percent_female_articles = 1 - gender_source_articles$percent_male_articles
genders = melt(gender_source_articles, id=c("source"))
genders = genders[grep("percent", genders$variable), ]
genders$value = as.numeric(genders$value) * 100
data_name = "gender_composition_articles_nounknown"
#genders = ddply(genders, .(variable), transform, pos = 0.75*value)
genders$pos = 10
genders[genders$variable == "percent_male_articles",]$pos = 90
genders$value = round(genders$value )
p = ggplot() + geom_bar(aes(y = value, x = source, fill = variable), data = genders,
                        stat="identity")
p = p + coord_flip()
p = p + labs(y="Percent", x="News Source")
p = p + ggtitle("Composition of Major News Sites By Gender (articles) Unknown gender removed")
fill = c("#81FFF4", "pink","#C8CFCE")
p = p + scale_fill_manual(values=fill)
#p = p + theme_fivethirtyeight() 
p = p + theme(
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_blank(), panel.background = element_blank())
p = p + geom_text(data=genders, aes(x=source, y=pos, label = paste0(value,"%")), size=2)

ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=8, height=6)



#####################################
## MOST PROLIFIC WRITERS
#####################################
all_writers_gender = all_writers_gender[order(all_writers_gender$num_articles, decreasing=TRUE),]
unknown = unknown[order(unknown$num_articles, decreasing=TRUE),]
top_writers = all_writers_gender[all_writers_gender$num_articles > 20,]



