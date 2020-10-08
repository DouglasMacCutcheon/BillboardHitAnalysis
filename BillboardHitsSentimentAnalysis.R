#setwd("C:\\Users\\currys\\Google Drive\\R stuff\\BillboardDecadeData")
setwd("C:\\Users\\dosman\\Google Drive\\R stuff\\BillboardDecadeData")
install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("broom")
install.packages("ez")
install.packages("plyr")
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(broom)
library(ez)
library(plyr)


# Datafile 1996
DataBillboard1996 <- read.csv(file="DataBillboard100_end1996.csv",header=F,sep=";")
DataBillboard1996 <- DataBillboard1996[1]
colnames(DataBillboard1996) <- "word"

# Datafile 2006
DataBillboard2006 <- read.csv(file="DataBillboard100_end2006.csv",header=F,sep=";")
DataBillboard2006 <- DataBillboard2006[1]
colnames(DataBillboard2006) <- "word"

# Datafile 2008
DataBillboard2008<- read.csv(file="DataBillboard100_end2008.csv",header=F,sep=";")
DataBillboard2008 <- DataBillboard2008[1]
colnames(DataBillboard2008) <- "word"

# Datafile 2009
DataBillboard2009 <- read.csv(file="DataBillboard100_end2009.csv",header=F,sep=";")
DataBillboard2009 <- DataBillboard2009[1]
colnames(DataBillboard2009) <- "word"

# Datafile 2010
DataBillboard2010 <- read.csv(file="DataBillboard100_end2010.csv",header=F,sep=";")
DataBillboard2010 <- DataBillboard2010[1]
colnames(DataBillboard2010) <- "word"

# Datafile 2011
DataBillboard2011 <- read.csv(file="DataBillboard100_end2011.csv",header=F,sep=";")
DataBillboard2011 <- DataBillboard2011[1]
colnames(DataBillboard2011) <- "word"

# Datafile 2012
DataBillboard2012 <- read.csv(file="DataBillboard100_end2012.csv",header=F,sep=";")
DataBillboard2012 <- DataBillboard2012[1]
colnames(DataBillboard2012) <- "word"

# Datafile 2013
DataBillboard2013 <- read.csv(file="DataBillboard100_end2013.csv",header=F,sep=";")
DataBillboard2013 <- DataBillboard2013[1]
colnames(DataBillboard2013) <- "word"

# Datafile 2014
DataBillboard2014 <- read.csv(file="DataBillboard100_end2014.csv",header=F,sep=";")
DataBillboard2014 <- DataBillboard2014[1]
colnames(DataBillboard2014) <- "word"

# Datafile 2015
DataBillboard2015 <- read.csv(file="DataBillboard100_end2015.csv",header=F,sep=";")
DataBillboard2015 <- DataBillboard2015[1]
colnames(DataBillboard2015) <- "word"

 # Datafile 2016
DataBillboard2016 <- read.csv(file="DataBillboard100_end2016.csv",header=F,sep=";")
DataBillboard2016 <- DataBillboard2016[1]
colnames(DataBillboard2016) <- "word"

# Datafile 2017
DataBillboard2017 <- read.csv(file="DataBillboard100_end2017.csv",header=F,sep=";")
DataBillboard2017 <- DataBillboard2017[1]
colnames(DataBillboard2017) <- "word"




# word frequencies 2016 and 2006
word_freq_1996 <- DataBillboard1996 %>% count(word, sort = TRUE)
word_freq_2006 <- DataBillboard2006 %>% count(word, sort = TRUE)
word_freq_2010 <- DataBillboard2010 %>% count(word, sort = TRUE)
word_freq_2011 <- DataBillboard2011 %>% count(word, sort = TRUE)
word_freq_2012 <- DataBillboard2012 %>% count(word, sort = TRUE)
word_freq_2013 <- DataBillboard2013 %>% count(word, sort = TRUE)
word_freq_2014 <- DataBillboard2014 %>% count(word, sort = TRUE)
word_freq_2015 <- DataBillboard2015 %>% count(word, sort = TRUE)
word_freq_2016 <- DataBillboard2016 %>% count(word, sort = TRUE)
word_freq_2017 <- DataBillboard2017 %>% count(word, sort = TRUE)



# NRC#########################################################

# sentiment analysis - negative
nrcneg <- get_sentiments("nrc") %>% filter(sentiment == "negative")
nrc_1996_neg <- inner_join(nrcneg,DataBillboard1996) %>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
nrc_2006_neg <- inner_join(nrcneg,DataBillboard2006) %>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
nrc_2008_neg <- inner_join(nrcneg,DataBillboard2008) %>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
nrc_2009_neg <- inner_join(nrcneg,DataBillboard2009) %>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
nrc_2010_neg <- inner_join(nrcneg,DataBillboard2010) %>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
nrc_2011_neg <- inner_join(nrcneg,DataBillboard2011) %>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
nrc_2012_neg <- inner_join(nrcneg,DataBillboard2012) %>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
nrc_2013_neg <- inner_join(nrcneg,DataBillboard2013) %>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
nrc_2014_neg <- inner_join(nrcneg,DataBillboard2014) %>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
nrc_2015_neg <- inner_join(nrcneg,DataBillboard2015) %>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
nrc_2016_neg <- inner_join(nrcneg,DataBillboard2016) %>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
nrc_2017_neg <- inner_join(nrcneg,DataBillboard2017) %>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
sum(nrc_1996_neg$n)
sum(nrc_2006_neg$n)
sum(nrc_2008_neg$n)
sum(nrc_2009_neg$n)
sum(nrc_2010_neg$n)
sum(nrc_2011_neg$n)
sum(nrc_2012_neg$n)
sum(nrc_2013_neg$n)
sum(nrc_2014_neg$n)
sum(nrc_2015_neg$n)
sum(nrc_2016_neg$n)
sum(nrc_2017_neg$n)

# sentiment analysis - positive
nrcpos <- get_sentiments("nrc") %>% filter(sentiment == "positive")
nrc_1996_pos <- inner_join(nrcpos,DataBillboard1996) %>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)
nrc_2006_pos <- inner_join(nrcpos,DataBillboard2006) %>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)
nrc_2008_pos <- inner_join(nrcpos,DataBillboard2008) %>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)
nrc_2009_pos <- inner_join(nrcpos,DataBillboard2009) %>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)
nrc_2010_pos <- inner_join(nrcpos,DataBillboard2010) %>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)
nrc_2011_pos <- inner_join(nrcpos,DataBillboard2011) %>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)
nrc_2012_pos <- inner_join(nrcpos,DataBillboard2012) %>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)
nrc_2013_pos <- inner_join(nrcpos,DataBillboard2013) %>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)
nrc_2014_pos <- inner_join(nrcpos,DataBillboard2014) %>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)
nrc_2015_pos <- inner_join(nrcpos,DataBillboard2015) %>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)
nrc_2016_pos <- inner_join(nrcpos,DataBillboard2016) %>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)
nrc_2017_pos <- inner_join(nrcpos,DataBillboard2017) %>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)

data_nrc_words_nrc <- data.frame(year = c(#1996,
 2006,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017), 
                                  pos_words = c(#sum(nrc_1996_pos$n), 
                                                sum(nrc_2006_pos$n),
                                                sum(nrc_2008_pos$n),
                                                sum(nrc_2009_pos$n),
                                                sum(nrc_2010_pos$n),
                                                sum(nrc_2011_pos$n),
                                                sum(nrc_2012_pos$n),
                                                sum(nrc_2013_pos$n),
                                                sum(nrc_2014_pos$n),
                                                sum(nrc_2015_pos$n),
                                                sum(nrc_2016_pos$n),
                                                sum(nrc_2017_pos$n)),
                                  neg_words = c(#sum(nrc_1996_neg$n),
                                                sum(nrc_2006_neg$n),
                                                sum(nrc_2008_neg$n),
                                                sum(nrc_2009_neg$n),
                                                sum(nrc_2010_neg$n),
                                                sum(nrc_2011_neg$n),
                                                sum(nrc_2012_neg$n),
                                                sum(nrc_2013_neg$n),
                                                sum(nrc_2014_neg$n),
                                                sum(nrc_2015_neg$n),
                                                sum(nrc_2016_neg$n),
                                                sum(nrc_2017_neg$n)))

# Correlation between positive and negative words
cor.test(data_nrc_words_nrc$neg_words, data_nrc_words_nrc$year, method ="pearson")                                 

# Correlation plot
correlation_neg_words <- 
  
ggplot(data_nrc_words_nrc, aes(x=year, y=neg_words)) + 
  stat_summary(fun.y=mean, geom="point")  +
  geom_smooth(method="lm", se=FALSE, color = "black") +
  theme_bw() +                  
  expand_limits(y=.5) +
  ylab("Total negative words (per year)") +
  xlab("Year") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=25, colour = "black"),
        axis.text.y = element_text(size=25, colour = "black"),
        axis.title.x = element_text(size=25, colour = "black"),
        axis.text.x = element_text(size=25, colour = "black")) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=.8)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(2000, 2020)) +
  coord_cartesian(ylim = c(1250, 2000)) + 
  scale_x_continuous(breaks=c(2006,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  annotate("text", x = 2008, y = 1800, label = "italic(r) == 0.54", parse = TRUE, size = 5) 


# Save  
png(filename="correlation_neg_words.png", 
    width = 7, height = 7, units = 'in', res = 300)
plot(correlation_neg_words)
dev.off()













# sentiment analysis - fear
nrcfear <- get_sentiments("nrc") %>% filter(sentiment == "fear")
nrc_1996_fear <- inner_join(nrcfear,DataBillboard1996) %>% 
  filter(sentiment == "fear") %>% count(word, sort = TRUE)
nrc_2006_fear <- inner_join(nrcfear,DataBillboard2006) %>% 
  filter(sentiment == "fear") %>% count(word, sort = TRUE)
nrc_2010_fear <- inner_join(nrcfear,DataBillboard2010) %>% 
  filter(sentiment == "fear") %>% count(word, sort = TRUE)
nrc_2011_fear <- inner_join(nrcfear,DataBillboard2011) %>% 
  filter(sentiment == "fear") %>% count(word, sort = TRUE)
nrc_2012_fear <- inner_join(nrcfear,DataBillboard2012) %>% 
  filter(sentiment == "fear") %>% count(word, sort = TRUE)
nrc_2013_fear <- inner_join(nrcfear,DataBillboard2013) %>% 
  filter(sentiment == "fear") %>% count(word, sort = TRUE)
nrc_2014_fear <- inner_join(nrcfear,DataBillboard2014) %>% 
  filter(sentiment == "fear") %>% count(word, sort = TRUE)
nrc_2015_fear <- inner_join(nrcfear,DataBillboard2015) %>% 
  filter(sentiment == "fear") %>% count(word, sort = TRUE)
nrc_2016_fear <- inner_join(nrcfear,DataBillboard2016) %>% 
  filter(sentiment == "fear") %>% count(word, sort = TRUE)
nrc_2017_fear <- inner_join(nrcfear,DataBillboard2017) %>% 
  filter(sentiment == "fear") %>% count(word, sort = TRUE)

sum(nrc_1996_fear$n)
sum(nrc_2006_fear$n)
sum(nrc_2010_fear$n)
sum(nrc_2011_fear$n)
sum(nrc_2012_fear$n)
sum(nrc_2013_fear$n)
sum(nrc_2014_fear$n)
sum(nrc_2015_fear$n)
sum(nrc_2016_fear$n)
sum(nrc_2017_fear$n)



# sentiment analysis - anger
nrcanger <- get_sentiments("nrc") %>% filter(sentiment == "anger")
nrc_1996_anger <- inner_join(nrcanger,DataBillboard1996) %>% 
  filter(sentiment == "anger") %>% count(word, sort = TRUE)
nrc_2006_anger <- inner_join(nrcanger,DataBillboard2006) %>% 
  filter(sentiment == "anger") %>% count(word, sort = TRUE)
nrc_2010_anger <- inner_join(nrcanger,DataBillboard2010) %>% 
  filter(sentiment == "anger") %>% count(word, sort = TRUE)
nrc_2011_anger <- inner_join(nrcanger,DataBillboard2011) %>% 
  filter(sentiment == "anger") %>% count(word, sort = TRUE)
nrc_2012_anger <- inner_join(nrcanger,DataBillboard2012) %>% 
  filter(sentiment == "anger") %>% count(word, sort = TRUE)
nrc_2013_anger <- inner_join(nrcanger,DataBillboard2013) %>% 
  filter(sentiment == "anger") %>% count(word, sort = TRUE)
nrc_2014_anger <- inner_join(nrcanger,DataBillboard2014) %>% 
  filter(sentiment == "anger") %>% count(word, sort = TRUE)
nrc_2015_anger <- inner_join(nrcanger,DataBillboard2015) %>% 
  filter(sentiment == "anger") %>% count(word, sort = TRUE)
nrc_2016_anger <- inner_join(nrcanger,DataBillboard2016) %>% 
  filter(sentiment == "anger") %>% count(word, sort = TRUE)
nrc_2017_anger <- inner_join(nrcanger,DataBillboard2017) %>% 
  filter(sentiment == "anger") %>% count(word, sort = TRUE)
sum(nrc_1996_anger$n)
sum(nrc_2006_anger$n)
sum(nrc_2010_anger$n)
sum(nrc_2011_anger$n)
sum(nrc_2012_anger$n)
sum(nrc_2013_anger$n)
sum(nrc_2014_anger$n)
sum(nrc_2015_anger$n)
sum(nrc_2016_anger$n)
sum(nrc_2017_anger$n)

# sentiment analysis - sadness
nrcsad <- get_sentiments("nrc") %>% filter(sentiment == "sadness")
nrc_1996_sadness <- inner_join(nrcsad,DataBillboard1996) %>% 
  filter(sentiment == "sadness") %>% count(word, sort = TRUE)
nrc_2006_sadness <- inner_join(nrcsad,DataBillboard2006) %>% 
  filter(sentiment == "sadness") %>% count(word, sort = TRUE)
nrc_2010_sadness <- inner_join(nrcsad,DataBillboard2010) %>% 
  filter(sentiment == "sadness") %>% count(word, sort = TRUE)
nrc_2011_sadness <- inner_join(nrcsad,DataBillboard2011) %>% 
  filter(sentiment == "sadness") %>% count(word, sort = TRUE)
nrc_2012_sadness <- inner_join(nrcsad,DataBillboard2012) %>% 
  filter(sentiment == "sadness") %>% count(word, sort = TRUE)
nrc_2013_sadness <- inner_join(nrcsad,DataBillboard2013) %>% 
  filter(sentiment == "sadness") %>% count(word, sort = TRUE)
nrc_2014_sadness <- inner_join(nrcsad,DataBillboard2014) %>% 
  filter(sentiment == "sadness") %>% count(word, sort = TRUE)
nrc_2015_sadness <- inner_join(nrcsad,DataBillboard2015) %>% 
  filter(sentiment == "sadness") %>% count(word, sort = TRUE)
nrc_2016_sadness <- inner_join(nrcsad,DataBillboard2016) %>% 
  filter(sentiment == "sadness") %>% count(word, sort = TRUE)
nrc_2017_sadness <- inner_join(nrcsad,DataBillboard2017) %>% 
  filter(sentiment == "sadness") %>% count(word, sort = TRUE)
sum(nrc_1996_sadness$n)
sum(nrc_2006_sadness$n)
sum(nrc_2010_sadness$n)
sum(nrc_2011_sadness$n)
sum(nrc_2012_sadness$n)
sum(nrc_2013_sadness$n)
sum(nrc_2014_sadness$n)
sum(nrc_2016_sadness$n)
sum(nrc_2017_sadness$n)

# sentiment analysis - disgust
nrcdisgust <- get_sentiments("nrc") %>% filter(sentiment == "disgust")
nrc_1996_disgust <- inner_join(nrcdisgust,DataBillboard1996) %>% 
  filter(sentiment == "disgust") %>% count(word, sort = TRUE)
nrc_2006_disgust <- inner_join(nrcdisgust,DataBillboard2006) %>% 
  filter(sentiment == "disgust") %>% count(word, sort = TRUE)
nrc_2010_disgust <- inner_join(nrcdisgust,DataBillboard2010) %>% 
  filter(sentiment == "disgust") %>% count(word, sort = TRUE)
nrc_2011_disgust <- inner_join(nrcdisgust,DataBillboard2011) %>% 
  filter(sentiment == "disgust") %>% count(word, sort = TRUE)
nrc_2013_disgust <- inner_join(nrcdisgust,DataBillboard2013) %>% 
  filter(sentiment == "disgust") %>% count(word, sort = TRUE)
nrc_2014_disgust <- inner_join(nrcdisgust,DataBillboard2014) %>% 
  filter(sentiment == "disgust") %>% count(word, sort = TRUE)
nrc_2015_disgust <- inner_join(nrcdisgust,DataBillboard2015) %>% 
  filter(sentiment == "disgust") %>% count(word, sort = TRUE)
nrc_2016_disgust <- inner_join(nrcdisgust,DataBillboard2016) %>% 
  filter(sentiment == "disgust") %>% count(word, sort = TRUE)
nrc_2017_disgust <- inner_join(nrcdisgust,DataBillboard2017) %>% 
  filter(sentiment == "disgust") %>% count(word, sort = TRUE)
sum(nrc_1996_disgust$n)
sum(nrc_2006_disgust$n)
sum(nrc_2010_disgust$n)
sum(nrc_2011_disgust$n)
sum(nrc_2012_disgust$n)
sum(nrc_2013_disgust$n)
sum(nrc_2014_disgust$n)
sum(nrc_2015_disgust$n)
sum(nrc_2016_disgust$n)
sum(nrc_2017_disgust$n)



# sentiment analysis - trust
nrctrustr <- get_sentiments("nrc") %>% filter(sentiment == "trust")
nrc_1996_trust <- inner_join(nrctrustr,DataBillboard1996) %>% 
  filter(sentiment == "trust") %>% count(word, sort = TRUE)
nrc_2006_trust <- inner_join(nrctrustr,DataBillboard2006) %>% 
  filter(sentiment == "trust") %>% count(word, sort = TRUE)
nrc_2010_trust <- inner_join(nrctrustr,DataBillboard2010) %>% 
  filter(sentiment == "trust") %>% count(word, sort = TRUE)
nrc_2011_trust <- inner_join(nrctrustr,DataBillboard2011) %>% 
  filter(sentiment == "trust") %>% count(word, sort = TRUE)
nrc_2012_trust <- inner_join(nrctrustr,DataBillboard2012) %>% 
  filter(sentiment == "trust") %>% count(word, sort = TRUE)
nrc_2013_trust <- inner_join(nrctrustr,DataBillboard2013) %>% 
  filter(sentiment == "trust") %>% count(word, sort = TRUE)
nrc_2014_trust <- inner_join(nrctrustr,DataBillboard2014) %>% 
  filter(sentiment == "trust") %>% count(word, sort = TRUE)
nrc_2015_trust <- inner_join(nrctrustr,DataBillboard2011) %>% 
  filter(sentiment == "trust") %>% count(word, sort = TRUE)
nrc_2016_trust <- inner_join(nrctrustr,DataBillboard2016) %>% 
  filter(sentiment == "trust") %>% count(word, sort = TRUE)
nrc_2017_trust <- inner_join(nrctrustr,DataBillboard2017) %>% 
  filter(sentiment == "trust") %>% count(word, sort = TRUE)
sum(nrc_1996_trust$n)
sum(nrc_2006_trust$n)
sum(nrc_2010_trust$n)
sum(nrc_2011_trust$n)
sum(nrc_2012_trust$n)
sum(nrc_2013_trust$n)
sum(nrc_2014_trust$n)
sum(nrc_2015_trust$n)
sum(nrc_2016_trust$n)
sum(nrc_2017_trust$n)

# sentiment analysis - anticipation
nrcanticipation <- get_sentiments("nrc") %>% filter(sentiment == "anticipation")
nrc_1996_anticipation <- inner_join(nrcanticipation,DataBillboard1996) %>% 
  filter(sentiment == "anticipation") %>% count(word, sort = TRUE)
nrc_2006_anticipation <- inner_join(nrcanticipation,DataBillboard2006) %>% 
  filter(sentiment == "anticipation") %>% count(word, sort = TRUE)
nrc_2010_anticipation <- inner_join(nrcanticipation,DataBillboard2010) %>% 
  filter(sentiment == "anticipation") %>% count(word, sort = TRUE)
nrc_2011_anticipation <- inner_join(nrcanticipation,DataBillboard2011) %>% 
  filter(sentiment == "anticipation") %>% count(word, sort = TRUE)
nrc_2013_anticipation <- inner_join(nrcanticipation,DataBillboard2013) %>% 
  filter(sentiment == "anticipation") %>% count(word, sort = TRUE)
nrc_2014_anticipation <- inner_join(nrcanticipation,DataBillboard2014) %>% 
  filter(sentiment == "anticipation") %>% count(word, sort = TRUE)
nrc_2015_anticipation <- inner_join(nrcanticipation,DataBillboard2015) %>% 
  filter(sentiment == "anticipation") %>% count(word, sort = TRUE)
nrc_2016_anticipation <- inner_join(nrcanticipation,DataBillboard2016) %>% 
  filter(sentiment == "anticipation") %>% count(word, sort = TRUE)
nrc_2017_anticipation <- inner_join(nrcanticipation,DataBillboard2017) %>% 
  filter(sentiment == "anticipation") %>% count(word, sort = TRUE)
sum(nrc_1996_anticipation$n)
sum(nrc_2006_anticipation$n)
sum(nrc_2010_anticipation$n)
sum(nrc_2011_anticipation$n)
sum(nrc_2012_anticipation$n)
sum(nrc_2013_anticipation$n)
sum(nrc_2014_anticipation$n)
sum(nrc_2015_anticipation$n)
sum(nrc_2016_anticipation$n)
sum(nrc_2017_anticipation$n)

# sentiment analysis - joy
nrcjoy <- get_sentiments("nrc") %>% filter(sentiment == "joy")
nrc_1996_joy <- inner_join(nrcjoy,DataBillboard1996) %>% 
  filter(sentiment == "joy") %>% count(word, sort = TRUE)
nrc_2006_joy <- inner_join(nrcjoy,DataBillboard2006) %>% 
  filter(sentiment == "joy") %>% count(word, sort = TRUE)
nrc_2010_joy <- inner_join(nrcjoy,DataBillboard2010) %>% 
  filter(sentiment == "joy") %>% count(word, sort = TRUE)
nrc_2011_joy <- inner_join(nrcjoy,DataBillboard2011) %>% 
  filter(sentiment == "joy") %>% count(word, sort = TRUE)
nrc_2012_joy <- inner_join(nrcjoy,DataBillboard2012) %>% 
  filter(sentiment == "joy") %>% count(word, sort = TRUE)
nrc_2013_joy <- inner_join(nrcjoy,DataBillboard2013) %>% 
  filter(sentiment == "joy") %>% count(word, sort = TRUE)
nrc_2014_joy <- inner_join(nrcjoy,DataBillboard2014) %>% 
  filter(sentiment == "joy") %>% count(word, sort = TRUE)
nrc_2015_joy <- inner_join(nrcjoy,DataBillboard2015) %>% 
  filter(sentiment == "joy") %>% count(word, sort = TRUE)
nrc_2016_joy <- inner_join(nrcjoy,DataBillboard2016) %>% 
  filter(sentiment == "joy") %>% count(word, sort = TRUE)
nrc_2017_joy <- inner_join(nrcjoy,DataBillboard2017) %>% 
  filter(sentiment == "joy") %>% count(word, sort = TRUE)
sum(nrc_1996_joy$n)
sum(nrc_2006_joy$n)
sum(nrc_2010_joy$n)
sum(nrc_2011_joy$n)
sum(nrc_2012_joy$n)
sum(nrc_2013_joy$n)
sum(nrc_2014_joy$n)
sum(nrc_2015_joy$n)
sum(nrc_2016_joy$n)
sum(nrc_2017_joy$n)

# sentiment analysis - surprise
nrcsurprise <- get_sentiments("nrc") %>% filter(sentiment == "surprise")
nrc_1996_surprise <- inner_join(nrcsurprise,DataBillboard1996) %>% 
  filter(sentiment == "surprise") %>% count(word, sort = TRUE)
nrc_2006_surprise <- inner_join(nrcsurprise,DataBillboard2006) %>% 
  filter(sentiment == "surprise") %>% count(word, sort = TRUE)
nrc_2010_surprise <- inner_join(nrcsurprise,DataBillboard2010) %>% 
  filter(sentiment == "surprise") %>% count(word, sort = TRUE)
nrc_2011_surprise <- inner_join(nrcsurprise,DataBillboard2011) %>% 
  filter(sentiment == "surprise") %>% count(word, sort = TRUE)
nrc_2012_surprise <- inner_join(nrcsurprise,DataBillboard2012) %>% 
  filter(sentiment == "surprise") %>% count(word, sort = TRUE)
nrc_2013_surprise <- inner_join(nrcsurprise,DataBillboard2013) %>% 
  filter(sentiment == "surprise") %>% count(word, sort = TRUE)
nrc_2014_surprise <- inner_join(nrcsurprise,DataBillboard2014) %>% 
  filter(sentiment == "surprise") %>% count(word, sort = TRUE)
nrc_2015_surprise <- inner_join(nrcsurprise,DataBillboard2015) %>% 
  filter(sentiment == "surprise") %>% count(word, sort = TRUE)
nrc_2016_surprise <- inner_join(nrcsurprise,DataBillboard2016) %>% 
  filter(sentiment == "surprise") %>% count(word, sort = TRUE)
nrc_2017_surprise <- inner_join(nrcsurprise,DataBillboard2017) %>% 
  filter(sentiment == "surprise") %>% count(word, sort = TRUE)
sum(nrc_1996_surprise$n)
sum(nrc_2006_surprise$n)
sum(nrc_2010_surprise$n)
sum(nrc_2011_surprise$n)
sum(nrc_2012_surprise$n)
sum(nrc_2013_surprise$n)
sum(nrc_2014_surprise$n)
sum(nrc_2015_surprise$n)
sum(nrc_2016_surprise$n)
sum(nrc_2017_surprise$n)

#################################################################################
# Sentiments with bing
bing_sentiments <- get_sentiments("bing")

bing_neg_1996 <- inner_join(bing_sentiments,DataBillboard1996)%>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
bing_pos_1996 <- inner_join(bing_sentiments,DataBillboard1996)%>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)

bing_neg_2006 <- inner_join(bing_sentiments,DataBillboard2006)%>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
bing_pos_2006 <- inner_join(bing_sentiments,DataBillboard2006)%>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)

bing_neg_2010 <- inner_join(bing_sentiments,DataBillboard2010)%>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
bing_pos_2010 <- inner_join(bing_sentiments,DataBillboard2010)%>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)

bing_neg_2011 <- inner_join(bing_sentiments,DataBillboard2011)%>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
bing_pos_2011 <- inner_join(bing_sentiments,DataBillboard2011)%>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)

bing_neg_2012 <- inner_join(bing_sentiments,DataBillboard2012)%>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
bing_pos_2012 <- inner_join(bing_sentiments,DataBillboard2012)%>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)

bing_neg_2013 <- inner_join(bing_sentiments,DataBillboard2013)%>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
bing_pos_2013 <- inner_join(bing_sentiments,DataBillboard2013)%>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)

bing_neg_2014 <- inner_join(bing_sentiments,DataBillboard2014)%>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
bing_pos_2014 <- inner_join(bing_sentiments,DataBillboard2014)%>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)

bing_neg_2015 <- inner_join(bing_sentiments,DataBillboard2015)%>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
bing_pos_2015 <- inner_join(bing_sentiments,DataBillboard2015)%>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)

bing_neg_2016 <- inner_join(bing_sentiments,DataBillboard2016)%>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
bing_pos_2016 <- inner_join(bing_sentiments,DataBillboard2016)%>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)

bing_neg_2017 <- inner_join(bing_sentiments,DataBillboard2017)%>% 
  filter(sentiment == "negative") %>% count(word, sort = TRUE)
bing_pos_2017 <- inner_join(bing_sentiments,DataBillboard2017)%>% 
  filter(sentiment == "positive") %>% count(word, sort = TRUE)

sum(bing_neg_1996$n)
sum(bing_pos_1996$n)
sum(bing_neg_2006$n)
sum(bing_pos_2006$n)
sum(bing_neg_2010$n)
sum(bing_pos_2010$n)
sum(bing_neg_2011$n)
sum(bing_pos_2011$n)
sum(bing_neg_2012$n)
sum(bing_pos_2012$n)
sum(bing_neg_2013$n)
sum(bing_pos_2013$n)
sum(bing_neg_2014$n)
sum(bing_pos_2014$n)
sum(bing_neg_2015$n)
sum(bing_pos_2015$n)
sum(bing_neg_2016$n)
sum(bing_pos_2016$n)
sum(bing_neg_2017$n)
sum(bing_pos_2017$n)

# add song_number variable to dataframe
bing_1996$song_number_1996 <- song_number_1996[1:end(bing_1996[,1])]
bing_2006$song_number_2006 <- song_number_2006[1:end(bing_2006[,1])]
bing_2016$song_number_2016 <- song_number_2016[1:end(bing_2016[,1])]

# code pos and neg numerically and add sentiment variable
bing_1996_sentiment <- bing_1996 %>% 
  count(word,song_number_1996,song_number_index = song_number_1996, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bing_2006_sentiment <- bing_2006 %>% 
  count(word,song_number_2006,song_number_index = song_number_2006, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bing_2016_sentiment <- bing_2016 %>% 
  count(word,song_number_2016,song_number_index = song_number_2016, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


# Plot with x axis binning according to song 
plot_bing_1996 <- ggplot(bing_1996_sentiment, aes(song_number_index, sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) 

plot_bing_2006 <- ggplot(bing_2006_sentiment, aes(song_number_index, sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE)

plot_bing_2016 <- ggplot(bing_2016_sentiment, aes(song_number_index, sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE)



###############################################################################
# Sentiments with afinn
afinn_sentiments <- get_sentiments("afinn")
afinn_1996 <- inner_join(afinn_sentiments,DataBillboard1996)
afinn_2006 <- inner_join(afinn_sentiments,DataBillboard2006)
afinn_2008 <- inner_join(afinn_sentiments,DataBillboard2008)
afinn_2009 <- inner_join(afinn_sentiments,DataBillboard2009)
afinn_2010 <- inner_join(afinn_sentiments,DataBillboard2010)
afinn_2011 <- inner_join(afinn_sentiments,DataBillboard2011)
afinn_2012 <- inner_join(afinn_sentiments,DataBillboard2012)
afinn_2013 <- inner_join(afinn_sentiments,DataBillboard2013)
afinn_2014 <- inner_join(afinn_sentiments,DataBillboard2014)
afinn_2015 <- inner_join(afinn_sentiments,DataBillboard2015)
afinn_2016 <- inner_join(afinn_sentiments,DataBillboard2016)
afinn_2017 <- inner_join(afinn_sentiments,DataBillboard2017)

afinn_1996$condition <- 1996
afinn_2006$condition <- 2006
afinn_2008$condition <- 2008
afinn_2009$condition <- 2009
afinn_2010$condition <- 2010
afinn_2011$condition <- 2011
afinn_2012$condition <- 2012
afinn_2013$condition <- 2013
afinn_2014$condition <- 2014
afinn_2015$condition <- 2015
afinn_2016$condition <- 2016
afinn_2017$condition <- 2017
#afinn_1996$condition <-1
#afinn_2006$condition <- 2
#afinn_2011$condition <- 3
#afinn_2016$condition <- 4


data_afinn_long <- data.frame(afinn_word = c(#afinn_1996$word,
                                             afinn_2006$word,
                                             afinn_2008$word,
                                             afinn_2009$word,
                                             afinn_2010$word,
                                             afinn_2011$word,
                                             afinn_2012$word,
                                             afinn_2013$word,
                                             afinn_2014$word,
                                             afinn_2015$word,
                                             afinn_2016$word,
                                             afinn_2017$word),
                             afinn_score = c(#afinn_1996$score,
                                             afinn_2006$score,
                                             afinn_2008$score,
                                             afinn_2009$score,
                                             afinn_2010$score,
                                             afinn_2011$score,
                                             afinn_2012$score,
                                             afinn_2013$score,
                                             afinn_2014$score,
                                             afinn_2015$score,
                                             afinn_2016$score,
                                             afinn_2017$score),
                             afinn_condition = c(#afinn_1996$condition,
                                           afinn_2006$condition,
                                           afinn_2008$condition,
                                           afinn_2009$condition,
                                           afinn_2010$condition,
                                           afinn_2011$condition,
                                           afinn_2012$condition,
                                           afinn_2013$condition,
                                           afinn_2014$condition,
                                           afinn_2015$condition,
                                           afinn_2016$condition,
                                           afinn_2017$condition)) 
data_afinn_long$afinn_word_number <- seq.int(nrow(data_afinn_long))
sapply(data_afinn_long, class)
data_afinn_long$afinn_condition <- as.factor(as.character(data_afinn_long$afinn_condition))
data_afinn_long$afinn_score <- as.integer(as.character(data_afinn_long$afinn_score))





########################################################################
## AFINN PLOTS of Pos/neg sentiment per decade

# change data for plotting regression line
sapply(data_afinn_long, class)
data_afinn_long$afinn_condition <- as.numeric(data_afinn_long$afinn_condition)


ggplot(data_afinn_long, aes(x=data_afinn_long$afinn_condition, y=data_afinn_long$afinn_score)) + 
  stat_summary(fun.y=mean, geom="point") + 
  stat_summary(fun.data = mean_se, geom = "errorbar",width=.3) +
  geom_smooth(method="lm", se=FALSE, color = "black") +
  theme_bw() +                  
  expand_limits(y=.5) +
  ylab("Sentiment score") +
  xlab("Year") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position="none") +
  ggtitle("Sentiment per decade") + 
  theme(plot.title = element_text(lineheight=.8)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(2007, 2018)) +
  coord_cartesian(ylim = c(-0.25, 0.75)) + 
  scale_x_continuous(breaks=c(2006,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


install.packages("ggpubr")
library("ggpubr")

cor.test(data_afinn_long$afinn_score, data_afinn_long$afinn_condition, method ="pearson")

ggscatter(data_afinn_long, x = "afinn_condition", y = "afinn_score", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")


#################################################################################


#################################################################
## AFINN ANALYSIS - ANOVA + Tukey

afinn_ANOVA <- aov(afinn_score~ afinn_condition, data=data_afinn_long) 
summary(fit)
TukeyHSD(fit)


######################################################################








afinn_1996_minus5 <- inner_join(afinn_sentiments,afinn_1996) %>% 
  filter(score == "-5") %>% count(word, sort = TRUE)
afinn_2006_minus5 <- inner_join(afinn_sentiments,afinn_2006) %>% 
  filter(score == "-5") %>% count(word, sort = TRUE)
afinn_2016_minus5 <- inner_join(afinn_sentiments,afinn_2016) %>% 
  filter(score == "-5") %>% count(word, sort = TRUE)

afinn_1996_minus4 <- inner_join(afinn_sentiments,afinn_1996) %>% 
  filter(score == "-4") %>% count(word, sort = TRUE)
afinn_2006_minus4 <- inner_join(afinn_sentiments,afinn_2006) %>% 
  filter(score == "-4") %>% count(word, sort = TRUE)
afinn_2016_minus4 <- inner_join(afinn_sentiments,afinn_2016) %>% 
  filter(score == "-4") %>% count(word, sort = TRUE)

afinn_1996_minus3 <- inner_join(afinn_sentiments,afinn_1996) %>% 
  filter(score == "-3") %>% count(word, sort = TRUE)
afinn_2006_minus3 <- inner_join(afinn_sentiments,afinn_2006) %>% 
  filter(score == "-3") %>% count(word, sort = TRUE)
afinn_2016_minus3 <- inner_join(afinn_sentiments,afinn_2016) %>% 
  filter(score == "-3") %>% count(word, sort = TRUE)

afinn_1996_minus2 <- inner_join(afinn_sentiments,afinn_1996) %>% 
  filter(score == "-2") %>% count(word, sort = TRUE)
afinn_2006_minus2 <- inner_join(afinn_sentiments,afinn_2006) %>% 
  filter(score == "-2") %>% count(word, sort = TRUE)
afinn_2016_minus2 <- inner_join(afinn_sentiments,afinn_2016) %>% 
  filter(score == "-2") %>% count(word, sort = TRUE)

afinn_1996_minus1 <- inner_join(afinn_sentiments,afinn_1996) %>% 
  filter(score == "-1") %>% count(word, sort = TRUE)
afinn_2006_minus1 <- inner_join(afinn_sentiments,afinn_2006) %>% 
  filter(score == "-1") %>% count(word, sort = TRUE)
afinn_2016_minus1 <- inner_join(afinn_sentiments,afinn_2016) %>% 
  filter(score == "-1") %>% count(word, sort = TRUE)

afinn_1996_0 <- inner_join(afinn_sentiments,afinn_1996) %>% 
  filter(score == "-0") %>% count(word, sort = TRUE)
afinn_2006_0 <- inner_join(afinn_sentiments,afinn_2006) %>% 
  filter(score == "-0") %>% count(word, sort = TRUE)
afinn_2016_0 <- inner_join(afinn_sentiments,afinn_2016) %>% 
  filter(score == "-0") %>% count(word, sort = TRUE)

afinn_1996_1 <- inner_join(afinn_sentiments,afinn_1996) %>% 
  filter(score == "1") %>% count(word, sort = TRUE)
afinn_2006_1 <- inner_join(afinn_sentiments,afinn_2006) %>% 
  filter(score == "1") %>% count(word, sort = TRUE)
afinn_2016_1 <- inner_join(afinn_sentiments,afinn_2016) %>% 
  filter(score == "1") %>% count(word, sort = TRUE)

afinn_1996_2 <- inner_join(afinn_sentiments,afinn_1996) %>% 
  filter(score == "2") %>% count(word, sort = TRUE)
afinn_2006_2 <- inner_join(afinn_sentiments,afinn_2006) %>% 
  filter(score == "2") %>% count(word, sort = TRUE)
afinn_2016_2 <- inner_join(afinn_sentiments,afinn_2016) %>% 
  filter(score == "2") %>% count(word, sort = TRUE)

afinn_1996_3 <- inner_join(afinn_sentiments,afinn_1996) %>% 
  filter(score == "3") %>% count(word, sort = TRUE)
afinn_2016_3 <- inner_join(afinn_sentiments,afinn_2016) %>% 
  filter(score == "3") %>% count(word, sort = TRUE)
afinn_2006_3 <- inner_join(afinn_sentiments,afinn_2006) %>% 
  filter(score == "3") %>% count(word, sort = TRUE)

afinn_1996_4 <- inner_join(afinn_sentiments,afinn_1996) %>% 
  filter(score == "4") %>% count(word, sort = TRUE)
afinn_2006_4 <- inner_join(afinn_sentiments,afinn_2006) %>% 
  filter(score == "4") %>% count(word, sort = TRUE)
afinn_2016_4 <- inner_join(afinn_sentiments,afinn_2016) %>% 
  filter(score == "4") %>% count(word, sort = TRUE)

afinn_1996_5 <- inner_join(afinn_sentiments,afinn_1996) %>% 
  filter(score == "5") %>% count(word, sort = TRUE)
afinn_2006_5 <- inner_join(afinn_sentiments,afinn_2006) %>% 
  filter(score == "5") %>% count(word, sort = TRUE)
afinn_2016_5 <- inner_join(afinn_sentiments,afinn_2016) %>% 
  filter(score == "5") %>% count(word, sort = TRUE)




# sum of all positive/negative
sum(afinn_1996_1$n,afinn_1996_2$n,afinn_1996_3$n,
    afinn_1996_4$n,afinn_1996_5$n)
sum(afinn_1996_minus5$n,afinn_1996_minus4$n,afinn_1996_minus3$n,
    afinn_1996_minus2$n,afinn_1996_minus1$n)
sum(afinn_2006_1$n,afinn_2006_2$n,afinn_2006_3$n,
    afinn_2006_4$n,afinn_2006_5$n)
sum(afinn_2006_minus5$n,afinn_2006_minus4$n,afinn_2006_minus3$n,
    afinn_2006_minus2$n,afinn_2006_minus1$n)
sum(afinn_2016_1$n,afinn_2016_2$n,afinn_2016_3$n,
    afinn_2016_4$n,afinn_2016_5$n)
sum(afinn_2016_minus5$n,afinn_2016_minus4$n,afinn_2016_minus3$n,
    afinn_2016_minus2$n,afinn_2016_minus1$n)



# plots:

# Sentiment analysis: Display how pos/neg each decade's lyrics are in relation to each other


# Creates vector with word/line number and sentiment score/rating/whatever
bing_1996 <- DataBillboard1996 %>% inner_join(get_sentiments("bing")) %>% 
  mutate(linenumber_1996 = row_number())
nrc_1996 <- DataBillboard1996 %>% inner_join(get_sentiments("nrc")) %>% 
  mutate(linenumber_1996 = row_number())
afinn_1996 <- DataBillboard1996 %>% inner_join(get_sentiments("afinn")) %>% 
  mutate(linenumber_1996 = row_number())

bing_2006 <- DataBillboard2006 %>% inner_join(get_sentiments("bing")) %>% 
  mutate(linenumber_2006 = row_number())
nrc_2006 <- DataBillboard2006 %>% inner_join(get_sentiments("nrc")) %>% 
  mutate(linenumber_2006 = row_number())
afinn_2006 <- DataBillboard2006 %>% inner_join(get_sentiments("afinn")) %>% 
  mutate(linenumber_2006 = row_number())

bing_2010 <- DataBillboard2010 %>% inner_join(get_sentiments("bing")) %>% 
  mutate(linenumber_2010 = row_number())
nrc_2010 <- DataBillboard2010 %>% inner_join(get_sentiments("nrc")) %>% 
  mutate(linenumber_2010 = row_number())
afinn_2010 <- DataBillboard2010 %>% inner_join(get_sentiments("afinn")) %>% 
  mutate(linenumber_2010 = row_number())

bing_2011 <- DataBillboard2011 %>% inner_join(get_sentiments("bing")) %>% 
  mutate(linenumber_2011 = row_number())
nrc_2011 <- DataBillboard2011 %>% inner_join(get_sentiments("nrc")) %>% 
  mutate(linenumber_2011 = row_number())
afinn_2011 <- DataBillboard2011 %>% inner_join(get_sentiments("afinn")) %>% 
  mutate(linenumber_2011 = row_number())

bing_2012 <- DataBillboard2012 %>% inner_join(get_sentiments("bing")) %>% 
  mutate(linenumber_2012 = row_number())
nrc_2012 <- DataBillboard2012 %>% inner_join(get_sentiments("nrc")) %>% 
  mutate(linenumber_2012 = row_number())
afinn_2012 <- DataBillboard2012 %>% inner_join(get_sentiments("afinn")) %>% 
  mutate(linenumber_2012 = row_number())

bing_2013 <- DataBillboard2013 %>% inner_join(get_sentiments("bing")) %>% 
  mutate(linenumber_2013 = row_number())
nrc_2013 <- DataBillboard2013 %>% inner_join(get_sentiments("nrc")) %>% 
  mutate(linenumber_2013 = row_number())
afinn_2013 <- DataBillboard2013 %>% inner_join(get_sentiments("afinn")) %>% 
  mutate(linenumber_2013 = row_number())

bing_2014 <- DataBillboard2014 %>% inner_join(get_sentiments("bing")) %>% 
  mutate(linenumber_2014 = row_number())
nrc_2014 <- DataBillboard2014 %>% inner_join(get_sentiments("nrc")) %>% 
  mutate(linenumber_2014 = row_number())
afinn_2014 <- DataBillboard2014 %>% inner_join(get_sentiments("afinn")) %>% 
  mutate(linenumber_2014 = row_number())

bing_2015 <- DataBillboard2015 %>% inner_join(get_sentiments("bing")) %>% 
  mutate(linenumber_2015 = row_number())
nrc_2015 <- DataBillboard2015 %>% inner_join(get_sentiments("nrc")) %>% 
  mutate(linenumber_2015 = row_number())
afinn_2015 <- DataBillboard2015 %>% inner_join(get_sentiments("afinn")) %>% 
  mutate(linenumber_2015 = row_number())

bing_2016 <- DataBillboard2016 %>% inner_join(get_sentiments("bing")) %>% 
  mutate(linenumber_2016 = row_number())
nrc_2016 <- DataBillboard2016 %>% inner_join(get_sentiments("nrc")) %>% 
  mutate(linenumber_2016 = row_number())
afinn_2016 <- DataBillboard2016 %>% inner_join(get_sentiments("afinn")) %>% 
  mutate(linenumber_2016 = row_number())

bing_2017 <- DataBillboard2017 %>% inner_join(get_sentiments("bing")) %>% 
  mutate(linenumber_2017 = row_number())
nrc_2017 <- DataBillboard2017 %>% inner_join(get_sentiments("nrc")) %>% 
  mutate(linenumber_2017 = row_number())
afinn_2017 <- DataBillboard2017 %>% inner_join(get_sentiments("afinn")) %>% 
  mutate(linenumber_2017 = row_number())

bing_1996$binary_sentiment_pos <- (bing_1996$sentiment=="positive")*1
bing_2006$binary_sentiment_pos <- (bing_2006$sentiment=="positive")*1
bing_2010$binary_sentiment_pos <- (bing_2010$sentiment=="positive")*1
bing_2011$binary_sentiment_pos <- (bing_2011$sentiment=="positive")*1
bing_2012$binary_sentiment_pos <- (bing_2012$sentiment=="positive")*1
bing_2013$binary_sentiment_pos <- (bing_2013$sentiment=="positive")*1
bing_2014$binary_sentiment_pos <- (bing_2014$sentiment=="positive")*1
bing_2015$binary_sentiment_pos <- (bing_2015$sentiment=="positive")*1
bing_2016$binary_sentiment_pos <- (bing_2016$sentiment=="positive")*1
bing_2017$binary_sentiment_pos <- (bing_2017$sentiment=="positive")*1


bing_1996$binary_sentiment_neg <- (bing_1996$sentiment=="negative")*1
bing_2006$binary_sentiment_neg <- (bing_2006$sentiment=="negative")*1
bing_2010$binary_sentiment_pos <- (bing_2010$sentiment=="negative")*1
bing_2011$binary_sentiment_pos <- (bing_2011$sentiment=="negative")*1
bing_2012$binary_sentiment_pos <- (bing_2012$sentiment=="negative")*1
bing_2013$binary_sentiment_pos <- (bing_2013$sentiment=="negative")*1
bing_2014$binary_sentiment_pos <- (bing_2014$sentiment=="negative")*1
bing_2015$binary_sentiment_pos <- (bing_2015$sentiment=="negative")*1
bing_2016$binary_sentiment_neg <- (bing_2016$sentiment=="negative")*1
bing_2017$binary_sentiment_neg <- (bing_2017$sentiment=="negative")*1

bing_1996$condition <- 1
bing_2006$condition <- 2
bing_2010$condition <- 3
bing_2011$condition <- 4
bing_2012$condition <- 5
bing_2013$condition <- 6
bing_2014$condition <- 7
bing_2015$condition <- 8
bing_2016$condition <- 9
bing_2017$condition <- 10

bing_1996$number <- 1
bing_2006$number <- 2
bing_2010$number <- 3
bing_2011$number <- 4
bing_2012$number <- 5
bing_2013$number <- 6
bing_2014$number <- 7
bing_2015$number <- 8
bing_2016$number <- 9
bing_2016$number <- 10

data_bing_long <- data.frame(word = c(bing_1996$word,bing_2006$word,bing_2010$word,bing_2011$word,
                                      bing_2012$word,bing_2015$word,
                                      bing_2016$word, bing_2017$word),
                             pos_binary_sentiment = c(bing_1996$binary_sentiment_pos,
                                                      bing_2006$binary_sentiment_pos,
                                                      bing_2011$binary_sentiment_pos,
                                                      bing_2015$binary_sentiment_pos,
                                                      bing_2016$binary_sentiment_pos,
                                                      bing_2017$binary_sentiment_pos),
                             neg_binary_sentiment = c(bing_1996$binary_sentiment_neg,
                                                      bing_2006$binary_sentiment_neg,
                                                      bing_2011$binary_sentiment_neg,
                                                      bing_2016$binary_sentiment_neg,
                                                      bing_2016$binary_sentiment_neg,
                                                      bing_2017$binary_sentiment_neg),
                             condition = c(bing_1996$condition,
                                           bing_2006$condition,
                                           bing_2011$condition,
                                           bing_2015$condition,
                                           bing_2016$condition,
                                           bing_2017$condition),
                             word_number=seq.int(nrow(data_bing_long))) 


# remove stop words
#data(stop_words)
#DataBillboard2016[,2] <- DataBillboard2016[,2] %>%
#  anti_join(stop_words)
#DataBillboard2006 <- DataBillboard2006 %>%
#  anti_join(stop_words)
#sum(DataBillboard2016$n)
#sum(DataBillboard2006$n)
