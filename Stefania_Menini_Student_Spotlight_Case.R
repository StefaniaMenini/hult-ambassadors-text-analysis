#' Title:   Hult International Business School Student Spotlight
#' Purpose: Is there any bias in the showcased ambassadors' bio and interest?
#' Author:  Stefania Menini
#' E-mail:  smenini2019@student.hult.edu
#' Date:    Mar 1 2021


################################################################################
# Analysis Objective ###########################################################
# The objective of the analysis is to ensure that the bio and the interest of  #
# the showcased Hult students ambassadors do not present bias towards campuses,#
# topics, or other observed information. Achieving this goal will guarantee a  # 
# welcoming, diverse, and inclusive learning atmosphere (Kwartler, 2020).      #
################################################################################


################################################################################
# Initial Set Up ###############################################################
################################################################################

# Setting the working directory
setwd("~/R/Git_hult_NLP_student/hult_NLP_student/cases/session II/student ambassadors")

# Loading basic packages required for the analysis
library(ggplot2)
library(ggthemes)
library(stringi)
library(stringr)
library(tm)

# Loading additional packages
library(lubridate)
library(wordcloud)
library(RColorBrewer)
library(plotrix)
library(ggalt)
library(tibble)
library(dplyr)
library(lexicon)
library(tidytext)
library(radarchart)
library(textdata)
library(magrittr)
library(corpus)
# library(hrbrthemes)
library(qdap)
library(igraph)
library(wordcloud2)
library(pbapply)


# Avoiding strings being counted as factors
options(stringsAsFactors = FALSE)

# Limiting errors by expanding the accepted number of location in character types
Sys.setlocale('LC_ALL','C')

# Loading "final_student_data" dataset and collecting dataset general info
student_text <- read.csv('final_student_data.csv', header = TRUE)
head(student_text,5) # Checking the first five rows of "final_student_data" 
names(student_text)  # Checking columns names of "final_student_data"
dim(student_text)    # Checking "final_student_data" dimension

# Creating stopwords using the 'SMART' 
stops <- c(stopwords('SMART'), 'hult', 'hult international business school')


################################################################################
# Customized functions #########################################################
################################################################################

# Defining "tryTolower"
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)}

# Defining "cleanCorpus"
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)}


################################################################################
# Ambassadors General Info #####################################################
################################################################################

# Ambassadors by "campus" 
ggplot(data = student_text, 
       mapping = aes(x = campus, fill = namSorGender.likelyGender)) +
  geom_bar(position = "dodge", alpha = .9) +
  labs(title = "Hult Ambassadors", x = "Hult Campuses", y = " ", 
       fill = "Gender") +
  scale_fill_manual(values = c("hotpink1", "lightgrey")) +
  theme_tufte() 

###

# Ambassadors by "programTitle"            
ggplot(data = student_text, mapping = aes(x = programTitle)) +
  geom_bar(fill = "hotpink1", position = "dodge", alpha = .9) +
  labs(title = "Ambassadors per Program Title", x = "", y = " ") +
  coord_flip() +
  theme_tufte() 

###

# Counting Ambassadors per Hult campus 
table(student_text$campus)

# Counting Ambassadors' per gender 
table(student_text$namSorGender.likelyGender)

# Counting Ambassadors per program title
table(student_text$programTitle)


################################################################################
# Creating and Organize Dataset Subsets ########################################
################################################################################

# Concatenating "bio" and "interest" 
student_text$student_allText <- paste(student_text$bio,student_text$interests)

# Renaming "student_text: first column 
names(student_text)[1] <- 'doc_id'

###

# Creating a data subset for the American Hult campuses (Boston & San Francisco)
america_campuses <- subset(student_text, student_text$campus == c('Boston') 
                           | student_text$campus == c('San Francisco'))

# Creating a data subset for the Eurasian Hult campuses (Dubai & London)
eurasia_campuses <- subset(student_text, student_text$campus == c('Dubai') 
                           | student_text$campus == c('London'))


################################################################################
# Searching for Word Patterns in  "america_campuses" ###########################
################################################################################

# Diversity keywords scanning in "america_campuses"
diversity_keywordsOR <-"diverse|diversity|variety|mix|multi-cultural|
                        multicultural|global|world|cultures|international"
america_diversity <- grepl(diversity_keywordsOR, america_campuses$student_allText,
                           ignore.case=TRUE)

# Calculating the % of times diversity keywords have been metioned
america_diversity_score <- sum(america_diversity) / nrow(student_text)
america_diversity_score # 0.3647059

###

# Thinking (Hult DNA) keywords scanning in "america_campuses"
thinking_keywordsOR <-"awareness|self|challenge|growth mindset|"
america_thinking <- grepl(thinking_keywordsOR, america_campuses$student_allText,
                          ignore.case=TRUE)

# Calculating the % of times thinking keywords have been metioned
america_thinking_score <- sum(america_thinking) / nrow(student_text) 
america_thinking_score # 0.4235294

###

# Communicating (Hult DNA) keywords scanning in "america_campuses"
communicating_keywordsOR <-"communication|communicate|confident|sharing|
                            listening|listen|influence"

america_communicating <- grepl(communicating_keywordsOR, 
                               america_campuses$student_allText,ignore.case=TRUE)

# Calculating the % of times communicating keywords have been mentioned
america_communicating_score <- sum(america_communicating) / nrow(student_text) 
america_communicating_score # 0.05882353

###

# Team-Building (Hult DNA) keywords scanning in "america_campuses"
teambuilding_keywordsOR <-"team|peers|clubs|community|engage|engagement|network|
                           connection|connecting|cooperation"

america_teambuilding <- grepl(teambuilding_keywordsOR, 
                              america_campuses$student_allText,ignore.case=TRUE) 

# Calculating the % of times team-building keywords have been mentioned
america_teambuilding_score <- sum(america_teambuilding) / nrow(student_text) 
america_teambuilding_score # 0.3058824


################################################################################
# Searching for Word Patterns in  "eurasia_campuses" ###########################
################################################################################

# Diversity keywords scanning in "eurasia_campuses"
eurasia_diversity <- grepl(diversity_keywordsOR, eurasia_campuses$student_allText,
                           ignore.case=TRUE)

# Calculating the % of times diversity keywords have been mentioned
eurasia_diversity_score <- sum(eurasia_diversity) / nrow(student_text) 
eurasia_diversity_score # 0.5411765

###

# Thinking (Hult DNA) keywords scanning in "eurasia_campuses"
eurasia_thinking <- grepl(thinking_keywordsOR, eurasia_campuses$student_allText,
                          ignore.case=TRUE) 

# Calculating the % of times thinking keywords have been mentioned
eurasia_thinking_score <- sum(eurasia_thinking) / nrow(student_text)
eurasia_thinking_score  # 0.5764706

###

# Communicating (Hult DNA) keywords scanning in "eurasia_campuses"
eurasia_communicating <- grepl(communicating_keywordsOR, 
                               eurasia_campuses$student_allText,ignore.case=TRUE)

# Calculating the % of times communicating keywords have been mentioned
eurasia_communicating_score <- sum(eurasia_communicating) / nrow(student_text) 
eurasia_communicating_score # 0.1176471

###

# Team-Building (Hult DNA) keywords scanning in "eurasia_campuses"
eurasia_teambuilding <- grepl(teambuilding_keywordsOR, 
                              eurasia_campuses$student_allText,ignore.case=TRUE) 

# Calculating the % of times team-building keywords have been mentioned
eurasia_teambuilding_score <- sum(eurasia_teambuilding) / nrow(student_text) 
eurasia_teambuilding_score # 0.3176471


################################################################################
# Comparing Results: "america_campuses" vs. "eurasia_campuses" #################
################################################################################

# Creating a matrix to summarize campuses' scores per category 
scores_comparison <- matrix(c(america_diversity_score, eurasia_diversity_score,
                              america_thinking_score, eurasia_thinking_score,
                              america_communicating_score, eurasia_communicating_score,
                              america_teambuilding_score, eurasia_teambuilding_score),
                            ncol = 2, byrow = TRUE )

# Defining "scores_comparison" columns' names
colnames(scores_comparison ) <- c("America Campuses", "Eurasia Campuses")

# Defining "scores_comparison" rows' names
rownames(scores_comparison) <- c("Diversity", "Thinking", "Communicating", 
                                 "Team Building")

# Displaying the "scores_comparison" matrix 
scores_comparison 


################################################################################
#  String_count() for Hult DNA categories and Diversity (all dataset) ##########
################################################################################

# Counting words based on the Hult DNA categories and diversity 
diversity <- sum(stri_count(student_text$student_allText, regex = 'diverse|diversity|
                                        variety|mix|multi-cultural|multicultural|
                                        global|world|cultures|international'))
thinking <- sum(stri_count(student_text$student_allText, regex ='awareness|self|
                                        challenge|growth mindset'))
communicating <- sum(stri_count(student_text$student_allText, regex ='communication|
                                        communicate|confident|sharing|listening|listen|
                                        influence'))
teambuilding <- sum(stri_count(student_text$student_allText, regex ='team|peers|
                                        clubs|community|engage|engagement|network|
                                        connection|connecting|cooperation'))

# Organizing term objects into a data frame
all_termFreq <- data.frame(Terms = c('diversity','thinking','communicating', 
                                     'teambuilding'),
                           Freq  = c(diversity, thinking, communicating, teambuilding))

# Checking the object frequencies 
all_termFreq

# Plotting a geom_bar() for "all_termFreq"
ggplot(data = all_termFreq, aes(x = reorder(Terms, Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill = "hotpink1") + 
  labs(title = "Hult DNA and Diversity Words' Categories ", y = "Count", x = " ") +
  coord_flip() + 
  theme_tufte() 


################################################################################
# Volatile Corpus ##############################################################
################################################################################

# Making and cleaning a volatile corpus for "student_text"
student_corp <- VCorpus(VectorSource(student_text$student_allText))
student_corp <- cleanCorpus(student_corp, stops)
content(student_corp[[1]]) # Checking student_corp

###

# Making and cleaning a volatile corpus for "america_campuses"
america_corp <- VCorpus(VectorSource(america_campuses$student_allText))
america_corp <- cleanCorpus(america_corp, stops)
content(america_corp[[1]])  # Checking america_corp

###

# Making and cleaning a volatile corpus for "eurasia_campuses"
eurasia_corp <- VCorpus(VectorSource(eurasia_campuses$student_allText))
eurasia_corp <- cleanCorpus(eurasia_corp, stops)
content(eurasia_corp[[1]]) # Checking eurasia_corp


################################################################################
# Term Document Matrix #########################################################
################################################################################

# Making a Term Document Matrix for "america_campuses"
america_Tdm <- TermDocumentMatrix(america_corp, control = list(weighting = weightTf))
america_TdmM <- as.matrix(america_Tdm)
dim(america_TdmM) # Checking matrix dimensions

###

# Making a Term Document Matrix for "eurasia_campuses"
eurasia_Tdm <- TermDocumentMatrix(eurasia_corp, control = list(weighting = weightTf))
eurasia_TdmM <- as.matrix(eurasia_Tdm)
dim(eurasia_TdmM) # Checking matrix dimensions

###

# Making a Term Document Matrix for "eurasia_campuses"
student_Tdm <- TermDocumentMatrix(student_corp, control = list(weighting = weightTf))
student_TdmM <- as.matrix(student_Tdm)
dim(student_TdmM) # Checking matrix dimensions


################################################################################
# Most Frequent Terms ##########################################################
################################################################################

# Getting the most frequent terms for "america_campuses"
america_TopTerms <- rowSums(america_TdmM)
america_TopTerms <- data.frame(terms = rownames(america_TdmM), freq = america_TopTerms)
rownames(america_TopTerms) <- NULL
head(america_TopTerms)

# Getting the most frequent term
america_idx <- which.max(america_TopTerms$freq)
america_TopTerms[america_idx, ] # business

###

# Getting the most frequent terms for "eurasia_campuses"
eurasia_TopTerms <- rowSums(eurasia_TdmM)
eurasia_TopTerms <- data.frame(terms = rownames(eurasia_TdmM), freq = eurasia_TopTerms)
rownames(eurasia_TopTerms) <- NULL
head(eurasia_TopTerms)

# Getting the most frequent term
eurasia_idx <- which.max(eurasia_TopTerms$freq)
america_TopTerms[eurasia_idx, ] # coming

###

# Getting the most frequent terms for all dataset
student_all_TopTerms <- rowSums(student_TdmM)
student_all_TopTerms <- data.frame(terms = rownames(student_TdmM), freq = student_all_TopTerms)
rownames(student_all_TopTerms) <- NULL
head(student_all_TopTerms)

# Getting the most frequent term
student_all_idx <- which.max(student_all_TopTerms$freq)
student_all_TopTerms[student_all_idx, ] # business 


################################################################################
# Plotting the most Frequent Terms #############################################
################################################################################

# Creating an "america_TopTerms" subset
america_Top_subset <- subset(america_TopTerms, america_TopTerms$freq > 12) 
america_Top_subset <- america_Top_subset[order(america_Top_subset$freq, decreasing=F),]
america_Top_subset[10:30,]

# Converting top terms into factors 
america_Top_subset$terms <- factor(america_Top_subset$terms, 
                                   levels=unique(as.character(america_Top_subset$terms))) 

# Plotting top terms for "america_campuses"
ggplot(data = america_Top_subset, mapping = aes(x=terms, y=freq)) + 
  geom_bar(stat="identity", fill = "hotpink1") + 
  labs(title = "Top Terms among America Campuses", x = "", y = "Frequency") +
  coord_flip() +
  theme_tufte() 

###

# Creating an "eurasia_TopTerms" subset
eurasia_Top_subset <- subset(eurasia_TopTerms, eurasia_TopTerms$freq > 16) 
eurasia_Top_subset <- eurasia_Top_subset[order(eurasia_Top_subset$freq, decreasing=F),]
eurasia_Top_subset[9:31,]

# Converting top terms into factors 
eurasia_Top_subset$terms <- factor(eurasia_Top_subset$terms, 
                                   levels=unique(as.character(eurasia_Top_subset$terms))) 

# Plotting top terms for "eurasia_campuses"
ggplot(data = eurasia_Top_subset, mapping = aes(x=terms, y=freq)) + 
  geom_bar(stat="identity", fill = "hotpink1") + 
  labs(title = "Top Terms among Eurasia Campuses", x = "", y = "Frequency") +
  coord_flip() +
  theme_tufte() 


################################################################################
# Association Analysis #########################################################
################################################################################

# Inspecting word associations for "america_campuses"
america_associations <- findAssocs(america_Tdm, 'business', 0.37)
america_associations # Checking results

# Organizing words for "america_associations"
america_assocDF <- data.frame(terms=names(america_associations[[1]]),
                              value=unlist(america_associations))
america_assocDF$terms <- factor(america_assocDF$terms, levels=america_assocDF$terms)
rownames(america_assocDF) <- NULL
america_assocDF

# Displaying associations
ggplot(america_assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=america_assocDF, col='hotpink1') +
  labs(title = "Association Analysis for Business in America Campuses", x = "Value", 
       y = " ") +
  theme_tufte() + 
  geom_text(aes(x=value,label=value), colour="grey",hjust="inward", 
            vjust ="inward" , size=3) 

###

# Inspecting word associations for "eurasia_campuses"
eurasia_associations <- findAssocs(eurasia_Tdm, 'business', 0.37)
eurasia_associations # Checking results

# Organizing words for "eurasia_associations"
eurasia_assocDF <- data.frame(terms=names(eurasia_associations[[1]]),
                              value=unlist(eurasia_associations))
eurasia_assocDF$terms <- factor(eurasia_assocDF$terms, levels=eurasia_assocDF$terms)
rownames(eurasia_assocDF) <- NULL
eurasia_assocDF

# Displaying associations
ggplot(eurasia_assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=eurasia_assocDF, col='hotpink1') +
  labs(title = "Association Analysis for Business in Eurasia Campuses", x = "Value", 
       y = " ") +
  theme_tufte() + 
  geom_text(aes(x=value,label=value), colour="grey",hjust="inward", 
            vjust ="inward" , size=3) 

###

# Inspecting word associations for all dataset
student_associations <- findAssocs(student_Tdm, 'business', 0.30)
student_associations # Checking results

# Organizing words for all dataset 
student_assocDF <- data.frame(terms=names(student_associations[[1]]),
                              value=unlist(student_associations))
student_assocDF$terms <- factor(student_assocDF$terms, levels=student_assocDF$terms)
rownames(student_assocDF) <- NULL
student_assocDF

# Displaying associations
ggplot(student_assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=student_assocDF, col='hotpink1') +
  labs(title = "Association Analysis for Business (all dataset)", x = "Value", 
       y = " ") +
  theme_tufte() + 
  geom_text(aes(x=value,label=value), colour="grey",hjust="inward", 
            vjust ="inward" , size=3) 


################################################################################
# WorldCloud ###################################################################
################################################################################

# Setting worldcloud palette
pal <- brewer.pal(8, "Greys")
pal <- pal[-(1:2)]

###

# Plotting a worldcloud for "america_campuses"
set.seed(1234)
wordcloud(america_TopTerms$terms,
          america_TopTerms$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

### 

# Plotting a worldcloud for "eurasia_campuses"
set.seed(1234)
wordcloud(eurasia_TopTerms$terms,
          eurasia_TopTerms$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

###

# Plotting a world cloud for all dataset
set.seed(1234)
wordcloud(student_all_TopTerms$terms,
          student_all_TopTerms$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))


################################################################################
# Other WorldCloud Type ########################################################
################################################################################

# Choose a color & drop light ones
pal2 <- brewer.pal(8, "Greys")
wordcloud2(student_all_TopTerms[1:50,], 
           color = pal2, 
           backgroundColor = "pink")


################################################################################
# Comparison Cloud: "bio" vs "interest" ########################################
################################################################################

# Defining a vector corpus
all_bio      <- VCorpus(VectorSource(student_text$bio))
all_interest <- VCorpus(VectorSource(student_text$interest))

# Cleaning up the data
all_bio       <- cleanCorpus(all_bio, stops)
all_interest <- cleanCorpus(all_interest, stops)

# Checking the results 
length(all_bio)
length(all_interest)

# Collapsing each document into a single "subject"
all_bio       <- paste(all_bio, collapse = ' ')
all_interest <- paste(all_interest, collapse = ' ')

# Combining the "all_bio" and "all_interest" 
bio_interest <- c(all_bio, all_interest)
bio_interest <- VCorpus((VectorSource(bio_interest)))

# Defining TDM 
ctrl      <- list(weighting = weightTfIdf)
bio_interest_TDM  <- TermDocumentMatrix(bio_interest, control = ctrl)
bio_interest_TDMm <- as.matrix(bio_interest_TDM)

# Defining columns order
colnames(bio_interest_TDMm) <- c('Bio', 'Interests')

# Examining TDM 
head(bio_interest_TDMm)

# Plotting a comparison cloud
comparison.cloud(bio_interest_TDMm, 
                 max.words= 30, 
                 random.order=FALSE,
                 title.size=0.8,
                 colors=brewer.pal(ncol(bio_interest_TDMm),"Paired"),
                 title.colors=FALSE, match.colors=FALSE,
                 scale=c(3,0.2))


# End ##########################################################################
