#####################################################
# Data preparation for SwifKey project
#
# Dimitra 18/09/2018
#####################################################
library(data.table)
library(tm)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(ngram)
library(RWeka)
library(slam)


# Import data
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", temp)


# Unzip folder
unzip (zipfile = temp)
rm(temp)

# # Read all files
# list_of_files <- list.files(pattern = ".txt", recursive = TRUE)
# datalist = lapply(list_of_files, FUN=readLines
#                   #, stringsAsFactors = F 
#                   ) 
# 
# tweets_source <- VectorSource(datalist)
# 
# 
# t1 <- readLines(list_of_files[[1]])
# 
# txt <- system.file("texts", "txt", package = "tm")
# 
# (ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"), 
#                  readerControl = list(language = "en")))

multiread(  "final/en_US/"  , extension=    "txt")

# Load datasets
US_twitter <- readLines(con <- file("final/en_US/en_US.twitter.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)
US_twitter[1:3]

US_blogs <- readLines(con <- file("final/en_US/en_US.blogs.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)

US_news <- readLines(con <- file("final/en_US/en_US.news.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)

# Exploratory analysis

summary(US_twitter)
summary(US_blogs)
summary(US_news)

lengthtwitter <- length(US_twitter)
lengthblog <- length(US_blogs)
lengthnews <- length(US_news)

countwordstwitter <- sum(sapply(gregexpr("\\S+", US_twitter), length))
countwordsblog <- sum(sapply(gregexpr("\\S+", US_blogs), length))
countwordsnews <- sum(sapply(gregexpr("\\S+", US_news), length))

maxtwitter <- max(nchar(US_twitter))
maxblog <- max(nchar(US_blogs))
maxnews <- max(nchar(US_news))

sizetwitter <- object.size(US_twitter)
sizeblog <- object.size(US_blogs)
sizenews <- object.size(US_news)

filesummary <- data.frame(
  fileName = c("Twitter","Blogs","News"),
  lineCount = c(lengthtwitter, lengthblog, lengthnews),
  wordCount = c(countwordstwitter, countwordsblog, countwordsnews),
  maxlength = c(maxtwitter, maxblog, maxnews),
  size = (c(sizetwitter[1],sizeblog[1],sizenews[1]))
)

knitr::kable(head(filesummary, 10))

docs <- Corpus(DirSource(US_twitter))   
dtm <- DocumentTermMatrix(docs)   
freq <- colSums(as.matrix(dtm))

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14) 
findFreqTerms(dtm, lowfreq=50)

wf <- data.frame(word=names(freq), freq=freq)   
head(wf) 

# Start of Quiz 1
# Find number of lines with word "love" and number of lines with word "hate"
# love<-length(grep("love", US_twitter))
# hate<-length(grep("hate", US_twitter))
# love/hate
# 
# biostats<-grep("biostats", US_twitter, value = T)
# 
# grep("A computer once beat me at chess, but it was no match for me at kickboxing", US_twitter)
# End of quiz 1

# Data preparation

# Remove alpha-numeric characters
new_twitter <- tolower(gsub(pattern="[^a-zA-Z0-9]+", x=US_twitter, replacement = ' '))
new_blog <- tolower(gsub(pattern="[^a-zA-Z0-9]+", x=US_blogs, replacement = ' '))
new_news <- tolower(gsub(pattern="[^a-zA-Z0-9]+", x=US_news, replacement = ' '))

rm(US_twitter_doc, US_blog_doc, US_news_doc)

# Create a sample for each data set and then aggregate into another data set
sample_twitter<-new_twitter[sample(1:length(new_twitter),200)]
sample_blog<-new_blog[sample(1:length(new_blog),200)]
sample_news<-new_news[sample(1:length(new_news),200)]
mysample <-c(sample_twitter, sample_blog, sample_news)

# Save the sample
writeLines(mysample, "final/en_US/mysample.txt")

# Concatenate samples and save file
#doc.corpus <- concatenate(sample_twitter, sample_blog, sample_news)

rm(sample_twitter, sample_blog, sample_news)

mysample[2:3]



lengthmysample <- length(mysample)
countwordsmysample <- sum(sapply(gregexpr("\\S+", mysample), length))
maxmysample <- max(stri_length(mysample))
sizemysample <- object.size(mysample)

# Create a summary of the data sets and compare to mysample
filesummary <- data.frame(
  fileName = c("Twitter","Blogs","News", "Aggregated Sample"),
  lineCount = c(lengthtwitter, lengthblog, lengthnews, lengthmysample),
  wordCount = c(countwordstwitter, countwordsblog, countwordsnews, countwordsmysample),
  maxlength = c(maxtwitter, maxblog, maxnews, maxmysample),
  size = (c(sizetwitter[1],sizeblog[1],sizenews[1], sizemysample[1]))
)

knitr::kable(head(filesummary, 10))

# Save files
saveRDS(filesummary, file = "/final/en_US/filesummary.Rda")

# Remove and convert odd characters
clean_mysample <- iconv(mysample, 'UTF-8', 'ASCII', "byte")

clean_mysample <- VCorpus(VectorSource(clean_mysample)) # Make corpus # Corpus(VectorSource(clean_mysample))
clean_mysample <- tm_map(clean_mysample, stripWhitespace) # Remove unneccesary white spaces
clean_mysample <- tm_map(clean_mysample, removePunctuation) # Remove punctuation
clean_mysample <- tm_map(clean_mysample, removeNumbers) # Remove numbers
clean_mysample <- tm_map(clean_mysample, tolower) # Convert to lowercase
clean_mysample <- tm_map(clean_mysample, PlainTextDocument) # Plain text


cleancorpusDF <-data.frame(text=unlist(sapply(clean_mysample,`[`, "content")), stringsAsFactors = FALSE)

# Create a matrix from corpus
dtm <- DocumentTermMatrix(clean_mysample)

freq <- colSums(as.matrix(dtm))

# Frequency of frequencies
a <- head(table(freq), 20)
a

# Frequency by word
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
b <- head(freq, 20)
b

# Find terms that appear more than 50 times
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)


# Plot of frequencies
p <- ggplot(subset(wf, freq>100), aes(word, freq, colour = "red"))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p  

# Words that occur at least 50 times by frequency
set.seed(1234)
wordcloud(clean_mysample, min.freq = 50, random.order = FALSE,rot.per=0.35, use.r.layout=FALSE)


# Top 300 words by frequency
set.seed(1234)
wordcloud(clean_mysample, max.words = 300, random.order = FALSE,rot.per=0.35, use.r.layout=FALSE,colors=brewer.pal(8, "Paired"))

#https://rstudio-pubs-static.s3.amazonaws.com/249408_b4ed31c8df4b4ca09c3250d6701071d6.html

# Building the model

n_grams_plot <- function(n, data) {
  options(mc.cores=1)
  
  # Builds n-gram tokenizer 
  tk <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  # Create matrix
  ngrams_matrix <- TermDocumentMatrix(data, control=list(tokenize=tk))
  # make matrix for easy view
  ngrams_matrix <- as.matrix(rollup(ngrams_matrix, 2, na.rm=TRUE, FUN=sum))
  ngrams_matrix <- data.frame(word=rownames(ngrams_matrix), freq=ngrams_matrix[,1])
  # find 20 most frequent n-grams in the matrix
  ngrams_matrix <- ngrams_matrix[order(-ngrams_matrix$freq), ][1:20, ]
  ngrams_matrix$word <- factor(ngrams_matrix$word, as.character(ngrams_matrix$word))
  
  # plots
  ggplot(ngrams_matrix, aes(x=word, y=freq)) + 
    geom_bar(stat="Identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("n-grams") + 
    ylab("Frequency")
}



n_grams_plot(n=1, data=clean_mysample)

n_grams_plot(n=2, data=clean_mysample)

n_grams_plot(n=3, data=clean_mysample)

n_grams_plot(n=4, data=clean_mysample)