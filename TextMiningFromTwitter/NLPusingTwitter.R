
# check if packages are installed , If not then Install the packages 

list.of.packages <- c("ggplot2", "tm","twitteR", "SnowballC","wordcloud","fpc","cluster","RColorBrewer","class")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,dependencies=TRUE)

# Import the required Libraries

library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(SnowballC)

# Connect to the Twitter

ckey <- 'yourckey'
skey <- 'yourskey'
token <- 'yourtoken'
sectoken <- 'yoursecuritytoekn'

setup_twitter_oauth(ckey,skey,token,sectoken)

# Get some tweets of some keyword
keyword = '#PakvEng' ;
keyword.tweets <- searchTwitter(keyword,n=1000,lang = 'en')

# Grabbing text Data from the Tweets
keyword.text <- sapply(keyword.tweets,function(x) x$getText())

#########################
# Clean Data
########################

keyword.text <- iconv(keyword.text,'UTF-8','ASCII')

# Create Corpus 
keyword.corpus <- Corpus(VectorSource(keyword.text))


## Preprocessing      
stopthesewordstoo <- c('pakvseng','amp','pakveng','engvpak','engvspak','eng','england','odi','pakistan','azizsarparah','cssays','ovs','httpstcozonoruymc','pakistani')

keyword.corpus <- tm_map(keyword.corpus, removePunctuation)   # *Removing punctuation:*    
keyword.corpus <- tm_map(keyword.corpus, removeNumbers)      # *Removing numbers:*    
keyword.corpus <- tm_map(keyword.corpus, tolower)   # *Converting to lowercase:*    
keyword.corpus <- tm_map(keyword.corpus, removeWords, c(stopthesewordstoo,stopwords('english')))   # *Removing "stopwords" 
library(SnowballC)
keyword.corpus <- tm_map(keyword.corpus, stemDocument) # *Removing common word endings* (e.g., "ing", "es")   
keyword.corpus <- tm_map(keyword.corpus, stripWhitespace) # *Stripping whitespace   
keyword.corpus <- tm_map(keyword.corpus, PlainTextDocument)

# Create Term Document matrix
term.doc.matrix <- TermDocumentMatrix(keyword.corpus)

# Convert Object into a matrix
term.doc.matrix <- as.matrix(term.doc.matrix)

# Get Word count
word.freq <- sort(rowSums(term.doc.matrix),decreasing = TRUE)

dtm <- data.frame(word = names(word.freq),freq = word.freq)

# Create WordCloud
dark2 <- brewer.pal(4, "Dark2")   
wordcloud(dtm$word, dtm$freq, max.words=100,  random.order = FALSE, rot.per=0.2, colors=dark2)





