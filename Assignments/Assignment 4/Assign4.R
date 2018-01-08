library(XML)
library(tm)
library(topicmodels)
library(readr)

#Read in files:
train <- read_csv("fitnesstweets.csv") #Read in the comma separated value data file for training the model

# Most of the tweets in the data set are Fitbit related tweets

train <- train[1:1000, ] # selecting only the first 1000 tweets since my laptop's computing power is not enough to form a term document matrix for all the tweets

### PREPARE/CLEAN TRAINING SET ###

#Get the content
news = VCorpus(DataframeSource(train))

##### Reducing Term Sparsity #####

#Clean up the corpus
news.clean = tm_map(news, stripWhitespace)                          # remove extra whitespace
news.clean = tm_map(news.clean, removeNumbers)                      # remove numbers
news.clean = tm_map(news.clean, removePunctuation)                  # remove punctuation
news.clean = tm_map(news.clean, content_transformer(tolower))       # ignore case
news.clean = tm_map(news.clean, removeWords, stopwords("english"))  # remove stop words
news.clean = tm_map(news.clean, stemDocument)                       # stem all words

#Recompute TF-IDF matrix
news.clean.tf = DocumentTermMatrix(news.clean)

# remove empty documents
row.sums = apply(news.clean.tf, 1, sum)
news = news[row.sums > 0]
news.clean.tf = news.clean.tf[row.sums > 0,]



# the list of topics that I think will be present in this data set are fitness company names, Running, lucky draw related tweets, customer support, general fitness terms (diet may be?)



# train topic model with 2 topics
topic.model = LDA(news.clean.tf, 2)

# look at the top 10 words within the first 5 topics
terms(topic.model, 10)[,1:2]

# its interesting that there are some common words between the two topics. Both Gamin and Fitbit are in the same group in topic 2


# train topic model with 5 topics
topic.model = LDA(news.clean.tf, 5)

# look at the top 10 words within the first 5 topics
terms(topic.model, 10)[,1:5]

# Fitbit is the single most common word in all the topics actually. The lesson here for me is that it doesn't always make sense to split the sentences into single words because the word blaze is meaningless without context (Fitbit Blaze is a popular model)



# train topic model with 10 topics
topic.model = LDA(news.clean.tf, 10)

# look at the top 10 words within the first 5 topics
terms(topic.model, 10)[,1:10]

# Topic 7. Most of the words appear to be customer support related tweets - not running or competitions etc. 



# train topic model with 25 topics
topic.model = LDA(news.clean.tf, 25)

# look at the top 10 words within the first 5 topics
terms(topic.model, 10)[,1:25]

# topic 1 is absolute fitness terms (motivating). Topic 5 seems to be customer support related. Topic 11 is absolutely lucky draw competitions group. Note that Fitbit is the most popular term in many topics and is present in almost all the topics



# train topic model with 50 topics
topic.model = LDA(news.clean.tf, 50)

# look at the top 10 words within the first 5 topics
terms(topic.model, 10)[,1:50]

# Oh, there's chinese terms in the topics now. Not sure how they are intrepeted though.



# train topic model with 100 topics   # may take 10 minutes to run
topic.model = LDA(news.clean.tf, 100)

# look at the top 10 words within the first 5 topics
terms(topic.model, 10)[,1:100]

