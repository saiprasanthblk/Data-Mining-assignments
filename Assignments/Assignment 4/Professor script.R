library(XML)
library(tm)
library(topicmodels)

# read random sample of 100 news documents
set.seed(1)
news = as.data.frame(xmlToDataFrame("Data/news_documents.xml", stringsAsFactors = FALSE)[,"c"])
news = VCorpus(DataframeSource(news))
news = news[sample(1:length(news), 100)]

# clean and compute TF
news.clean = tm_map(news, stripWhitespace)                          # remove extra whitespace
news.clean = tm_map(news.clean, removeNumbers)                      # remove numbers
news.clean = tm_map(news.clean, removePunctuation)                  # remove punctuation
news.clean = tm_map(news.clean, content_transformer(tolower))       # ignore case
news.clean = tm_map(news.clean, removeWords, stopwords("english"))  # remove stop words
news.clean = tm_map(news.clean, stemDocument)                       # stem all words
news.clean.tf = DocumentTermMatrix(news.clean, control = list(weighting = weightTf))

# remove empty documents
row.sums = apply(news.clean.tf, 1, sum)
news = news[row.sums > 0]
news.clean.tf = news.clean.tf[row.sums > 0,]

# train topic model with 10 topics
topic.model = LDA(news.clean.tf, 10)

# look at the top 10 words within the first 5 topics
terms(topic.model, 10)[,1:5]

# look at the top 5 topics within the first 10 documents
topics(topic.model, 5)[,1:10]

# group documents by most likely topic and look at one of the document groups
document.most.likely.topic = topics(topic.model, 1)
document.topic.clusters = split(news, document.most.likely.topic)

# each element in the list is a subcorpus of documents that have the same most likely topic
document.topic.clusters[[1]]  # document whose most likely topic is "1"

# print documents within the first few topic clusters
for(cluster.num in 1:3)
{
  # print the first 2 documents in the current cluster
  num.docs.in.cluster = length(document.topic.clusters[[cluster.num]])
  for(doc.num in 1:min(2,num.docs.in.cluster))
  {
    print(paste("Cluster", cluster.num, ", Document", doc.num, ": ", document.topic.clusters[[cluster.num]][[doc.num]]$content))
  }
}

# gamma contains the document-topic matrix...what might we do with it?
topic.model@gamma[1:5,]

# infer the topic probabilities of new text using the topic model we estimated above -- why is this important when using topic probabilities as predictors?
testing.documents = as.data.frame(c("Kerry said the state should increase taxes."))
testing.corpus = VCorpus(DataframeSource(testing.documents))
testing.corpus = tm_map(testing.corpus, stripWhitespace)                    # remove extra whitespace
testing.corpus = tm_map(testing.corpus, removeNumbers)                      # remove numbers
testing.corpus = tm_map(testing.corpus, removePunctuation)                  # remove punctuation
testing.corpus = tm_map(testing.corpus, content_transformer(tolower))       # ignore case
testing.corpus = tm_map(testing.corpus, removeWords, stopwords("english"))  # remove stop words
testing.corpus = tm_map(testing.corpus, stemDocument)                       # stem all words
testing.corpus.tf = DocumentTermMatrix(testing.corpus, control = list(weighting = weightTf))
inferred_probabilities = posterior(topic.model, testing.corpus.tf)
inferred_probabilities$topics

# cluster documents in topic space
document.topic.probabilities = topic.model@gamma  # topic distribution for each document
topic.space.kmeans.clusters = kmeans(document.topic.probabilities, 10)
topic.space.clustered.news = split(news, topic.space.kmeans.clusters$cluster)
topic.space.clustered.news[[1]][[1]]$content
topic.space.clustered.news[[1]][[2]]$content
topic.space.clustered.news[[1]][[3]]$content
