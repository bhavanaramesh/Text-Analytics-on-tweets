#--------------------------------Installing packages in R-----------------------------------------------
packages <- c("twitteR","ROAuth","rtweet","dplyr","tidytext","")
check.install.load.Package<-function(package_name){
  if(!package_name%in%installed.packages()){
    install.packages(package_name)
  }
  library(package_name,character.only = TRUE)
}
for(package in packages){
  check.install.load.Package(package)
}
#--------------------------------Twitter API Secret----------------------------------------------------
api_key = " " # your api_key
api_secret = " " # your api_secret 
access_token = " " # your access_token 
access_token_secret = " " # your access_token_sceret 
credential<-OAuthFactory$new(consumerKey=api_key,
                             consumerSecret=api_secret,
                             requestURL="https://api.twitter.com/oauth/request_token",
                             accessURL="https://api.twitter.com/oauth/access_token",
                             authURL="https://api.twitter.com/oauth/authorize")
credential$handshake()
setup_twitter_oauth(api_key,api_secret,access_token,
                    access_token_secret)

#--------------------------------Search for tweet keywords, in this example its "#spplewatchseries4", gathering 5000 tweets-------------
#----------------------------------usually 5000 tweets is an upper limit, you wouldn't get that many-----------------
mytweets<- rtweet::search_tweets(q = "#applewatchseries4", n = 5000)
#head(mytweets$screen_name)
#head(mytweets$screen_name)
#length(unique(mytweets$location))
#head(mytweets$text)


#----------------------------saving as CSV, this is unclean raw data, we use this for sentiment analysis------------------------------------------------
mytweets_subset <- mytweets[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
write.csv(mytweets_subset, file= "Rawdata_Applewatch.csv", sep = " ",row.names = FALSE, col.names = TRUE)



#--------------------------------Convert tweets to dataframe-----------------------------------------------
MyNewData <-as.data.frame(mytweets)
MyNewData$tweets_clean <-MyNewData$text 


#------------------------------------Text cleaning---------------------
#convert to lower
MyNewData$tweets_clean<- tolower(MyNewData$tweets_clean)
# Replace blank space (“rt”)
MyNewData$tweets_clean <- gsub("rt", "", MyNewData$tweets_clean)
#Replace @UserName
MyNewData$tweets_clean <- gsub("@\\w+", "", MyNewData$tweets_clean)
# Remove punctuation
MyNewData$tweets_clean <- gsub("[[:punct:]]", "", MyNewData$tweets_clean)
#Remove links
MyNewData$tweets_clean <- gsub("http\\w+", "", MyNewData$tweets_clean)
#Remove links
MyNewData$tweets_clean <- gsub("https\\w+", "", MyNewData$tweets_clean)
# Remove tabs
MyNewData$tweets_clean <- gsub("[ |\t]{2,}", "", MyNewData$tweets_clean)
## Remove blank spaces at the beginning
#MyNewData$tweets_clean <- gsub("^ ", "", MyNewData$tweets_clean)
## Remove blank spaces at the end
#MyNewData$tweets_clean <- gsub(" $", "", MyNewData$tweets_clean)
#remove digits
MyNewData$tweets_clean = gsub("[[:digit:]]", "", MyNewData$tweets_clean)
#Removes english and keeps only other language texts
#MyNewData$tweets_clean != gsub(pattern = '[[^a-zA-Z0-9\\s]+', replacement = " ", MyNewData$tweets_clean)
#Removes non english and keeps english text 
MyNewData$tweets_clean<- gsub("[^\x20-\x7E]", "", MyNewData$tweets_clean)


#--------------Converts dataframe to Corpus ------------------------------------------------------------------------
myCorpus <- tm::Corpus(tm::VectorSource(as.character(MyNewData$tweets_clean)))
#Removes stopwords
mystopwords <- c(tm::stopwords("english"),"rt","apple","applewatch","applewatchseries","watchapplewatch","applewatchs","watch")
myCorpus <- tm::tm_map(myCorpus,tm::removeWords, mystopwords)
#Create Term Document matrices stemming removes last one or two words from the text-------------------------
#myCorpus <- tm::tm_map(myCorpus, tm::stemDocument)
#tm::inspect(myCorpus)


write.csv(myCorpus, file= "Sentiment_Analysis_corpusdata.csv", sep = " ",row.names = FALSE, col.names = TRUE)
dtm1 <- tm::DocumentTermMatrix(myCorpus)


tdm <- tm::TermDocumentMatrix(myCorpus)
#tdm
#tm::inspect(tdm[1:5, 1:20])
#-------So, its a correct interpretation of this to say if sparse is equal to .99, then we are removing terms that only appear in at most 1% of the data?

#tdm89 <- removeSparseTerms(tdm, 0.89) # removing terms that only appear in atmost 11% of data
#tdm90  <- removeSparseTerms(tdm, 0.9) # removing terms that only appear in atmost 10% of data
#tdm91 <- removeSparseTerms(tdm, 0.91) # removing terms that only appear in atmost 9% of data
#tdm92 <- removeSparseTerms(tdm, 0.92) # removing terms that only appear in atmost 8% of data
tdm99 <- tm::removeSparseTerms(tdm, 0.99) # removing terms that only appear in atmost 1% of data

tdm99$i
tdm99$j
tdm99$v
tdm99$nrow
tdm99$ncol
tdm99$dimnames$Terms
tdm99$dimnames$Docs

dtm = as.matrix(tdm99)
dtm[dtm >= 1] = 1
dtm = dtm %*% t(dtm)
dtm[5:10,5:10]


#--------------Kmeans---------------------------------------------------------------------------
k2 <- kmeans(dtm, centers = 2, nstart = 25)
k3 <- kmeans(dtm, centers = 3, nstart = 25)
k4 <- kmeans(dtm, centers = 4, nstart = 25)
k5 <- kmeans(dtm, centers = 5, nstart = 25)
k10 <- kmeans(dtm, centers = 10, nstart = 25)
k30 <- kmeans(dtm, centers = 30, nstart = 25)
#Plot clusters on fviz
factoextra::fviz_cluster(k2, data = dtm, main = "Kmeans: 2 cluster")
factoextra::fviz_cluster(k3, data = dtm, main = "Kmeans: 3 cluster")
factoextra::fviz_cluster(k4, data = dtm, main = "Kmeans: 4 cluster")
factoextra::fviz_cluster(k5, data = dtm, main = "Kmeans: 5 cluster")
factoextra::fviz_cluster(k10, data = dtm, main = "Kmeans: 10 cluster")
factoextra::fviz_cluster(k30, data = dtm, main = "Kmeans: 30 cluster")
#Plot word 
plot(k2$cluster, main = "kmeans: 2 clusters", xlab = "Word representation", ylab = "Clusters")
plot(k3$cluster, main = "kmeans: 3 clusters", xlab = "Word representation", ylab = "Clusters")
plot(k4$cluster, main = "kmeans: 4 clusters", xlab = "Word representation", ylab = "Clusters")
plot(k5$cluster, main = "kmeans: 5 clusters", xlab = "Word representation", ylab = "Clusters")
plot(k10$cluster, main = "kmeans: 10 clusters", xlab = "Word representation", ylab = "Clusters")
plot(k30$cluster, main = "kmeans: 30 clusters", xlab = "Word representation", ylab = "Clusters")

factoextra::fviz_cluster(k2, data = dtm, geom = "point", main = "Kmeans: 2 cluster", ellipse.type = "norm", palette = "jco", ggtheme = ggplot2::theme_minimal())
factoextra::fviz_cluster(k3, data = dtm, geom = "point", main = "Kmeans: 3 cluster", ellipse.type = "norm", palette = "jco", ggtheme = ggplot2::theme_minimal())
factoextra::fviz_cluster(k4, data = dtm, geom = "point", main = "Kmeans: 4 cluster", ellipse.type = "norm", palette = "jco", ggtheme = ggplot2::theme_minimal())
factoextra::fviz_cluster(k5, data = dtm, geom = "point", main = "Kmeans: 5 cluster", ellipse.type = "norm", palette = "jco", ggtheme = ggplot2::theme_minimal())
factoextra::fviz_cluster(k10, data = dtm, geom = "point", main = "Kmeans: 10 cluster", ellipse.type = "norm", palette = "jco", ggtheme = ggplot2::theme_minimal())
factoextra::fviz_cluster(k30, data = dtm, geom = "point", main = "Kmeans: 30 cluster", ellipse.type = "norm", palette = "jco", ggtheme = ggplot2::theme_minimal())

#-------------dtm: scale the data------------------------------------------------------------------------
dtm.scaled <- scale(dtm)
km.res2 <- kmeans(dtm.scaled, 2, nstart = 10)
factoextra::fviz_cluster(km.res2, dtm, ellipse.type = "norm")
km.res3 <- kmeans(dtm.scaled, 3, nstart = 10)
factoextra::fviz_cluster(km.res3, dtm, ellipse.type = "norm")
km.res4 <- kmeans(dtm.scaled, 4, nstart = 10)
factoextra::fviz_cluster(km.res4, dtm, ellipse.type = "norm")
km.res5 <- kmeans(dtm.scaled, 5, nstart = 10)
factoextra::fviz_cluster(km.res5, dtm, ellipse.type = "norm")
km.res10 <- kmeans(dtm.scaled, 10, nstart = 10)
factoextra::fviz_cluster(km.res10, dtm, ellipse.type = "norm")
km.res30 <- kmeans(dtm.scaled, 30, nstart = 10)
factoextra::fviz_cluster(km.res30, dtm, ellipse.type = "norm")
#----------different plot:  Change the color palette and theme------------------------------------------
km.res2 <- kmeans(dtm.scaled, 2, nstart = 10)
factoextra::fviz_cluster(km.res2, dtm, palette = "Set1", ggtheme = ggplot2::theme_minimal())
km.res3 <- kmeans(dtm.scaled, 3, nstart = 10)
factoextra::fviz_cluster(km.res3, dtm, palette = "Set1", ggtheme = ggplot2::theme_minimal())
km.res4 <- kmeans(dtm.scaled, 4, nstart = 10)
factoextra::fviz_cluster(km.res4, dtm, palette = "Set1", ggtheme = ggplot2::theme_minimal())
km.res5 <- kmeans(dtm.scaled, 5, nstart = 10)
factoextra::fviz_cluster(km.res5, dtm, palette = "Set1", ggtheme = ggplot2::theme_minimal())
km.res10 <- kmeans(dtm.scaled, 10, nstart = 10)
factoextra::fviz_cluster(km.res10, dtm, palette = "Set1", ggtheme = ggplot2::theme_minimal())
km.res30 <- kmeans(dtm.scaled, 30, nstart = 10)
factoextra::fviz_cluster(km.res30, dtm, palette = "Set1", ggtheme = ggplot2::theme_minimal())
#------------PAM cluster------------------------------------------------------------------------
pam.res2 <- cluster::pam(dtm.scaled, 2)
factoextra::fviz_cluster(pam.res2, ellipse.type = "norm")
pam.res3 <- cluster::pam(dtm.scaled, 3)
factoextra::fviz_cluster(pam.res3, ellipse.type = "norm")
pam.res4 <- cluster::pam(dtm.scaled, 4)
factoextra::fviz_cluster(pam.res4, ellipse.type = "norm")
pam.res5 <- cluster::pam(dtm.scaled, 5)
factoextra::fviz_cluster(pam.res5, ellipse.type = "norm")
pam.res10 <- cluster::pam(dtm.scaled, 10)
factoextra::fviz_cluster(pam.res10, ellipse.type = "norm")
pam.res30 <- cluster::pam(dtm.scaled, 30)
factoextra::fviz_cluster(pam.res30, ellipse.type = "norm")



#-------------------silhouette-------------------------------------------------------------------
avg_sil <- function(k) {
  km.res <- kmeans(dtm, centers = k, nstart = 25)
  ss <- cluster::silhouette(km.res$cluster, dist(dtm))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 50
k.values <- 2:10
# extract avg silhouette for 2-15 clusters
avg_sil_values <- purrr::map_dbl(k.values, avg_sil)
plot(k.values, avg_sil_values, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters K", ylab = "Average Silhouettes")
#plot
factoextra::fviz_nbclust(dtm, kmeans, method = "silhouette")

#-------------------OR use this for elbow-----------------------------------------------------
set.seed(123)
factoextra::fviz_nbclust(dtm,kmeans, method = "wss")
#------------------OR Gap statistic method----------------------------------------------------
set.seed(123)
gap_stat <- cluster::clusGap(dtm, FUN = kmeans, nstart = 25, K.max = 15, B = 5)
print(gap_stat, method = "firstmax")
factoextra::fviz_gap_stat(gap_stat)



#-----------hiearichal clustering-------------------------------------------------------------------------------------------------
h_clust <- factoextra::eclust(dtm, "hclust", k = 2, hc_metric = "euclidean", hc_method = "ward.D2", graph = FALSE)
#unrooted
plot(phylogram::as.phylo(h_clust), type = "unrooted", cex = 0.7, no.margin = TRUE)
# Default plot
plot(h_clust)
rect.hclust(h_clust, k=3, border = 2:6)
abline(h = 3, col = 'red')

# Put the labels at the same height: hang = -1
plot(h_clust, hang = -1, cex = 0.6)
# Convert hclust into a dendrogram and plot
h_clust <- as.dendrogram(h_clust)

# Default plot
#plot(h_clust, type = "rectangle", ylab = "Height")

# Zoom in to the first dendrogram
#plot(hcd.2, xlim = c(1, 20), ylim = c(1,8))
#Fan
plot(phylogram::as.phylo(h_clust), type = "fan", edge.col=c("red","green","blue"))
# Radial
plot(phylogram::as.phylo(h_clust), type = "radial", edge.col=c("red","green","blue"))
# Cut the dendrogram into 4 clusters
#colors = c("red", "blue", "green", "black")
#clus4 = stats::cutree(h_clust, 4)
#plot(phylogram::as.phylo(h_clust), type = "fan", tip.color = colors[clus4],
# label.offset = 1, cex = 0.7)



#------------------Network graph--------------------------------------------------------------
g <- igraph::graph.adjacency(dtm, weighted=T, mode = "undirected")
g <- igraph::simplify(g)
igraph::V(g)$label <- igraph::V(g)$name
igraph::V(g)$degree <- igraph::degree(g)

set.seed(3952)
layout1 <- igraph::layout.fruchterman.reingold(g)
palf <- colorRampPalette(c("gray70", "dark red", "orange"))



#--------------plot network graph-----------------
plot(g, layout=layout1)
plot(g, layout=layout.kamada.kawai)
#-------------------------------------------------
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .7, .9)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
# plot the graph in layout1
plot(g, layout=layout1)




#------------identifying frequent terms---------------
freq.terms <- tm::findFreqTerms(tdm, lowfreq = 50)
term.freq_tdm <- rowSums(as.matrix(tdm))
term.freq_tdm <- subset(term.freq_tdm, term.freq_tdm >= 50)
term_freq_df_tdm <- data.frame(term = names(term.freq_tdm), freq = term.freq_tdm)
term_freq_df_tdm


#-----------plot the frequent terms ---------------------
library(ggplot2)
ggplot(term_freq_df_tdm, aes(x=term, y=freq))  + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))


#--------------Word cloud--------------------------------
m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]
# plot word cloud
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10,
          random.order = F, colorPalette = "Set3")




#--------------------Topic modeling using LDA algorithm-------------------------------------
#inspect (dtm[1:5, 1:20])
#tdm <- tm::TermDocumentMatrix(myCorpus)
library(topicmodels)
#lda <- topicmodels::LDA(dtm1, k=8)
rowTotals <- apply(dtm1 , 1, sum)
dtm.new   <- dtm1[rowTotals> 0, ] 
lda <- topicmodels::LDA(dtm.new, k=8)
term <- terms(lda, 7)
burnin <-1000
iter<-2000
thin <- 500
nstart <-5
seed <- list(254672,109,122887,145629037,2)
best <-TRUE
k <-5
ldaOut <- topicmodels::LDA(dtm.new,k,method = "Gibbs", control = list(nstart=nstart, seed = seed, best = best, burnin=burnin, iter=iter, thin=thin))
topicmodels::terms(ldaOut,6)
topicmodels::topics(ldaOut)
topicProbabilities <- as.data.frame(ldaOut@gamma)
summary(topicProbabilities)




