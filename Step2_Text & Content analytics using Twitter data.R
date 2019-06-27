library(stringr)
library(twitteR)
library(RColorBrewer)
library(qdapDictionaries)
library(readxl)
library(gdata)
library(igraph)
library(anocva)
library(MASS)
library(ade4)
library(phylogram)
library(devtools)
install_github("cran/fpc")
library(DiagrammeR)
library(reshape2)
library(linkcomm)

#load raw data from step1

MyData1<- read.csv("Rawdata_Applewatch.csv", header=TRUE, sep=",")
MyNewData1 <- as.data.frame(MyData1)
MyNewData1$tweets_clean <-MyNewData1$text 
#------------------------------------Text cleaning---------------------
#convert to lower
MyNewData1$tweets_clean<- tolower(MyNewData1$tweets_clean)
# Replace blank space (“rt”)
MyNewData1$tweets_clean <- gsub("rt", "", MyNewData1$tweets_clean)
#Replace @UserName
MyNewData1$tweets_clean <- gsub("@\\w+", "", MyNewData1$tweets_clean)
# Remove punctuation
MyNewData1$tweets_clean <- gsub("[[:punct:]]", "", MyNewData1$tweets_clean)
#Remove links
MyNewData1$tweets_clean <- gsub("http\\w+", "", MyNewData1$tweets_clean)
#Remove links
MyNewData1$tweets_clean <- gsub("https\\w+", "", MyNewData1$tweets_clean)
# Remove tabs
MyNewData1$tweets_clean <- gsub("[ |\t]{2,}", "", MyNewData1$tweets_clean)
## Remove blank spaces at the beginning
MyNewData1$tweets_clean <- gsub("^ ", "", MyNewData1$tweets_clean)
## Remove blank spaces at the end
MyNewData1$tweets_clean <- gsub(" $", "", MyNewData1$tweets_clean)
#remove digits
MyNewData1$tweets_clean = gsub("[[:digit:]]", "", MyNewData1$tweets_clean)
#Removes english and keeps only other language texts
MyNewData1$tweets_clean != gsub(pattern = '[[^a-zA-Z0-9\\s]+', replacement = " ", MyNewData1$tweets_clean)
#Removes non english and keeps english text 
MyNewData1$tweets_clean<- gsub("[^\x20-\x7E]", "", MyNewData1$tweets_clean)

#--------------Converts dataframe to Corpus 
myCorpus <- tm::Corpus(tm::VectorSource(as.character(MyNewData1$tweets_clean)))
#Removes stopwords
mystopwords <- c(tm::stopwords("english"),"rt","apple","applewatch","applewatchseries","watchapplewatch","applewatchs","watch")
myCorpus <- tm::tm_map(myCorpus,tm::removeWords, mystopwords)

#Create Term Document matrices 
myCorpus <- tm::tm_map(myCorpus, tm::stemDocument)
#tm::inspect(myCorpus)
dtm1 <- tm::DocumentTermMatrix(myCorpus)
#dtm
#inspect (dtm[1:5, 1:20])
tdm <- tm::TermDocumentMatrix(myCorpus)
#tdm
#tm::inspect(tdm[1:5, 1:20])
#So, is a correct interpretation of this to say if sparse is equal to .99, then we are removing terms that only appear in at most 1% of the data?

#tdm89 <- removeSparseTerms(tdm, 0.89) # removing terms that only appear in atmost 11% of data
#tdm90  <- removeSparseTerms(tdm, 0.9) # removing terms that only appear in atmost 10% of data
#tdm91 <- removeSparseTerms(tdm, 0.91) # removing terms that only appear in atmost 9% of data
#tdm92 <- removeSparseTerms(tdm, 0.92) # removing terms that only appear in atmost 8% of data
tdm98 <- tm::removeSparseTerms(tdm, 0.98) # removing terms that only appear in atmost 1% of data

tdm98$i
tdm98$j
tdm98$v
tdm98$nrow
tdm98$ncol
tdm98$dimnames$Terms
tdm98$dimnames$Docs

dtm = as.matrix(tdm98)
dtm[dtm >= 1] = 1
dtm = dtm %*% t(dtm)
dtm[5:10,5:10]
#g<- igraph::graph(edges = tdm99$i, tdm99$j)

#--------------word data as a graph with edges and vertices---------

tm::inspect(myCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

g <- igraph::graph.data.frame(subset(reshape2::melt(d, value.name = "width"), !!width), directed = FALSE)
head(g)
igraph::V(g)$color <- rep(2:3, dim(d))
plot(g)





d.word <- data.frame(d$word)#converting matrix into single column dataframe
d.freq <- data.frame(d$freq)#converting matrix into single column dataframe
#actors <- data.frame(words = c(d$word))
#relations <- data.frame(freqs = c(d$freq))
g <- igraph::graph_from_data_frame(d) #making d a graph

print(g, e=TRUE, v=TRUE)
plot(g)#dirty graph 

igraph::gorder(g)#The number of vertices in a graph is the order of the graph.
igraph::gsize(g)# size of graph
igraph::V(g) #vertices of graph
igraph::E(g) #edges of graph
igraph::is_connected(g, mode = "weak")
igraph::is_connected(g, mode = "strong")


#-------Modularity----------
#We can use modularity to decide which of the many partitions predicted by a hierarchical method offers the best community structure, selecting the one for which M is maximal
#convert g graph to undirected to detect modularity---------
g <- igraph::as.undirected(g)

#------------------------------ Girvan-Newman--------------------------------------
girvNew <- igraph::cluster_edge_betweenness(g, modularity = TRUE)
girvNew_sizesComm <- igraph::sizes(girvNew)
girvNew_numComm <- length(girvNew_sizesComm)
girvNew_modularity <- igraph::modularity(girvNew)
print(girvNew_numComm)
print(girvNew_sizesComm)
print(girvNew_modularity)
girvNew_den <- as.dendrogram(girvNew)
plot(girvNew_den)
plot(girvNew_den, type = "rectangle", ylab = "Height")
plot(phylogram::as.phylo(girvNew_den), type = "fan")
plot(phylogram::as.phylo(girvNew_den), type = "radial")
#-----------------------------fast greedy-----------------------------------------
fastgreedy <- igraph::fastgreedy.community(g)
fastgreedy_sizesComm <- igraph::sizes(fastgreedy)
fastgreedy_numComm <- length(fastgreedy_sizesComm)
fastgreedy_modularity <- igraph::modularity(fastgreedy)
print(fastgreedy_numComm)
print(fastgreedy_sizesComm)
print(fastgreedy_modularity)
fastgreedy_den <- as.dendrogram(fastgreedy)
plot(fastgreedy_den)
plot(fastgreedy_den, type = "rectangle", ylab = "Height")
plot(phylogram::as.phylo(fastgreedy_den), type = "fan")
plot(phylogram::as.phylo(fastgreedy_den), type = "radial")
#-----------------------------------Walk trap-------------------------------------
walktrap <- igraph::cluster_walktrap(g)
walktrap_sizesComm <- igraph::sizes(walktrap) 
walktrap_numComm <- length(walktrap_sizesComm)
walktrap_modularity <- igraph::modularity(walktrap)
print(walktrap_numComm)
print(walktrap_sizesComm)
print(walktrap_modularity)
walktrap_den <- as.dendrogram(walktrap)
plot(walktrap_den)
plot(walktrap_den, type = "rectangle", ylab = "Height")
plot(phylogram::as.phylo(walktrap_den), type = "fan")
plot(phylogram::as.phylo(walktrap_den), type = "radial")
#-----------------------------------Louvain Algorithm-----------------------------
louvain <- igraph::cluster_louvain(graph = g, weights = NULL)
louvain_sizesComm <- igraph::sizes(louvain)
louvain_numComm <- length(louvain_sizesComm)
louvain_modularity <- igraph::modularity(louvain)
print(louvain_numComm)
print(louvain_sizesComm)
print(louvain_modularity)
louvain_den <- as.dendrogram(walktrap)
plot(louvain_den)
plot(louvain_den, type = "rectangle", ylab = "Height")
plot(phylogram::as.phylo(louvain_den), type = "fan")
plot(phylogram::as.phylo(louvain_den), type = "radial")

#----------------------------------Summary------------------------------------------
print("the num of community detected by the 4 modularity algorithms")
print(paste0("number of community in Girvan-Newman is = ", girvNew_numComm))
print(paste0("number of community in fast greedy is = ", fastgreedy_numComm))
print(paste0("number of community in Walk trap is = ", walktrap_numComm))     
print(paste0("number of community in Louvain Algorithmy is = ", louvain_numComm))
#------------------------------------------------------------------------------------
print("community sizes are as below")
print(girvNew_sizesComm)
print (fastgreedy_sizesComm)
print (walktrap_sizesComm)   
print(louvain_sizesComm)
#------------------------------------------------------------------------------------    
print("the best partition is provided by the one with higher modularity value")
print(paste0("Modularity by Girvan-Newman = ", girvNew_modularity))
print(paste0("Modularity by fast greedy = ", fastgreedy_modularity))
print(paste0("Modularity by Walk trap = ", walktrap_modularity))     
print(paste0("Modularity byLouvain Algorithmy = ", louvain_modularity))


#-------------------------------Spectral algorithm-----------------------------------
#Here we explicitly implement the idea of computing the leading eigenvector of modularity matrix B.
Spectral.A <- igraph::as_adjacency_matrix(g)
Spectral.A <- as.matrix(Spectral.A)
Spectral.k <- colSums(Spectral.A)
Spectral.m <- sum(Spectral.k)/2
Spectral.Km <- Spectral.k%*%t(Spectral.k)
Spectral.B <- Spectral.A-Spectral.Km/(2*Spectral.m)
Spectral.r <- eigen(Spectral.B)
Spectral.u <- Spectral.r$vectors[,1]
Spectral.wcr <- list(which(Spectral.u>0),which(Spectral.u<0))
plot(g,mark.groups = Spectral.wcr)
#To compute modularity Q:
Spectral.s <- Spectral.u
Spectral.s[which(Spectral.s<0)] <- -1
Spectral.s[which(Spectral.s>0)] <- 1
print(Spectral.Q <- sum(Spectral.B*(Spectral.s%*%t(Spectral.s)+1))/(4*Spectral.m))
#Calling community detection algorithms in igraph is straightforward. 
#Function cluster_leading_eigen implements the spectral algorithm in PRE06, and function 
#modularity directly gives out the Q value for a partition. 
#The implementation considers further division thus returns four communities, and the Q value is higher.
Spectral.wc <- cluster_leading_eigen(g)
Spectral.wc <- igraph::cluster_leading_eigen(g)
igraph::membership(Spectral.wc)
igraph::modularity(Spectral.wc)
plot(Spectral.wc, g)