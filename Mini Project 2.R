# read data from COW trade
setwd("~/Desktop/R practice/COW_Trade_4.0")
library(foreign)
trade <- read.csv("Dyadic_COW_4.0.csv")

# subset data to only include year 2000 and smooth trade larger than 50,000 in million dollars 
trade0 <- subset(trade, year == 2000)
trade1 <- subset(trade0, smoothtotrade >= 50000.00)

View(trade2)

# cleaning data
library(data.table)

trade1<-data.table(trade1)
colnames(trade1)
trade1<-trade1[,c(4,5)]
colnames(trade1)<-c("importer1","importer2")
trade1<-data.frame(lapply(trade1, gsub, pattern = "trade", replacement = ""))


#basic graph
library(igraph)
g = graph_from_data_frame(trade1,directed = F)

plot(g, vertex.label.color = "black", layout = layout_nicely(g))

#graph properties
V(g)
E(g)
gsize(g)
gorder(g)

#calculate degree, betweeness, closeness and eigenvector
g.d <- degree(g)
g.b <- betweenness(g)
g.c <- closeness(g)
g.e <- eigen_centrality(g,directed=F)

#assign node color to clusters
sizes(fastgreedy.community(g))
i <-membership(fastgreedy.community(g))
g <- set_vertex_attr(g, "color", value = c("red", "green", "yellow", "dodgerblue")[i])

#New plot based on different centrality measures
par(mfrow=c(2, 2))
summary(degree(g))
V(g)$label <-ifelse(degree(g)>=2.8, V(g)$name, NA)
plot(g,
     edge.color = 'black',
     vertex.label.color = "black", 
     vertex.size = sqrt(degree(g))*5,
     edge.width = degree(g)*0.4,
     layout = layout_with_fr(g),
     margin =-0.3,
     main = "Degree")

summary(betweenness(g))
V(g)$label <-ifelse(betweenness(g)>=17, V(g)$name, NA)
plot(g, 
     edge.color = 'black',
     vertex.label.color = "black", 
     vertex.size = sqrt(betweenness(g))+8,
     edge.width = sqrt(betweenness(g))*0.5,
     layout = layout_nicely(g),
     margin =-0.3,
     main = "Betweenness")

summary(closeness(g))
V(g)$label <-ifelse(closeness(g)>=0.019, V(g)$name, NA)
plot(g, 
     edge.color = 'black',
     vertex.label.color = "black", 
     vertex.size = closeness(g)*600,
     edge.width = closeness(g)*150,
     layout = layout_nicely(g),
     margin =-0.3,
     main = "Closeness")

summary(g.e$vector)
V(g)$label <-ifelse(g.e$vector>=0.42, V(g)$name, NA)
plot(g, 
     edge.color = 'black',
     vertex.label.color = "black", 
     vertex.size = (g.e$vector)*20,
     edge.width = sqrt(g.e$vector)*3.5,
     layout = layout_nicely(g),
     margin =-0.3,
     main = "Eigenvector")

# correlation
cor<-cbind(g.d,g.b,g.c,g.e$vector)
colnames(cor)=c("Degree","Betweenness","Closeness","Eigenvector")
cor<-as.data.frame(cor)
cor<-cor(cor)

##Top 6 actors in one table
#create four different df for four measures
a<-as.data.frame(sort(degree(g), decreasing=TRUE)[1:6])
colnames(a)="Degree"
b<-as.data.frame(sort(betweenness(g), decreasing=TRUE)[1:6])
colnames(b)="Betweenness"
c<-as.data.frame(sort(closeness(g), decreasing=TRUE)[1:6])
colnames(c)="Closeness"
d<-as.data.frame(sort(eigen_centrality(g)$vector, decreasing=TRUE)[1:6])
colnames(d)="Eigenvector"

#combine four df into one table
library(plyr)
a$rn <- rownames(a)
b$rn <- rownames(b)
c$rn <- rownames(c)
d$rn <- rownames(d)
Top6 <- join_all(list(a,b,c,d), by = 'rn',type = 'full')
row.names(Top6)=Top6$rn
Top6$rn <- NULL

#scatterplot
par(mfrow=c(2, 3))
corn<-as.data.frame(cor)
plot(corn$Degree,corn$Betweenness)
plot(corn$Degree,corn$Closeness)
plot(corn$Degree,corn$Eigenvector)
plot(corn$Betweenness,corn$Closeness)
plot(corn$Betweenness,corn$Eigenvector)
plot(corn$Closeness,corn$Eigenvector)





### trade relationship in 2008 threshold 50,000 million dollars
trade2 <- subset(trade, year == 2008)
trade2 <- subset(trade2, smoothtotrade >= 50000.00)
# cleaning data
library(data.table)

trade2 <-data.table(trade2)
colnames(trade2)
trade2 <- trade2[,c(4,5)]
colnames(trade2)<-c("importer1","importer2")
trade2 <-data.frame(lapply(trade2, gsub, pattern = "trade", replacement = ""))


#basic graph
library(igraph)
g = graph_from_data_frame(trade2,directed = F)

plot(g, vertex.label.color = "black", layout = layout_nicely(g))

#graph properties
V(g)
E(g)
gsize(g)
gorder(g)

#calculate degree, betweeness, closeness and eigenvector
g.d <- degree(g)
g.b <- betweenness(g)
g.c <- closeness(g)
g.e <- eigen_centrality(g,directed=F)

#assign node color to clusters
sizes(fastgreedy.community(g))
i <-membership(fastgreedy.community(g))
g <- set_vertex_attr(g, "color", value = c("red", "green", "yellow","dodgerblue")[i])

#New plot based on different centrality measures
par(mfrow=c(2, 2))
summary(degree(g))
V(g)$label <-ifelse(degree(g)>=2.8, V(g)$name, NA)
plot(g,
     edge.color = 'black',
     vertex.label.color = "black", 
     vertex.size = sqrt(degree(g))*5,
     edge.width = degree(g)*0.4,
     layout = layout_with_fr(g),
     margin =-0.3,
     main = "Degree")

summary(betweenness(g))
V(g)$label <-ifelse(betweenness(g)>=17, V(g)$name, NA)
plot(g, 
     edge.color = 'black',
     vertex.label.color = "black", 
     vertex.size = sqrt(betweenness(g))+8,
     edge.width = sqrt(betweenness(g))*0.5,
     layout = layout_nicely(g),
     margin =-0.3,
     main = "Betweenness")

summary(closeness(g))
V(g)$label <-ifelse(closeness(g)>=0.019, V(g)$name, NA)
plot(g, 
     edge.color = 'black',
     vertex.label.color = "black", 
     vertex.size = closeness(g)*600,
     edge.width = closeness(g)*150,
     layout = layout_nicely(g),
     margin =-0.3,
     main = "Closeness")

summary(g.e$vector)
V(g)$label <-ifelse(g.e$vector>=0.42, V(g)$name, NA)
plot(g, 
     edge.color = 'black',
     vertex.label.color = "black", 
     vertex.size = (g.e$vector)*20,
     edge.width = sqrt(g.e$vector)*3.5,
     layout = layout_nicely(g),
     margin =-0.3,
     main = "Eigenvector")

# correlation
cor<-cbind(g.d,g.b,g.c,g.e$vector)
colnames(cor)=c("Degree","Betweenness","Closeness","Eigenvector")
cor<-as.data.frame(cor)
cor<-cor(cor)

##Top 6 actors in one table
#create four different df for four measures
a<-as.data.frame(sort(degree(g), decreasing=TRUE)[1:6])
colnames(a)="Degree"
b<-as.data.frame(sort(betweenness(g), decreasing=TRUE)[1:6])
colnames(b)="Betweenness"
c<-as.data.frame(sort(closeness(g), decreasing=TRUE)[1:6])
colnames(c)="Closeness"
d<-as.data.frame(sort(eigen_centrality(g)$vector, decreasing=TRUE)[1:6])
colnames(d)="Eigenvector"

#combine four df into one table
library(plyr)
a$rn <- rownames(a)
b$rn <- rownames(b)
c$rn <- rownames(c)
d$rn <- rownames(d)
Top6 <- join_all(list(a,b,c,d), by = 'rn',type = 'full')
row.names(Top6)=Top6$rn
Top6$rn <- NULL

#scatterplot
par(mfrow=c(2, 3))
corn<-as.data.frame(cor)
plot(corn$Degree,corn$Betweenness)
plot(corn$Degree,corn$Closeness)
plot(corn$Degree,corn$Eigenvector)
plot(corn$Betweenness,corn$Closeness)
plot(corn$Betweenness,corn$Eigenvector)
plot(corn$Closeness,corn$Eigenvector)