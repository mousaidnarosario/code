---
#title: "GOT characters relationship"
#author: "Mousaidna Rosario"
#date: "7/25/2020"
---
  
#This file shows relationship between different Game Of Thrones characters. 

#Reading data from dataset file:
  
installed.packages("igraph") 
library(igraph)
data<- read.csv('got_edges.csv', header=T)


#First six data of Game of Thrones characters dataset

head(data)


#Getting the degree of connections from the dataset...

gtn1 <- graph.data.frame(data, directed=F)
V(gtn1) # vertices of the graph
E(gtn1) # edges of a graph 
V(gtn1)$label <- V(gtn1)$name # pull the names of vertices of the graph

#frequency of connections between vertices
V(gtn1)$degree <- degree(gtn1)

##Plots
set.seed(222)

#getting the groups between data nodes
clr <- c("black, grey, white")
cnet <- cluster_edge_betweenness(gtn1) # density of connection between networks 

# plot usig layout.fruchterman.reingold.
plot(cnet
     ,gtn1
     ,vertex.color= V(gtn1)$degree*0.1 
     ,vertex.label.color= "black"
     ,vertex.size=V(gtn1)$degree*0.5
     ,vertex.label.cex=V(gtn1)$degree*0.05
     ,edge.color= "gray"
     ,edge.curved = 0.2
     ,layout=layout.fruchterman.reingold
     ,main="Game of Thrones Characters Relationship"
)


#layout kamada kawai

plot(cnet
     ,gtn1
     ,vertex.color= V(gtn1)$degree*0.1 
     ,vertex.label.color="black" 
     ,vertex.size=V(gtn1)$degree*0.5
     ,vertex.label.cex=V(gtn1)$degree*0.05
     ,edge.color="gray"
     , edge.curve=1
     ,layout=layout.kamada.kawai
     ,main="Game of Thrones Characters Relationship"
)

par(bg="white")

# interactive plot using networkD3
#use package networkD3

install.packages('networkD3')
library(networkD3)
clr<-c('blue','green')
simpleNetwork(data,fontSize = 16, nodeColour ='blue' ,linkDistance = 50, charge = -30,
               zoom=T, linkColour = "#666",  opacity = 0.7)
