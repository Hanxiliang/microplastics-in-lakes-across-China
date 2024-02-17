# # Introduction
#### This document contains R code used to network analysis based on microplastic community and Linear discriminant analysis (LDA) analysis for the paper "Human footprint dominates the distribution, sources, and ecological risk of microplastics in lakes across China". 

# ----------------------------------------------------------------------------------------------------
# 1.1 Network analysis based on microplastic community

## Install packages and call libraries
install.packages("igraph")
install.packages("vegan")
install.packages("psych")
library(psych)
library(igraph)
library(vegan)

## Upload dataset
otu<-read.csv("OTU_Polymer.csv",row.names = 1)
otu<-as.data.frame(lapply(otu,as.numeric))

## Screening for polymer types occurring in more than half (51) of the 102 lakes in China
otu1<-otu
otu1[otu1>0]<-1
out<-otu[which(rowSums(otu1)>=51), ]


## Computation of similarity matrix based on "1 - Bray-Curits"
otu<-t(otu)
comm_sim<-1-as.matrix(vegan::vegdist(otu,method="bray"))
diag(comm_sim)<-0

## Select the results for p>0.05 and assign the values that do not satisfy the condition to 0
p<-comm_sim
p[p<=0.5]<-0

## Constructing igraph objects
library(igraph)
g1<-graph_from_adjacency_matrix(p,weighted=TRUE,mode="undirected",diag=FALSE)
g1
E(g1)$weight

## Creation of the side file
edge<-data.frame(as_edgelist(g1,names=TRUE))
df<-as.data.frame(E(g1)$weight)
df[df>0]<-1
df[df<0]<- -1
colnames(df)<-(("p"))
edge<-data.frame(source=edge[[1]],target=edge[[2]],weight=E(g1)$weight,correlation=E(g1)$weight)
edge

## Output the side file and then analyze it next with Gephi (version 0.9.2)
write.csv(edge,"network.edge.csv",row.names=F)

# ----------------------------------------------------------------------------------------------------
# 1.2 Linear discriminant analysis (LDA) analysis for microplastic polymers

## Install packages and call libraries
install.packages("MASS")
library(MASS)
library(ggplot2)


## Upload dataset
otu<-read.csv(csvpath)
otu<-read.csv("LDA.csv",header=TRUE)
otu

## Normality test 
test<-with(otu,tapply(CP,Category,shapiro.test))
test

test<-with(otu,tapply(PET,Category,shapiro.test))
test

## Linear discriminant analysis (LDA) analysis for microplastic polymers
model<-lda(Category~., data=otu)
model

## Plot the figure
windowsFonts(TNM=windowsFont("Times New Roman"))
palette = c("#9ed048", "#1685a9", "#ed5736")
p1<-ggplot(cbind(otu,predict(model)$x,habillage = otu$Category,ellipse.type = "convex"),aes(LD1,LD2,color=Category))+
  geom_point(size = 0.9,shape=19,alpha=0.7)+
  theme_light()+
  scale_shape_manual(values = c(1:25))+
  stat_ellipse(level=0.95,show.legend=FALSE)+
  theme_bw()

## Output the figure
png(filename="Extentaded Data Figure 2b.png",width=6000,heigh=2500,units="px",res=900)