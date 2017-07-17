install.packages("ABC") # This will install the package --ABC--. 
install.packages("network")
install.packages("igraph")
install.packages("plyr")
install.packages("dplyr")

library(ABC) # Load the package --ABC-- to your workspace
library(igraph)
library(plyr)
library(network)
library(plyr)
library(dplyr)



getwd() # Shows the working directory (wd)
dir.create("C:/test") # Creates folder 'test' in drive 'c:'
setwd(choose.dir()) # Select the working directory interactively
setwd("C:/test") # Changes the working directory to "c:/test"


fix(mydata) # Rename interactively
names(mydata)[3] <- "First"

#rm(list = ls())
#rm(list = ls(pattern = "^lay")) 

#GDentNode<-read.csv(file.choose(), header=TRUE)
#GDentEdge<-read.csv(file.choose(), header=TRUE)
#Rgraph<- graph.data.frame(GDentEdge, directed = F, vertices=GDentNode)

#Data from *.csv (interactively)
Testdatacsv <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE)
RDF <- data.frame(Testdatacsv) 
RDF$fname <- gsub("([A-Za-z]+).*", "\\1", RDF$Name)
#RDF$fname <- gsub("([A-Za-z]+).*", "\\1", RDF$OName)


RDF1<-cbind.data.frame(RDF$ID, RDF$RE_RelID, RDF$RE_reltype, stringsAsFactors=FALSE)
colnames(RDF1)<-c("ID", "RE_relatID", "RE_reltype")
RDF1$ID<-as.character(RDF1$ID)
sapply(RDF1, class)
unique(RDF1$ID)

RDF2<-setNames(RDF1, c("RE_relatID", "RE_relatID2", "RE_reltype2")) 
RDF2<-full_join(RDF1,RDF2)%>% filter(ID != RE_relatID2)%>% filter(RE_reltype2 != "Classmate")
RDF2<-full_join(RDF1,RDF2)%>% filter(ID != RE_relatID2)


RDF2 %>% arrange(ID, RE_relatID, RE_relatID2)
RDF2$Relationship <- paste(RDF2$ID,RDF2$RE_relatID,RDF2$RE_relatID2,sep=' --> ')
sapply(RDF2, class)

RDF1$fname <- gsub("([A-Za-z]+).*", "\\1", RDF$name)
#RDF1$fname <- gsub("([A-Za-z]+).*", "\\1", RDF$OName)
RDF1$order <- 1 
RDF3 <- RDF1[,c(2,1,3)]
RDF3$fname <- gsub("([A-Za-z]+).*", "\\1", RDF$RE_Name)
RDF3<-setNames(RDF3, c("ID", "RE_relatID", "RE_reltype", "fname"))  
RDF3$order <- 2  
RDF3 <- rbind(RDF3, RDF1) 
unique(RDF3$ID)

RDF4<-merge(RDF3, RDF2)
RDF4<-full_join (RDF3, RDF2)
RDF4$Relationship1 <- paste(RDF4$ID,RDF4$RE_relatID,sep=' --> ')

#create social network -igraph object
Rgraph<-graph_from_data_frame(RDF, directed=T)
Rgraph<-simplify(Rgraph,remove.multiple=F, remove.loops=T)

#generate network layout -coordinates based on different layouts
layFR <- layout.fruchterman.reingold(Rgraph)
plot(Rgraph,layout=layFR)
colnames(layFR)<-c("XCOO1","YCOO1")


layKK <- layout.kamada.kawai(Rgraph)
plot(Rgraph,layout=layKK)
colnames(layKK)<-c("XCOO2","YCOO2")

layLGL<-layout.lgl(Rgraph)
plot(Rgraph,layout=layLGL)
colnames(layLGL)<-c("XCOO3","YCOO3")

layCIR<-layout_in_circle(Rgraph)
plot(Rgraph,layout=layCIR)
colnames(layCIR)<-c("XCOO4","YCOO4")

laySPH<-layout_on_sphere(Rgraph)
plot(Rgraph,layout=laySPH)
colnames(laySPH)<-c("XCOO5","YCOO5", "ZCOO5")

laySTA<-layout_as_star(Rgraph)
plot(Rgraph,layout=laySTA)
colnames(laySTA)<-c("XCOO6","YCOO6")

layNIC<-layout_nicely(Rgraph)
plot(Rgraph,layout=layNIC)
colnames(layNIC)<-c("XCOO7","YCOO7")

layGRI<-layout_on_grid(Rgraph)
plot(Rgraph,layout=layGRI)
colnames(layGRI)<-c("XCOO8","YCOO8")

layRAN<-layout_randomly (Rgraph)
plot(Rgraph,layout=layRAN)
colnames(layRAN)<-c("XCOO9","YCOO9")

layDH<-layout_with_dh (Rgraph)
plot(Rgraph,layout=layDH)
colnames(layDH)<-c("XCOO10","YCOO10")

layDRL<-layout_with_drl (Rgraph)
plot(Rgraph,layout=layDRL)
colnames(layDRL)<-c("XCOO11","YCOO11")

layGEM<-layout_with_gem (Rgraph)
plot(Rgraph,layout=layGEM)
colnames(layGEM)<-c("XCOO12","YCOO12")

layGRA<-layout_with_graphopt(Rgraph)
plot(Rgraph,layout=layGRA)
colnames(layGRA)<-c("XCOO13","YCOO13")

layMDS<-layout_with_mds(Rgraph)
plot(Rgraph,layout=layMDS)
colnames(layMDS)<-c("XCOO14","YCOO14")

layTRE<-layout_as_tree (Rgraph)
plot(Rgraph,layout=layTRE)
colnames(layTRE)<-c("XCOO15","YCOO15")

#check number of nodes and edges
#number of nodes
vcount(Rgraph)
#number of edges
ecount(Rgraph)


#derive network centrality measures

# Activity: Degree (total degree)
deg <- degree(Rgraph, mode="all")
#indeg<-degree(Rgraph, mode="in")
# outdeg<-degree(Rgraph, mode="out")
#Efficiency: Closeness
clos<-closeness(Rgraph, mode="all")
#Control: Betweenness
betw<-betweenness(Rgraph)
#Overall: Eigenvector
eigc<-eigen_centrality(Rgraph)
# Edge Betweenness
ebetw<-edge.betweenness(Rgraph)
#page rank
prank <-page_rank(Rgraph)$vector

c<-cbind(betw, eigc$vector, clos, deg, layCIR, layDH, layDRL, layFR, layGEM, layGRA, layGRI, layKK, layLGL, layMDS, layNIC, layRAN, laySPH, laySTA,  data.frame(ID=V(Rgraph)$name))
graphD<-data.frame(c)
RgraphD<-merge(RDF4, graphD)

save(RgraphD, file = "RgraphD.rdata")






#remove labels
V(Rgraph)$label <- NA
plot(Rgraph,layout=layFR, edge.color="gray30", vertex.color="red", vertex.size=5)
hist(deg)
Rgraph

#write.csv(df, file="out.csv", row.name=FALSE)


RgraphD2 <- get.adjacency(RgraphD1)
RgraphD2 <- RgraphD2 %*% RgraphD2        # Rgraph2 contains 2-walks
diag(RgraphD2) <- 0        # take out loops
RgraphD2[RgraphD2!=0] <- 1 # normalize RgraphD2, not interested in multiplicity of walks
RgraphD2 <- graph.adjacency(RgraphD2)
RDF2 <-get.data.frame(RgraphD2)
colnames(RDF2)<-c("ID","RE_relatID")
RDF2$Relationship <- paste(RDF2$ID,RDF2$RE_relatID,sep=' --> ')
RDF2$Order<- 2

RDF<-rbind.fill(RDF2, RDF1)

adj <- get.adjlist(RgraphD1)
out <- file("adj.txt", open="w")
cat("Name\tNo of neighbors\tList of neighbors\n", file=out)
lapply(seq_along(adj), function(x) { 
  cat(x, "\t", length(adj[[x]]), "\t\t", sep="", file=out)
  cat(adj[[x]], sep="  ", file=out)
  cat("\n", file=out)
})
close(out)

edgeDF <- rbind(edges, edges[,c(2,1)]) %>% 
  as.data.frame() %>% 
  setNames(c("NodeId", "FirstConnection"))
edgeDF %>% arrange(NodeId, FirstConnection)

SecondCon <- setNames(edgeDF, c("FirstConnection", "SecondConnection")) %>% 
  full_join(edgeDF) %>% filter(NodeId != SecondConnection)

SecondCon %>% arrange(NodeId, FirstConnection, SecondConnection)