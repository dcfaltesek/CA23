library(sna)
library(ggplot2)
library(dplyr)

#step one: translate into a matrix
C<-network(clean_football, directed=FALSE)
#to access some of this we need a matrix method
smasher<-as.matrix.network.adjacency(C)
#modularities
#louvain- this is a poor implementation of Louvain, but it works with the libraries
louvain_net<-modMax::greedy(smasher)
#kcores - older method of modularity, falls back to degree centrality 
kcores_net<-kcores(C)


#cleans up that dataframe to just get the stuff we need
modularities<-data.frame(louvain_net[3], kcores_net)
colnames(modularities)[1]<-"Louvain"
View(modularities)

#differential between these two methods
ggplot(modularities, aes(Louvain, kcores_net))+geom_jitter()

#lets store a few things that could be useful in the matrix 
#yes matrix world has a v pipe, we can also do it with a vertex attribute call (as we did last time)
C %v% "louvain" <- louvain_net[3]
C %v% "kcores" <- kcores_net


L<-ggnetwork(C, layout = "fruchtermanreingold")
J<-filter(L, vertex.names>75)

ggplot(J, aes(x, y, xend = xend, yend = yend)) +
  #just make the lines orange
  geom_edges(aes(linetype = TYPE))+
  #set the other aesthetics as static values
  geom_nodetext(aes(colour = as.factor(kcores), label = vertex.names))+
  theme_blank()

#further fun
cutpoints(C)
mutuality(C)
court_brokerage<-brokerage(C, modularities$Louvain)

#strengths - larger numbers mean stronger
comm.str(smasher, modularities$Louvain)
comcat(smasher)
impact(smasher)
leverage(smasher)