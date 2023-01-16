library(igraph)

ramayana <- function()
{
  df <- read.csv("C:/Users/hp/Desktop/ramayana.csv", header=F) # create directed graph network
  r <- graph.data.frame(df, directed=T)
  set.seed(222) # setting to have similar pattern graph
       pdf("r1.pdf", "pdf")
       plot(r, main=". Ramayana .  ",
       vertex.color= "darkgoldenrod1",
       vertex.size = 17,
       vertex.label.cex=0.6,
       vertex.label.dist=0,
       vertex.shape= "circle",
       edge.color="red",
       edge.width=0.5,
       edge.arrow.size=0.4,
       edge.curved = 0.25,
       layout=layout.kamada.kawai
  ) 
  dev.off()
  pdf("r2.pdf", "pdf")
  plot(r, main="Ramayana", 
       vertex.color="darkgoldenrod1",
       vertex.frame.color="black",
       vertex.size = degree(r)*4, # size as per degree 
       vertex.label.cex=0.6,
       vertex.label.dist=-0.55,
       vertex.label.color="black", 
       edge.color="red",
       edge.width=2,
       edge.arrow.size=0.1,
       edge.curved = 0.25,
       layout=layout.kamada.kawai
  )
  dev.off()
  l <- layout.circle(r)
  print(l)
  hs <- hub_score(r)$vector
  as <- authority_score(r)$vector
  
  pdf("r3.pdf", "pdf")
  par(mfrow=c(1,2)) 
  plot(r, main="Hubs",
       vertex.color="orange",
       vertex.label.color="gray2",
       vertex.size = hs*30,
       # size as per degree 
       vertex.label.cex=0.3,
       vertex.label.dist=-0.7,
       edge.color="red",
       edge.arrow.size=0.1,
       layout=layout.kamada.kawai
  )
  dev.off() 
  pdf("r4.pdf", "pdf")
  plot(r, main="Authority",
       vertex.color=rainbow(15),
       vertex.size = as*30, # size as per degree 
       vertex.label.cex=0.6,
       vertex.label.dist=0.5,
       vertex.label.color="gray2",
       edge.color="red",
       edge.arrow.size=0.1,
       layout=layout.kamada.kawai
  )
  dev.off()
  # community detection
  # To detect the groups of densily connected nodes 
  par(mfrow=c(1,1))
  r1 <- graph.data.frame(df, directed=F)
  cg <- cluster_edge_betweenness(r1)
  pdf("r5.pdf", "pdf")
  plot(cg, r1,
       main="cluster networks",
       vertex.size = 10, 
       vertex.label.cex=0.5,
       layout=layout.kamada.kawai
  )
  dev.off()
  
  # Histogram of degree of nodes
  pdf("r6.pdf", "pdf")
  hist(degree(r),
       main="Hist. of Degree of nodes",
       col="orange",
       xlab = "Degree of nodes",
       ylab = "Frequency"
  )
  dev.off()
  
}
ramayana()