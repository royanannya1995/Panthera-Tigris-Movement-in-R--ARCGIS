set.seed(1234567890)
g <- graph.full(n = 20)
E(g)$weight <- round(runif(ecount(g)), 2) * 100
mstPrim <- minimum.spanning.tree(g, algorithm = "prim")
edgeMatrix <- data.frame(cbind(get.edgelist(g), E(g)$weight))
names(edgeMatrix) <- c("from", "to", "weight")
plot(mstPrim, layout = layout.kamada.kawai, edge.label = E(mstPrim)$weight)



#convert with graph packagege to BAM class of graph an calculate mst
mstKruskalBAM <- mstree.kruskal(graphBAM(edgeMatrix))
#build new data frame with resut
mstKruskalDF <- data.frame(cbind(t(mstKruskalBAM$edgeList),
                                 t(mstKruskalBAM$weight)))
#convert back to igraph package
mstKruskal <- graph.data.frame(mstKruskalDF, directed=FALSE)


plot(mstKruskal, layout = layout.kamada.kawai, edge.label = mstKruskal$weight)