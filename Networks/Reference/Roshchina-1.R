install.packages("igraph")
install.packages("igraphdata")
install.packages("intergraph")
library(igraph)
library(igraphdata) # data 
library(intergraph) # change formats, function asIgraph()
??igraph
 

###################################################
###################################################
# introduction: loading, visualization, some characteristics
###################################################
###################################################

# Create graph as list of paired vertex 
g <- graph(edges = c(1,2,3,4,2,5,2,4), directed = F)
g
# The description of an igraph object starts with up to four letters: 
# D or U, for a directed or undirected graph
# N for a named graph (where nodes have a name attribute)
# W for a weighted graph (where edges have a weight attribute)
# B for a bipartite (two-mode) graph (where nodes have a type attribute)
# The two numbers that follow refer to the number of nodes and edges in the graph. 
# The description also lists node & edge attributes, for example: 
# (g/c) - graph-level character attribute 
# (v/c) - vertex-level character attribute 
# (e/n) - edge-level numeric attribute 

V(g)
E(g)
plot(g)

# assign characteristics to the vertices and edges
V(g)$gender <- c("male", "male", "male", "male", "female") 
E(g)$type <- c("friend", "family", "family", "friend")
E(g)$weight <- c(1, 3, 3, 1) 
g # посмотрим на характеристики вершин и ребер
# alternatively: set_vertex_attr
# see also set_edge_attr / delete_graph_attr / delete_edge_attr / ...
edge_attr(g)
vertex_attr(g)
plot(g)
plot(g, edge.label = paste(as.character(E(g)$weight), as.character(E(g)$type), sep = " "), vertex.color = ifelse(V(g)$gender == 'male', "orange", "dodgerblue"), edge.width = E(g)$weight) 
?igraph.plotting

# main types of graphs
eg <- make_empty_graph(20)
eg
plot(eg) 

fg <- make_full_graph(20)
fg
plot(fg) 

st <- make_star(20, mode = "out")
st
plot(st) 

tr <- make_tree(20, children = 3, mode = "undirected")
tr
plot(tr) 

# Create graph from dataframe
# network of hyperlinks and mentions among news sources
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T) 
View(nodes)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T)
View(links)
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
net
plot(net)
# simplify graph: remove loops and repeated edges
net_s <- simplify(net, remove.multiple = T, remove.loops = T, edge.attr.comb=c(weight="sum", type="ignore") )
net_s
plot(net_s, edge.arrow.size=.2, vertex.label = NA, vertex.size = V(net)$audience.size/4) # alternatively V(net_s)$size <- V(net)$audience.size/4 
# make undirected graph from directed graph
plot(as.undirected(net_s, mode = "collapse"))

# Create graph from adjacency matrix
M <- get.adjacency(net)
M
M <- get.adjacency(net_s)
M
M <- as_adjacency_matrix(net_s, attr = "weight")
M
M <- as.matrix(M)
M
net_M <- graph_from_adjacency_matrix(M)
net_M
net_s
net_M <- graph_from_adjacency_matrix(M, weighted = T)
net_M
net_s

# take built-in data from igraphdata and visualize it beautifully
data(karate)
?karate
karate
vertex_attr(karate)
plot(karate)
plot(karate, vertex.label=NA, edge.width = E(karate)$weight)
# assign two leaders their own color to separate them in the picture
V(karate)$color[1] <- 3
V(karate)$color[34] <- 3
vertex_attr(karate)
plot(karate, vertex.label=NA, edge.width = E(karate)$weight)

# Network layouts – algorithms that return coordinates for each node in a network, 
# minimization of crossings between edges
plot(karate, vertex.label=NA, layout = layout_in_circle)
l <- layout_in_circle(karate)
# l is simply a matrix of x,y coordinates (N x 2) for the N nodes in the graph. 

# You can generate your own
l[1,1] <- 2
l[34,1] <- -2
plot(karate, vertex.label=NA, layout = l)

# layout as tree
plot(karate, vertex.label=NA, layout = layout_as_tree) 

# The Fruchterman-Reingold force-directed algorithm 
# Nice but slow, most often used in graphs smaller than ~1000 vertices
plot(karate, vertex.label=NA, layout = layout_with_fr)

# You can also notice that the layout is not deterministic - different runs 
# will result in slightly different configurations

# Another popular force-directed algorithm that produces nice results for
# connected graphs is Kamada Kawai. Like Fruchterman Reingold, it attempts to 
# minimize the energy in a spring system.
plot(karate, vertex.label=NA, layout = layout_with_kk)

# The LGL algorithm is for large connected graphs. Here you can specify a root - 
# the node that will be placed in the middle of the layout.
V(karate)$color[2] <- 4
plot(karate, vertex.label=NA, layout = layout_with_lgl, root = 2)
V(karate)$color[2] <- 1

# R decides for you
plot(karate, vertex.label=NA, layout = layout_nicely)

# you can add a layout as a graph characteristic
# it will be fixed and choosen automatically when plotting
karate1 <- add_layout_(karate, in_circle()) 
plot(karate1)
legend(x=0, y=0, legend = "CIRCLE")


