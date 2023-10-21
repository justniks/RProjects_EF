install.packages("igraph")
install.packages("igraphdata")
install.packages("intergraph")
library(igraph)
library(igraphdata) # data 
library(intergraph) # change formats, function asIgraph()

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

############### your HW ##################
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
V(karate)$color[20] <- 4
plot(karate, vertex.label=NA)
plot(karate, vertex.label=NA, layout = layout_with_lgl(karate, root = 20))
V(karate)$color[20] <- 1

# R decides for you
plot(karate, vertex.label=NA, layout = layout_nicely)

# you can add a layout as a graph characteristic
# it will be fixed and choosen automatically when plotting
karate1 <- add_layout_(karate, in_circle()) 
plot(karate1)
legend(x=0, y=0, legend = "CIRCLE")


###################################################
## today's script
###################################################

# numerical characteristics of the graph
vcount(karate)
ecount(karate)
# Density
# The proportion of present edges from all possible ties
edge_density(karate)
edge_density(make_star(10, mode = "undirected"))
edge_density(make_star(14, mode = "undirected"))

# Connectivity
components(karate)
igraph::components(karate) # statnet package has the same function
x <- components(karate)$membership
V(karate)$cluster <- x
vertex_attr(karate)

# function decompose creates a list of separate graphs

# Diameter (longest geodesic distance)
# Note that edge weights are used by default, unless set to NA
diameter(karate) # with edge weights
diameter(karate, weights = NA) # ignore weights
diam <- get_diameter(karate)
diam

# Average path length 
# The mean of the shortest distance between each pair of nodes 
# (in both directions for directed graphs)
mean_distance(karate)
mean_distance(karate, weights = NA)
mean_distance(karate, weights = NA, details = T)

# matrix of shortest paths
distances(karate)
distances(karate, weights=NA)
distances(karate, v=c("Mr Hi", "Actor 28"), to="John A", weights=NA)

# We can also find the shortest path itself between specific nodes
# Say here between Mr Hi and the John A
shortest_paths(karate, from = "Mr Hi", to = "John A", output = "both") # both path nodes and edges
shortest.paths(karate, to = "Mr Hi", algorithm = "dijkstra")
shortest.paths(karate, to = "Mr Hi", algorithm = "dijkstra", weights=NA)

#Example
ex <- graph(edges = c(1,2,2,3,1,4,2,4,4,3), directed = F)
E(ex)$weight <- c(1,3,5,2,1)
shortest_paths(ex, from = 1, to = 3, algorithm = "dijkstra",  output = "both")
all_shortest_paths(ex, from = 1, to = 3)

# We can also easily identify the immediate neighbors of a vertex
# The 'neighbors' function finds all nodes one step out from the focal actor
# To find the neighbors for multiple nodes, use 'adjacent_vertices()'
neighbors(karate, "Mr Hi")
neighbors(karate, "John A")
a <- neighbors(karate, "Mr Hi")
b <- neighbors(karate, "John A")
intersection(a,b)
adjacent_vertices(karate, c("Mr Hi", "John A"))

# To find node neighborhoods going more than one step out, use function 'ego()'
# with parameter 'order' set to the number of steps out to go from the focal node(s)
ego(karate, order = 2, nodes = "Actor 17")
# create list of graphs, one graph for each element of nodes
g <- make_ego_graph(karate, order = 2, nodes = "Actor 17")
plot(g[[1]])

# To create a subgraph based on some attribute except neighborhood
g1 <- induced_subgraph(karate, vids = V(karate)$Faction == "1")
g2 <- induced_subgraph(karate, vids = V(karate)$Faction == "2")
par(mfrow=c(1,2)) # make several pictures
plot(g1)
plot(g2)
par(mfrow=c(1,1))# return to one-picture mode
edge_density(g1) / edge_density(karate) # two times more connected graph
edge_density(g2) / edge_density(karate) # 1.6 times more connected graph

# leave only edges satisfying some conditions
g <- subgraph.edges(karate, 
               eids = E(karate)[-which(weight<3)], 
               delete.vertices = FALSE)
plot(g) 

###################################################
# for interactive visualization
# http://datastorm-open.github.io/visNetwork/
# install.packages("fastmap")
install.packages("visNetwork")
library(visNetwork)
visIgraph(karate, layout = "layout_with_fr") %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T)

###################################################
#Centrality
###################################################

c <- c(0,1,0,0,1,0,1,1,0,1,0,1,0,1,1,0)
M <- matrix(c, nrow = 4, ncol = 4, byrow = T)
M
eigen(M)
ce <- (-1)*eigen(M)$vectors[,1]
ce
ce[which.max(ce)]
S <- 0
for (i in 1:4) S <- S+ ce[which.max(ce)]-c[i]
S

# Centrality functions (vertex level) and centralization functions (graph level).
# The centralization functions return "res" - vertex centrality, "centralization", 
# and "theoretical_max" - maximum centralization score for a graph of that size.
# The centrality functions can run on a subset of nodes (set with the "vids" parameter)

# first foe our example
net_M <- graph_from_adjacency_matrix(M, mode = "undirected")
plot(net_M)
# degree centrality
centr_degree(net_M)
# Closeness centrality
centr_clo(net_M)
# Betweenness centrality
centr_betw(net_M)
# Eigen centrality
centr_eigen(net_M)


# for carate
# degree centrality
# Histogram of vertex degrees
deg <- degree(karate) # which.max(deg)
plot(karate, vertex.label=NA, layout = layout_with_fr, vertex.size = deg)
hist(deg, main = "Histogram of node degree", breaks = 1:vcount(karate)-1, prob = TRUE) # degree_distribution
xfit <- seq(min(deg), max(deg), length = 40) 
yfit <- dnorm(xfit, mean = mean(deg), sd = sd(deg))
lines(xfit, yfit, col = "black", lwd = 2) 
centr_degree(karate) # $res (as in в deg)


# Closeness centrality (based on distance to others in the graph)
# Inverse of the node's average geodesic distance to others in the network
clos <- closeness(karate, weights = NA)
plot(karate, vertex.label=NA, layout = layout_with_fr, vertex.size = clos * 1000)
centr_clo(karate) # отличаются от clos умножением на (vcount(karate)-1)

# Betweenness centrality (based on a broker position connecting others)
# (Number of geodesics that pass through the node or the edge)
betw <- betweenness(karate, weights = NA)
centr_betw(karate)
edge_betweenness(karate) # number of shortest paths between vertices that contain the edge, то есть это центральность для ребра

# Eigen centrality (proportional to the sum of connection centralities)
# Values of the first eigenvector of the graph adjacency matrix
eig <- eigen_centrality(karate, weights = NA)
centr_eigen(karate)

# Page rank: http://infolab.stanford.edu/~backrub/google.html
page_rank(karate)

# объединим все центральности в одну матрицу и посмотрим корреляции 
df <- data.frame (deg, clos, betw, eig$vector)
cor(df)

