install.packages("igraph")
install.packages("igraphdata")
install.packages("intergraph")
library(igraph)
library(igraphdata) # data 
library(intergraph) # change formats, function asIgraph()

# take built-in data "karate"
data(karate)
plot(karate)

# graph formation models and confidence intervals

# Erdos-Renyi model
p <- edge_density(karate)
p
g.er <- sample_gnp(n = vcount(karate), p = p)

par(mfrow = c(1, 2))
plot(karate, vertex.label=NA, vertex.color = "yellow")
plot(g.er, vertex.label=NA)
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
plot(karate, vertex.label=NA, vertex.color = "yellow", layout = layout_in_circle)
plot(g.er, vertex.label=NA, layout = layout_in_circle)
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
N <- round(1+log(vcount(karate),2))+1
N
hist(degree(karate), main = "Histogram of node degree", breaks = N, prob = TRUE)
hist(degree(g.er), main = "Histogram of node degree", breaks = N, prob = TRUE)
par(mfrow = c(1, 1))

graph_vector <- vector('list', 1000)
mean_dist_vector <- rep(NA, 1000)
for(i in 1:1000){
  graph_vector[[i]] <- sample_gnp(n = vcount(karate), p = p)
  mean_dist_vector[i] <- mean_distance(graph_vector[[i]])
}

hist(mean_dist_vector)
abline(v = mean_distance(karate, weights = NA), col = "red", lty = 3, lwd = 2)

# Watts-Strogatz small-world graph
# Creates a lattice with 'dim' dimensions of 'size' nodes each, and rewires edges 
# randomly with probability 'p'. You can allow 'loops' and 'multiple' edges.
# The neighborhood in which edges are connected is 'nei'.

g.WS <- sample_smallworld(dim = 1, size = 5, nei = 1, p = 0)
plot(g.WS, vertex.label=NA, layout = layout_in_circle, vertex.size = 5)

g.WS <- sample_smallworld(dim = 1, size = vcount(karate), nei = 3, p = 0.05)
plot(g.WS, vertex.label=NA, layout = layout_in_circle, vertex.size = 5)

graph_vector <- vector('list', 1000)
mean_dist_vector <- rep(NA, 1000)
for(i in 1:1000){
  graph_vector[[i]] <- sample_smallworld(dim = 1, size = vcount(karate), nei = 2, p = 0.05)
  mean_dist_vector[i] <- mean_distance(graph_vector[[i]])
}

hist(mean_dist_vector)
abline(v = mean_distance(karate, weights = NA), col = "red", lty = 3, lwd = 2)

hist(mean_dist_vector, xlim=c(2,5))
abline(v = mean_distance(karate, weights = NA), col = "red", lty = 3, lwd = 2)

# Barabasi-Albert preferential attachment model for scale-free graphs
# 'n' is number of nodes, 'power' is the power of attachment (1 is linear)
# 'm' is the number of edges added on each time step 
g.BA <- sample_pa(n = vcount(karate), power = 1, directed = FALSE)
par(mfrow = c(1, 2))
plot(karate, vertex.label=NA, vertex.color = "yellow")
plot(g.BA, vertex.label=NA)
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
N <- round(1+log(vcount(karate),2))+1
N
hist(degree(karate), main = "Histogram of node degree", breaks = N, prob = TRUE)
hist(degree(g.BA), main = "Histogram of node degree", breaks = N, prob = TRUE)
par(mfrow = c(1, 1))

startg <- subgraph.edges(karate, 
                              eids = E(karate)[-which(weight<4)], 
                              delete.vertices = T)
plot(startg)
g.BAs <- sample_pa(n = vcount(karate), power = 1, directed = FALSE, start.graph = startg)
par(mfrow = c(1, 3))
plot(karate, vertex.label=NA, vertex.color = "yellow")
plot(g.BA, vertex.label=NA)
plot(g.BAs, vertex.label=NA)
par(mfrow = c(1, 1))

# karate has little nodes, let's try 500 nodes power law
g <- sample_pa(500, power = 1, directed = FALSE, m=1)
ecount(g)
plot(g, vertex.label=NA, vertex.size = 2)
plot(degree.distribution(g)) 
plot(degree.distribution(g), log='xy') # linear in log

# 25% new node has 0 neighbors 
# 50% new node has 1 neighbors
# 25% new node has 2 neighbors
# out.dist sets distribution
g <- sample_pa(500, out.dist = c(0.25, 0.5, 0.25), directed = FALSE, zero.appeal = 1)
plot(g, vertex.label=NA, vertex.size = degree(g)/2)
plot(degree.distribution(g))
plot(degree.distribution(g), log='xy') # not so linear in logs 


# Assortativity / homophily
# The tendency of nodes to connect to others who are similar on some variable
# assortativity_nominal() is for categorical variables (labels)
# assortativity() is for ordinal and above variables
# assortativity_degree() checks assortativity in node degrees

p <- edge_density(karate)
assortativity_nominal(karate, types = V(karate)$Faction)
assortativity(karate, types1 = V(karate)$Faction)
assortativity_degree(karate)


gl <- vector('list', 1000)
m <- rep(NA, 1000)
for(i in 1:1000){
  gl[[i]] <- erdos.renyi.game(n = vcount(karate), p.or.m = p, type = "gnp")
  m[i] <- assortativity(gl[[i]], types1 = sample(V(karate)$Faction))
}
hist(m, xlim = range(c(-1, 1)))
abline(v = assortativity(karate, types1 = V(karate)$Faction), col = "red", lty = 3, lwd = 2)


###########################################
############################################
# транзитивность, кластеры, поиск сообществ
############################################
############################################

# транзитивность
data(karate)
p <- edge_density(karate)

# global - ratio of triangles (direction disregarded) to connected triples
# local - ratio of triangles to connected triples each vertex is part of
transitivity(karate, type = "local")
transitivity(karate, type = "global")

gl <- vector('list', 1000)
m <- rep(NA, 1000)
for(i in 1:1000){
  gl[[i]] <- erdos.renyi.game(n = vcount(karate), p.or.m = p, type = "gnp")
  m[i] <- transitivity(gl[[i]], type = "global")
}
hist(m, xlim = range(c(0, 1)))
abline(v = transitivity(karate, type = "global"), col = "red", lty = 3, lwd = 2)

###########################################
# A number of algorithms aim to detect groups that consist of densely connected nodes
# with fewer connections across groups. 

components(karate) # компоненты связности

# Find cliques (complete subgraphs of an undirected graph)
largest_cliques(karate)
vcol <- rep("grey80", vcount(karate))
vcol[unlist(largest_cliques(karate)[[1]])] <- "gold"
plot(karate, vertex.label = NA, vertex.color = vcol, layout = layout_with_fr)

# K-core decomposition
# The k-core is the maximal subgraph in which every node has degree of at least k
# This also means that the (k+1)-core will be a subgraph of the k-core.
# The result here gives the coreness of each vertex in the network.
kc <- coreness(karate) # subgraph in which every node has degree of at least k
max(kc)
table(kc)
colrs <- c("gray50", "tomato", "gold", "yellowgreen")
plot(karate, vertex.size = kc*3, vertex.label = kc, vertex.color = colrs[kc], layout = layout_with_fr)

# Community detection based on edge betweenness (Newman-Girvan)
# High-betweenness edges are removed sequentially (recalculating at each step)
# and the best partitioning of the network is selected.
ceb <- cluster_edge_betweenness(karate, weights = NULL)
plot(ceb, karate)
membership(ceb) 
sizes(ceb)
modularity(ceb)

# High modularity for a partitioning reflects dense connections within communities 
# and sparse connections across communities.
# Community detection based on greedy optimization of modularity
cfg <- cluster_fast_greedy(karate, weights = NULL)
plot(cfg, karate)
membership(cfg) 
sizes(cfg)
modularity(cfg)

par(mfrow = c(1, 2)) 
plot(ceb, karate)
plot(cfg, karate) 

# функция compare() считает whether or not any two vertices are members of the same community
compare(ceb, cfg)


###########################################
# похожие вершины по корреляции Пирсона между их соседями
A <- as_adjacency_matrix(karate, attr = "weight", names = TRUE)
A <- as.matrix(A)
S <- cor(A)
diag(S) <- 0
S[S < .6] <- 0
filtered_network <- graph_from_adjacency_matrix(adjmatrix = as.matrix(S), weighted = TRUE, mode = "undirected")
plot(filtered_network, layout = layout_with_kk, vertex.label = NA)

######### similarity ###########
similarity(karate, vids = c("Mr Hi", "John A")) # Jaccard similarity

# intersection.igraph
jaccard_edgeset_similarity <- function(G1, G2) {
  inter <- length(E(G1 %s% G2))
  un <- length(E(G1 %u% G2))
  
  if (un == 0) {
    0
  } else {
    inter/un
  }
}

gJ <- erdos.renyi.game(n = gorder(karate), p.or.m = p, type = "gnp")

jaccard_edgeset_similarity(gJ, karate)


###########################################
# иерархическая кластеризация как в машинном обучении
S <- cor(A)
D <- 1-S
d <- as.dist(D)
cc <- hclust(d)
plot(cc)
cls <- cutree(cc, k = 2)
V(karate)$cluster <- cls
cor(V(karate)$cluster, V(karate)$Faction)
plot(karate, vertex.label = NA, vertex.color = ifelse(V(karate)$cluster == 1, "orange", "dodgerblue"), edge.width = E(karate)$weight)
