install.packages("igraph")
install.packages("igraphdata")
library(igraph)
library(igraphdata)
# data(package="igraphdata") список встроенных датасетов
# https://statnet.org/packages/ еще есть такие пакеты, с некоторыми потом познакомимся
# library(intergraph) # пакет, чтобы менять форматы, см. ??intergraph
# функция asIgraph()

###################################################
###################################################
# введение: загрузка, визуализация, некоторые характеристики
###################################################
###################################################

# как можно создать граф? (0) list of edges 
g <- graph(edges = c(1,2,3,4,2,5,2,4), directed = F) # а если n = 10?
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

E(g)
V(g)
plot(g)

# присвоим вершинам и ребрам некоторые характеристики
V(g)$gender <- c("male", "male", "male", "male", "female") # set_vertex_attr
E(g)$type <- c("friend", "family", "family", "friend")
E(g)$weight <- c(1, 3, 3, 1) # delete_graph_attr(graph, "something") / delete_edge_attr(graph, "something")
g # посмотрим на характеристики вершин и ребер
edge_attr(g)
vertex_attr(g)
plot(g)
plot(g, vertex.color = ifelse(V(g)$gender == 'male', "orange", "dodgerblue"), edge.width = E(g)$weight) 
# ?igraph.plotting : узнаем, какие вообще есть опции при визуализации

# типы графов, которые мы смотрели на слайдах
eg <- make_empty_graph(40)
fg <- make_full_graph(40)
st <- make_star(40)
tr <- make_tree(40, children = 3, mode = "undirected")
plot() 

# как можно создать граф? (1) dataframe
# network of hyperlinks and mentions among news sources
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T) 
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T)
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
plot(net)
# упростим граф, уберем петли, а повторные ребра суммируем через edge.attr.comb
net_s <- simplify(net, remove.multiple = T, remove.loops = T, edge.attr.comb=c(weight="sum", type="ignore") )
plot(net_s, edge.arrow.size=.4, vertex.label=NA, vertex.size = V(net)$audience.size/4) # можно было создать характеристику V(net_s)$size <- V(net)$audience.size/4 
# можно из направленного графа сделать ненаправленный
as.undirected(net_s, mode = "collapse")

# как можно создать граф? (2) adjacency matrix
M <- get.adjacency(net)
M <- as_adjacency_matrix(net, attr = "weight") # суммируем вес ребер в матрице смежности
M <- as.matrix(M)
net_M <- graph_from_adjacency_matrix(M)

# возьмем готовые данные и визуализируем красиво: ?igraph.plotting
data(karate) # загружаем встроенный набор из пакета igraphdata
?karate
karate
vertex_attr(karate)
# присвоим двум лидерам свою фракцию, чтобы отделить их цветом на картинке
V(karate)$Faction[1] <- 3
V(karate)$Faction[34] <- 4
colrs <- c("gray50", "gold", "red", "dodgerblue") # colors() список всех цветов
V(karate)$color <- colrs[V(karate)$Faction]
vertex_attr(karate) # теперь есть характеристика color, она будет на картинке 
edge_attr(karate)

plot(karate, edge.width = E(karate)$weight)
plot(karate, vertex.label=NA, edge.width = E(karate)$weight)
# цвет ребра и тип ребра можно делать разный, если есть категориальная информация про ребра

# Network layouts – algorithms that return coordinates for each node in a network, 
# minimization of crossings between edges
plot(karate, vertex.label=NA, layout = layout_in_circle)
l <- layout_in_circle(karate)
# l is simply a matrix of x,y coordinates (N x 2) for the N nodes in the graph. 
# You can generate your own

plot(karate, vertex.label=NA, layout = layout_as_tree) # еще один не очень полезный пример

plot(karate, vertex.label=NA, layout = layout_with_fr)
# The Fruchterman-Reingold force-directed algorithm 
# Nice but slow, most often used in graphs smaller than ~1000 vertices

# You will also notice that the layout is not deterministic - different runs 
# will result in slightly different configurations

plot(karate, vertex.label=NA, layout = layout_with_kk)
# Another popular force-directed algorithm that produces nice results for
# connected graphs is Kamada Kawai. Like Fruchterman Reingold, it attempts to 
# minimize the energy in a spring system.

plot(karate, vertex.label=NA, layout = layout_with_lgl)
# The LGL algorithm is for large connected graphs. Here you can specify a root - 
# the node that will be placed in the middle of the layout.

plot(karate, vertex.label=NA, layout = layout_nicely)
# можно отображать только ребра с большим weight / разный цвет у разного типа ребер

karate_grid <- add_layout_(karate, on_grid()) # к объекту-графу можно добавить
# layout как характеристику, после чего при построении она будет фиксирована
plot(karate_grid)

# для добавления информации к графику можно базовую функцию legend()

###################################################
# смотрим на некоторые числовые характеристики графа
# Density
# The proportion of present edges from all possible ties.
edge_density(karate)

# связность сети
igraph::components(karate) # в пакете statnet такое же название
# decompose создает отдельные графы из компонент

# Diameter (longest geodesic distance) (если несколько компонент, то диаметр самой большой)
# Note that edge weights are used by default, unless set to NA.
diameter(karate)
diam <- get_diameter(karate)
diam

# рассчитаем кратчайшие пути
# Average path length 
# The mean of the shortest distance between each pair of nodes in the network 
# (in both directions for directed graphs). 
mean_distance(karate)

# We can also find the length of all shortest paths in the graph:
distances(karate) # with edge weights
distances(karate, weights=NA) # ignore weights
distances(karate, v=V(karate)["Mr Hi"], to=V(karate)["John A"], weights=NA)

# We can also find the shortest path between specific nodes.
# Say here between Mr Hi and the John A
shortest_paths(karate, 
               from = V(karate)["Mr Hi"], 
               to  = V(karate)["John A"],
               output = "both") # both path nodes and edges
shortest.paths(karate, to = V(karate)["Mr Hi"], algorithm = "dijkstra")

# We can also easily identify the immediate neighbors of a vertex.
# The 'neighbors' function finds all nodes one step out from the focal actor.
# To find the neighbors for multiple nodes, use 'adjacent_vertices()'.
neighbors(karate, V(karate)["Mr Hi"], mode="out")

# To find node neighborhoods going more than one step out, use function 'ego()'
# with parameter 'order' set to the number of steps out to go from the focal node(s).
ego(karate, order = 1, nodes = V(karate)["Mr Hi"])
g <- make_ego_graph(karate, order = 1, nodes = V(karate)["Mr Hi"])
plot(g[[1]])

# если нужно сделать подграф по какому-то признаку кроме соседства
g <- induced_subgraph(karate, vids = V(karate)$Faction == "1")
edge_density(g) / edge_density(karate) # это намного более связный граф
# можно оставлять только нужные ребра, наверное можно как-то проще это делать
edges <- ifelse(E(karate)$weight >= 3, 1, 0)*c(1:ecount(karate))
edges <- edges[-which(edges == 0)]
g <- subgraph.edges(karate, 
               eids = edges, 
               delete.vertices = FALSE)
plot(g) # видимо только подграф  самых сильно связанных вершин

###################################################
# еще немного визуализаций
# visNetwork package интерактивный пакет: http://datastorm-open.github.io/visNetwork/
# install.packages("fastmap")
library(visNetwork)
visIgraph(karate, layout = "layout_with_fr") %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T)

# ggraph package: https://rpubs.com/neloe/ggraph_intro



###################################################
###################################################
# диффузия в графе, центральности
###################################################
###################################################
# Centrality functions (vertex level) and centralization functions (graph level).
# The centralization functions return "res" - vertex centrality, "centralization", 
# and "theoretical_max" - maximum centralization score for a graph of that size.
# The centrality functions can run on a subset of nodes (set with the "vids" parameter)

# центральность по степени вершины
# сначала построим гистограмму степеней вершины
deg <- degree(karate) # which.max(deg)
plot(karate, vertex.label=NA, layout = layout_with_fr, vertex.size = deg)
hist(deg, main = "Histogram of node degree", breaks = 1:vcount(karate)-1, prob = TRUE) # degree_distribution
xfit <- seq(min(deg), max(deg), length = 40) 
yfit <- dnorm(xfit, mean = mean(deg), sd = sd(deg))
lines(xfit, yfit, col = "black", lwd = 2) # а точно ли надо с нормальным законом сравнивать?

centr_degree(karate) # $res тоже самое, что хранится в deg

# центральность по близости
# Closeness (centrality based on distance to others in the graph)
# Inverse of the node's average geodesic distance to others in the network
clos <- closeness(karate, weights = NA)
plot(karate, vertex.label=NA, layout = layout_with_fr, vertex.size = clos * 1000)
centr_clo(karate) # отличаются от clos умножением на (vcount(karate)-1)

# центральность по кратчайшему пути
# Betweenness (centrality based on a broker position connecting others)
# (Number of geodesics that pass through the node or the edge)
betw <- betweenness(karate, weights = NA)
centr_betw(karate)
edge_betweenness(karate) # number of shortest paths between vertices that contain the edge, то есть это центральность для ребра

# центральность по собственному значению
# Eigenvector (centrality proportional to the sum of connection centralities)
# Values of the first eigenvector of the graph adjacency matrix
eig <- eigen_centrality(karate, weights = NA)
centr_eigen(karate)

# Page rank: http://infolab.stanford.edu/~backrub/google.html
page_rank(karate)

# объединим все центральности в одну матрицу и посмотрим корреляции 
df <- data.frame (deg, clos, betw, eig$vector)
cor(df)


#####################################################
#####################################################
# модели формирования графа и доверительные интервалы
#####################################################
#####################################################

# модель Эрдеша-Реньи
data(karate)
p <- edge_density(karate)
g.random <- erdos.renyi.game(n = gorder(karate), p.or.m = p, type = "gnp")
# or er <- sample_gnm(n=100, m=40) 
# ('n' is number of nodes, 'm' is the number of edges)
par(mfrow = c(1, 2))
plot(karate, vertex.label=NA)
plot(g.random, vertex.label=NA)
hist(degree(g.random), main = "Histogram of node degree", breaks = 1:vcount(karate)-1, prob = TRUE)

par(mfrow = c(1, 2))
hist(degree(karate), main = "Histogram of node degree", breaks = 1:vcount(karate)-1, prob = TRUE)
hist(degree(g.random), main = "Histogram of node degree", breaks = 1:vcount(karate)-1, prob = TRUE)
par(mfrow = c(1, 1))

gl <- vector('list', 1000)
m <- rep(NA, 1000)
for(i in 1:1000){
  gl[[i]] <- erdos.renyi.game(n = gorder(karate), p.or.m = p, type = "gnp")
  m[i] <- mean_distance(gl[[i]])
}
hist(m, xlim = range(c(min(m), 6)))
abline(v = mean_distance(karate), col = "red", lty = 3, lwd = 2)
# оказывается, в случайном графе очень маленькое расстояние между вершинами

# модель Watts-Strogatz
# Watts-Strogatz small-world graph
# Creates a lattice with 'dim' dimensions of 'size' nodes each, and rewires edges 
# randomly with probability 'p'. You can allow 'loops' and 'multiple' edges.
# The neighborhood in which edges are connected is 'nei'.
g.WS <- sample_smallworld(dim = 1, size = gorder(karate), nei = 3, p = 0.05)
plot(g.WS, vertex.label=NA, layout = layout_in_circle, vertex.size = 5)

# модель Барабаши-Альберта
# Barabasi-Albert preferential attachment model for scale-free graphs
# 'n' is number of nodes, 'power' is the power of attachment (1 is linear)
# 'm' is the number of edges added on each time step 
g.BA <- sample_pa(n = gorder(karate), power = 1, directed = FALSE)
hist(degree(g.BA), main = "Histogram of node degree", breaks = 1:vcount(karate)-1, prob = TRUE)

# у karate мало вершин, посмотрим на степенной закон для 500 вершин
g <- sample_pa(500, power = 1, directed = FALSE)
plot(g, vertex.label=NA, vertex.size = 2)
plot(degree.distribution(g)) 
plot(degree.distribution(g), log='xy') # в логарифмах линейная функция

# в 25% случаев новый узел вообще не будет соединен ни с одним узлом (т. е. будет изолированным), 
# в 50% случаев он будет соединен с одним узлом, в 25% случаев – с двумя узлами
# параметр out.dist
g <- sample_pa(500, out.dist = c(0.25, 0.5, 0.25), directed = FALSE, zero.appeal = 1)
plot(g, vertex.label=NA, vertex.size = degree(g)/2)
plot(degree.distribution(g))
plot(degree.distribution(g), log='xy') # уже не совсем линейная функция


############################################
############################################
# предпочтительное присоединение и гомофилия
############################################
############################################

data(karate)
p <- edge_density(karate)

# предпочтительное присоединение
# Assortativity / homophily
# The tendency of nodes to connect to others who are similar on some variable.
# assortativity_nominal() is for categorical variables (labels)
# assortativity() is for ordinal and above variables
# assortativity_degree() checks assortativity in node degrees

assortativity_degree(karate)
vertex_attr(karate) # если фракция только 1 или 2, то это одинаковые корреляции будут
assortativity_nominal(karate, types = V(karate)$Faction)
assortativity(karate, types1 = V(karate)$Faction)

gl <- vector('list', 1000)
m <- rep(NA, 1000)
for(i in 1:1000){
  gl[[i]] <- erdos.renyi.game(n = gorder(karate), p.or.m = p, type = "gnp")
  m[i] <- assortativity(gl[[i]], types1 = sample(V(karate)$Faction))
}
hist(m, xlim = range(c(-1, 1)))
abline(v = assortativity(karate, types1 = V(karate)$Faction), col = "red", lty = 3, lwd = 2)

###########################################
# гомофилия

V(karate)$name <- seq(from = 1, to = 34)
edgeList <- as_edgelist(karate)
faction <- matrix(c(V(karate)$name, V(karate)$Faction), nrow = 34, ncol = 2)
FromLabel <- as.numeric(faction[match(as.numeric(edgeList[,1]), as.numeric(faction[,1])), 2]) # match(x, y) returns a vector with the location of x in y
ToLabel <- as.numeric(faction[match(as.numeric(edgeList[,2]), as.numeric(faction[,1])), 2]) 
edgeType <- FromLabel + ToLabel

table(edgeType)
table(as.numeric(faction[,2]))
exp_dyad <- table(as.numeric(faction[,2]))[1] * (table(as.numeric(faction[,2]))[1] - 1) * p/2
dyadicity <- table(edgeType)[1] / exp_dyad
exp_hetero <- table(as.numeric(faction[,2]))[1] * table(as.numeric(faction[,2]))[2] * p
heterophilicity <- table(edgeType)[2] / exp_hetero


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
  gl[[i]] <- erdos.renyi.game(n = gorder(karate), p.or.m = p, type = "gnp")
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

###########################################
###### Blockmodeling (отдельный скрипт) ###
###########################################


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
