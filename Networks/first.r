install.packages("igraph")
install.packages("igraphdata")
install.packages("network") 
install.packages("tidygraph")
install.packages("ggraph")
install.packages("visNetwork")
install.packages("threejs")
install.packages("networkD3")
install.packages("DigrammeR")
library(igraph)
library(igraphdata)
library(ggraph)
library(tidygraph)

#Импортируем файл и формируем датасеты с вершинами и ребрами 
edges <- read.csv("edges_1.csv", header=T) 
edges <- edges[1:500,]
colnames(edges) <- c('from', 'to', 'type1', 'type2', 'weight')
nodes <- unique(edges[c('from', 'type1')])
more_nodes <- unique(edges[c('to', 'type2')])
colnames(nodes) <- c('organisation', 'type')
colnames(more_nodes) <- c('organisation', 'type')
nodes <- rbind(nodes, more_nodes)
nodes <- unique(nodes[c('organisation', 'type')])
rownames(nodes) <- NULL
edges <- edges[c('from', 'to', 'weight')]

net <- graph_from_data_frame(d=edges, vertices=nodes, directed=T)
V(net)$degrees <- degree(net)
V(net)$degrees

#Визуализируем граф при помощи пакета 'ggraph'
#Нужно перебрать разные layout, которые можно посмотреть в help функции, выбрать лучший на глаз
net %>%
  ggraph(layout='nicely') +
  geom_node_point(aes(color = type, size=degrees)) +
  geom_edge_link(alpha = 0.2) +
  theme_void() +
  ggtitle("Our Network")

#Считаем базовые параметры графа
net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)
# смотрим на некоторые числовые характеристики графа
# Density
# The proportion of present edges from all possible ties.
edge_density(net)

# связность сети
igraph::components(net) # в пакете statnet такое же название
# decompose создает отдельные графы из компонент

# Diameter (longest geodesic distance) (если несколько компонент, то диаметр самой большой)
# Note that edge weights are used by default, unless set to NA.
diameter(net)
diam <- get_diameter(net)
diam

# рассчитаем кратчайшие пути
# Average path length 
# The mean of the shortest distance between each pair of nodes in the network 
# (in both directions for directed graphs). 
mean_distance(net)

# We can also find the length of all shortest paths in the graph:
#Но не будем

# We can also find the shortest path between specific nodes.


# We can also easily identify the immediate neighbors of a vertex.
# The 'neighbors' function finds all nodes one step out from the focal actor.
# To find the neighbors for multiple nodes, use 'adjacent_vertices()'.
neighbors(net, V(net)["University of Auckland"], mode="out")

# To find node neighborhoods going more than one step out, use function 'ego()'
# with parameter 'order' set to the number of steps out to go from the focal node(s).
ego(net, order = 1, nodes = V(net)["University of Auckland"])
g <- make_ego_graph(net, order = 2, nodes = V(net)["University of Auckland"])
g[[1]] %>%
  ggraph(layout='nicely') +
  geom_node_point(aes(color = type)) +
  geom_edge_link(alpha = 0.2) +
  theme_void() +
  ggtitle("Neighbours 2 step away from University of Auckland")
  