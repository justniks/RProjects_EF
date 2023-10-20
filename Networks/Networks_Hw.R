# Установим рабочую директорию
setwd("~/Documents/GitHub/RProjects_EF/Networks")


# Подгрузим необходимые для дальнейшего анализа библиотеки
## install.packages("igraph")
## install.packages("igraphdata")
## install.packages("network") 
## install.packages("tidygraph")
## install.packages("ggraph")
## install.packages("visNetwork")
## install.packages("threejs")
## install.packages("networkD3")
## install.packages("DigrammeR")


library(igraph);
library(igraphdata);
# library(intergraph);
library(network);
library(tidygraph);
library(ggraph);
library(visNetwork);
library(threejs);
library(networkD3);
# library(DigrammeR);


# Загрузим данные для исследования
edges <- read.csv("edges_1.csv", header=T) 
# edges <- edges[1:500, ]
colnames(edges) <- c('from', 'to', 'type1', 'type2', 'weight')

nodes <- unique(edges[c('from', 'type1')])
more_nodes <- unique(edges[c('to', 'type2')])

colnames(nodes) <- c('organisation', 'type')
colnames(more_nodes) <- c('organisation', 'type')

nodes <- rbind(nodes, more_nodes)
nodes <- unique(nodes[c('organisation', 'type')])
rownames(nodes) <- NULL
edges <- edges[c('from', 'to', 'weight')]

# Создадим также набор обратных величин для весов ребер (это пригодится нам в дальнейшем)
edges_inv <- edges
edges_inv$weight <- 1 / edges_inv$weight

net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)
net_inv <- graph_from_data_frame(d=edges_inv, vertices=nodes, directed=F)
# направленный граф нужен для визуализации графа при помощи пакета ggraph
net_directed <- graph_from_data_frame(d=edges, vertices=nodes, directed=T)


ceb <- cluster_edge_betweenness(net_inv, weights=E(net_inv)$weight)

mbship_ceb <- membership(ceb) 
# mbship_ceb

s_ceb <- sizes(ceb)
s_ceb

ms_ceb <- modularity(ceb)
ms_ceb


ceb


plot(ceb, net)
plot(ceb, net, vertex.color==c())
V(net_inv)$type


cfg <- cluster_fast_greedy(net_inv, weights=E(net_inv)$weight) 
# plot(cfg, net)
mbship_cfg <- membership(cfg) 
# mbship_cfg
s_cfg <- sizes(cfg)
s_cfg
ms_cfg <- modularity(cfg)
ms_cfg


cfg


plot(cfg, net)


compare(ceb, cfg)