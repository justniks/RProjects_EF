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


# plot(
#   net_inv, 
#   vertex.label=NA,
#   vertex.color=V(net_inv)$color,
#   #vertex.size=degree(net_inv, v=V(net_inv)),
#   edge.width=E(net_inv)$weight, 
#   layout=layout_nicely
# )

ceb <- cluster_edge_betweenness(net_inv, weights=E(net_inv)$weight)

mbship_ceb <- membership(ceb) 
# mbship_ceb

s_ceb <- sizes(ceb)
s_ceb

ms_ceb <- modularity(ceb)
ms_ceb


ceb


group_sizes_ceb <- table(mbship_ceb)
group_sizes_ceb
group_sizes_ceb <- sort(group_sizes_ceb, decreasing = TRUE)
group_sizes_ceb
top_10_groups <- names(group_sizes_ceb)[1:10]
top_10_groups

group_vertices <- list()
for (group_id in top_10_groups) {
  group_indices <- which(mbship_ceb == group_id)
  group_vertices[[group_id]] <- names(group_indices)
}
group_vertices
group_vertices$`2`
mbship_ceb$`2`
class(group_vertices)


V(net_inv)$name

V(net_inv).which(group_vertices$`2` == V(net_inv)$name)

for (i in 1:10){
  group_vertices$`i`
}


plot(ceb, net_inv, layout=layout_nicely)

# plot( ceb, net_inv, vertex.color=c("red", "purple", "green", "blue"), layout=layout_nicely )
V(net_inv)$color <- ifelse( V(net_inv)$type=="Business Enterprise", "red", 
  ifelse( V(net_inv)$type=="Private not for profit", "purple", 
  ifelse( V(net_inv)$type=="Government", "green", 
  ifelse( V(net_inv)$type=="Higher Education", "blue", "black") ) ) )
# unique( V(net_inv)$color )

##########
# plot(
#   net_inv, 
#   vertex.label=NA,
#   vertex.color=V(net_inv)$color,
#   #vertex.size=degree(net_inv, v=V(net_inv)),
#   edge.width=E(net_inv)$weight, 
#   layout=layout_nicely
# )
# plot(net_inv, vertex.label=NA, edge.width=E(net_inv)$weight, layout=layout_with_kk)
#########

##########
# plot(
#   net_inv, 
#   vertex.label=NA,
#   vertex.color=V(net_inv)$color,
#   # vertex.size=degree(net_inv, v=V(net_inv)),
#   edge.width=E(net_inv)$weight, 
#   layout=layout_with_lgl
# )
#########

length(V(net_inv)) # 1551 ?


plot( ceb, net_inv, vertex.label=NA, vertex.color=V(net_inv)$color, layout=layout_nicely )
plot( ceb, net_inv, vertex.label=NA, vertex.color=V(net_inv)$color, layout=layout_with_lgl )

plot( ceb, net_inv, vertex.label=NA, layout=layout_nicely )

V(net_inv)$type
compare(ceb, V(net_inv)$type, method='nmi') # 3.287989


cfg <- cluster_fast_greedy(net_inv, weights=E(net_inv)$weight) 
# plot(cfg, net_inv)
mbship_cfg <- membership(cfg) 
# mbship_cfg
s_cfg <- sizes(cfg)
s_cfg
ms_cfg <- modularity(cfg)
ms_cfg


cfg


plot(cfg, net_inv, layout=layout_nicely)


compare(ceb, net_inv)


compare(ceb, cfg)


# sum(is.na(V(net_inv)$type))


V(net_inv)$num_type <- ifelse( V(net_inv)$type=="Business Enterprise", 1, 
                            ifelse( V(net_inv)$type=="Private not for profit", 2, 
                                    ifelse( V(net_inv)$type=="Government", 3, 
                                            ifelse( V(net_inv)$type=="Higher Education", 4, 0) ) ) )

assortativity_nominal(net_inv, types=V(net_inv)$num_type, directed=F) # -0.05876298


V(net_inv)$num_type1 <- ifelse( V(net_inv)$type=="Business Enterprise", 2, 
                                ifelse( V(net_inv)$type=="Private not for profit", 7, 
                                        ifelse( V(net_inv)$type=="Government", 5, 
                                                ifelse( V(net_inv)$type=="Higher Education", 10, 0) 
                                        ) 
                                ) 
)

assortativity_nominal(net_inv, types=V(net_inv)$num_type1, directed=F) # -0.05876298


assortativity_nominal(net_inv, types=V(net_inv)$type, directed=F) #NaN
assortativity_degree(net_inv, directed=F) # -0.3361749


unique(V(net_inv)$color)
head(V(net_inv)$color, 10)
