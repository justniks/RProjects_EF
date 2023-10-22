
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


## Обработка данных и создание графа

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

V(net_inv)$color <- ifelse( V(net_inv)$type=="Business Enterprise", "red", 
                            ifelse( V(net_inv)$type=="Private not for profit", "purple", 
                                    ifelse( V(net_inv)$type=="Government", "green", 
                                            ifelse( V(net_inv)$type=="Higher Education", "blue", "black") 
                                    ) 
                            ) 
)
head(V(net_inv)$color, 10)




################
################
################
# хотим посчитать гомофилию для универов vs все остальные
# далее делаем некоторые вершины unknown (кроме супер популярных / супер центральных) 
# и хотим построить предсказательную модель для предсказания типа вершины, которая изначально unknown

# p <- edge_density(net_inv)
# n_c <- sum( V(net_inv)$type == 'Higher Education' )
# exp_diad_uni <- n_c * (n_c - 1) / 2 * p
# 
# m_c <- 0
# for (e in edges_inv) {
#   if (e$from == 'Higher Education' && e$to == 'Higher Education') {
#     m_c <- m_c + 1
#   }
# }
# 
# diadicity_uni <- m_c / exp_diad_uni # ?



####

# n_nc <- sum( V(net_inv)$type != 'Higher Education' )
# 
# exp_hetero_uni <- n_nc * n_c * p
# 
# m_mixed <- 0
# for (e in edges_inv) {
#   if (e$from == 'Higher Education' && e$to != 'Higher Education') {
#     m_mixed <- m_mixed + 1
#   }
#   if (e$from != 'Higher Education' && e$to == 'Higher Education') {
#     m_mixed <- m_mixed + 1
#   }
# }
# 
# heterophilicity_uni <- m_mixed / exp_hetero_uni # ?


# for (e in edges_inv) {
#   if (e['from'] == 'Higher Education') {
#     print(e$from)
#   }
# }
# 
# for (e in edges_inv) {
#   print( class(e['from']) )
# }
# 
# class( edges_inv['from'] )
# 
# edges_inv['from']

# for (row in 1:nrow(edges_inv)) {
#   from_obj <- edges_inv[row, 'from']
#   to_obj <- edges_inv[row, 'to']
#   
#   print(from_obj)
#   print(to_obj)
#   
#   if (row > 5) break
# }
# 
# from_obj
# to_obj
# 
# class(V(net_inv))
# head(V(net_inv), 10)
# 
# unis <- V(net_inv)[which( V(net_inv)$type == 'Higher Education' )]
# unis
# 
# length(unis)
# sum( V(net_inv)$type == 'Higher Education' )


############################### НАЧАЛО
p <- edge_density(net_inv)
p # 0.003745601
n_c <- sum( V(net_inv)$type == 'Higher Education' )
n_c # 54
exp_diad_uni <- n_c * (n_c - 1) / 2 * p
exp_diad_uni # 5.359955

# m_c <- 0
# for (e in edges_inv) {
#   if (e$from == 'Higher Education' && e$to == 'Higher Education') {
#     m_c <- m_c + 1
#   }
# }


unis <- V(net_inv)[which( V(net_inv)$type == 'Higher Education' )]
unis <- names(unis)
unis

m_c <- 0

for (row in 1:nrow(edges_inv)) {
  from_obj <- edges_inv[row, 'from']
  to_obj <- edges_inv[row, 'to']
  
  if ( (from_obj %in% unis) && (to_obj %in% unis) ) {
    m_c <- m_c + 1
  }
  # if (row <= 5) {
  #   print(from_obj)
  #   print(to_obj)
  # }
}

m_c # 172

diadicity_uni <- m_c / exp_diad_uni 
diadicity_uni # 32.08982

############################### КОНЕЦ

# head(nodes, 20)
# 
# head( V(net_inv)$type )
# unis
# 
# 'Ako Aotearoa National Centre for Tertiary Teaching Excellence' %in% unis
# class(unis)
# class( unis[1] )
# 
# head( names(unis), 10 )

############################### НАЧАЛО

n_c # 54
p # 0.003745601
n_nc <- sum( V(net_inv)$type != 'Higher Education' )

exp_hetero_uni <- n_nc * n_c * p
exp_hetero_uni # 294.6964

m_mixed <- 0

for (row in 1:nrow(edges_inv)) {
  from_obj <- edges_inv[row, 'from']
  to_obj <- edges_inv[row, 'to']
  
  if ( (from_obj %in% unis) && !(to_obj %in% unis) ) {
    m_mixed <- m_mixed + 1
  }
  if ( !(from_obj %in% unis) && (to_obj %in% unis) ) {
    m_mixed <- m_mixed + 1
  }
}
m_mixed # 2051

heterophilicity_uni <- m_mixed / exp_hetero_uni # ?
heterophilicity_uni # 6.959706

n_nc

############################### КОНЕЦ
