
# Установим рабочую директорию
setwd("~/Documents/GitHub/RProjects_EF/Networks")

library(igraph);
library(igraphdata);
library(ggraph);
library(tidygraph);


## Обработка данных и создание графа

# Загрузим данные для исследования: набор ребер и вершин
edges <- read.csv("edges_1.csv", header=T) 
colnames(edges) <- c('from', 'to', 'type1', 'type2', 'weight')

nodes <- unique(edges[c('from', 'type1')])
more_nodes <- unique(edges[c('to', 'type2')])

colnames(nodes) <- c('organisation', 'type')
colnames(more_nodes) <- c('organisation', 'type')

nodes <- rbind(nodes, more_nodes)
nodes <- unique(nodes[c('organisation', 'type')])
rownames(nodes) <- NULL
edges <- edges[c('from', 'to', 'weight')]

# Создадим также набор обратных величин для весов ребер (это пригодится нам в дальнейшем). Исходные веса нам уже не понадобятся
edges$weight <- 1 / edges$weight

net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)

V(net)$color <- ifelse( V(net)$type=="Business Enterprise", "red", 
                        ifelse( V(net)$type=="Private not for profit", "purple", 
                                ifelse( V(net)$type=="Government", "green", 
                                        ifelse( V(net)$type=="Higher Education", "blue", "black") 
                                ) 
                        ) 
)
head(V(net)$color, 10)




################
################
################
# хотим посчитать гомофилию для универов vs все остальные
# далее делаем некоторые вершины unknown (кроме супер популярных / супер центральных) 
# и хотим построить предсказательную модель для предсказания типа вершины, которая изначально unknown

# p <- edge_density(net)
# n_c <- sum( V(net)$type == 'Higher Education' )
# exp_diad_uni <- n_c * (n_c - 1) / 2 * p
# 
# m_c <- 0
# for (e in edges) {
#   if (e$from == 'Higher Education' && e$to == 'Higher Education') {
#     m_c <- m_c + 1
#   }
# }
# 
# diadicity_uni <- m_c / exp_diad_uni # ?



####

# n_nc <- sum( V(net)$type != 'Higher Education' )
# 
# exp_hetero_uni <- n_nc * n_c * p
# 
# m_mixed <- 0
# for (e in edges) {
#   if (e$from == 'Higher Education' && e$to != 'Higher Education') {
#     m_mixed <- m_mixed + 1
#   }
#   if (e$from != 'Higher Education' && e$to == 'Higher Education') {
#     m_mixed <- m_mixed + 1
#   }
# }
# 
# heterophilicity_uni <- m_mixed / exp_hetero_uni # ?


# for (e in edges) {
#   if (e['from'] == 'Higher Education') {
#     print(e$from)
#   }
# }
# 
# for (e in edges) {
#   print( class(e['from']) )
# }
# 
# class( edges['from'] )
# 
# edges['from']

# for (row in 1:nrow(edges)) {
#   from_obj <- edges[row, 'from']
#   to_obj <- edges[row, 'to']
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
# class(V(net))
# head(V(net), 10)
# 
# unis <- V(net)[which( V(net)$type == 'Higher Education' )]
# unis
# 
# length(unis)
# sum( V(net)$type == 'Higher Education' )


############################### НАЧАЛО
p <- edge_density(net)
p # 0.003745601
n_c <- sum( V(net)$type == 'Higher Education' )
n_c # 54
exp_diad_uni <- n_c * (n_c - 1) / 2 * p
exp_diad_uni # 5.359955

# m_c <- 0
# for (e in edges) {
#   if (e$from == 'Higher Education' && e$to == 'Higher Education') {
#     m_c <- m_c + 1
#   }
# }


unis <- V(net)[which( V(net)$type == 'Higher Education' )]
unis <- names(unis)
unis

m_c <- 0

for (row in 1:nrow(edges)) {
  from_obj <- edges[row, 'from']
  to_obj <- edges[row, 'to']
  
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
# head( V(net)$type )
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
n_nc <- sum( V(net)$type != 'Higher Education' )

exp_hetero_uni <- n_nc * n_c * p
exp_hetero_uni # 294.6964

m_mixed <- 0

for (row in 1:nrow(edges)) {
  from_obj <- edges[row, 'from']
  to_obj <- edges[row, 'to']
  
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

n_nc # 1457

############################### КОНЕЦ


uni_net <- induced_subgraph(net, v = V(net)[which(V(net)$type == 'Higher Education')])
uni_net
V(uni_net)$type # проверяем, что все вершины в новом графе churn = 1
edge_density(net) # 0.003745601
edge_density(uni_net) # 0.1201957

edge_density(uni_net) / edge_density(net) # 32.08982

net

## net[which( V(net)$type == 'Higher Educattion' )]
