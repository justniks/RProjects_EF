rm(list = ls())
library(igraph)

######################################
######################################
# predictive analytics on network data
######################################
######################################

# загружаем данные: edgelist это список ребер
# customers это клиенты онлайн магазина, столбец churn = 1 означает, что клиент перестал быть клиентом 
# фактически если churn = 1, то клиент точно ушел, а если churn = 0, то он может уйти
# вот для таких клиентов, у которых churn = 0, мы хотим посчитать вероятность, что они уйдут
# то есть перейдут в класс churn = 1
load("/Users/olgaklachkova/Desktop/TEACHING/МОР-3 (сетевые модели)/R скрипты/StudentCustomers.RData")
load("/Users/olgaklachkova/Desktop/TEACHING/МОР-3 (сетевые модели)/R скрипты/StudentEdgelist.RData")
network <- graph_from_data_frame(edgeList, directed = FALSE)
# мы создали граф из списка ребер, но у вершин нет характеристики churn
# нужно эту характеристику добавить в граф
V(network) # Порядок вершин в графе и в data frame не совпадает

# например, вершина 2 в списке вершин на месте 2 (втором)
# найдем  ее номер в data frame customers 
which(customers$id == V(network)[2]) # оказывается, она под пятым номером

# сделаем цикл, чтобы правильно записать, какая вершина churn
# x это вектор названий вершин по порядку из графа
# y это вектор того, на каком месте в data frame customers стоит вершина с названием из х
# z это соотвествующий факт, churn вершина или нет
x <- as.numeric(V(network)$name) # пишем as.numeric, так как в data frame Factor
y <- rep(NA, 4964)
z <- rep(NA, 4964)
for(i in 1:4964){
  y[i] <- which(customers$id == x[i])
  z[i] <- customers$churn[y[i]]
}
V(network)$churn <- z

plot(network, vertex.label = NA, edge.label = NA,
     edge.color = "black", vertex.size = 2, 
     vertex.color = ifelse(V(network)$churn == 1, "red", "white"),
     layout = layout_with_lgl)


mean(customers$churn) # 15,6 % клиентов ушли

# исследовательский вопрос: правда ли, что если у вершины сосед имеет churn = 1, 
# то эта вершина с большей вероятностью станет churn = 1, чем если у нее нет таких соседей
# как понять, что есть предпочтительное присоединение?
assortativity(network, types1 = V(network)$churn)
# больше 0, значит вершины одного типа имеют тенденцию чаще иметь ребра между собой

# посмотрим на подграф, где все вершины churner'ы
churnerNetwork <- induced_subgraph(network, v = V(network)[which(V(network)$churn == 1)])
churnerNetwork
V(churnerNetwork)$churn # проверяем, что все вершины в новом графе churn = 1
plot(churnerNetwork, vertex.label = NA, vertex.size = 2)
edge_density(network)
edge_density(churnerNetwork)
# плотность нового графа значительно выше, подтверждаем идею про предпочтительное присоединение


# relational neighbor classifier
# вероятность вершины перейти в класс churn = 1 равна доле соседей churn = 1 от общего числа соседей
# используем матричные вычисления
A <- as_adjacency_matrix(network, names = TRUE)
A <- as.matrix(A)
Neighbors <- rowSums(A) # общее число соседей вершины
ChurnNeighbors <- A %*% V(network)$churn # число соседей churn = 1
churnProb <- ChurnNeighbors / Neighbors
mostLikelyChurners <- which(churnProb > 0.7)
# знаем номера клиентов, которые наиболее вероятно уйдут 

# можно проделать несколько итераций
# это наверняка даст результат не очень хороший, потому что очень мало вершин churn = 1
# это значит, что вероятность ухода будет становиться все ближе к 0,
# если мы все более далеких соседей принимаем во внимание
# probabilistic relational neighbor classifier
churnProb_iter <- churnProb
for(i in 1:5){
  churnProb_iter <- as.vector((A %*% churnProb_iter) / Neighbors)
}
V(network)[which(churnProb_iter > 0.5)] # с такой вероятностью уже не осталось вершин
V(network)[which(churnProb_iter > 0.3)] # а вот такие вершины в графе есть


#######################################
# исследование, обогащенное сетевыми данными
# network featurization: добавим сетевые характеристики в data frame customers
V(network)$degree <- degree(network, normalized = TRUE)
V(network)$ChurnNeighbors <- ChurnNeighbors
V(network)$RelationalNeighbor <- churnProb
V(network)$averageDegree <- (A %*% V(network)$degree) / degree(network)
Data <- igraph::as_data_frame(network, what = 'vertices')

# logistic regression предсказания churn = 1
logit <- glm(churn ~ degree + ChurnNeighbors + averageDegree, family = 'binomial', data = Data)
summary(logit) 

# можно стандартным для машинного обучения образом разделить данные
# на тренировочную и тестовую выборки,
# хотя так нельзя делать для сетевых данных
set.seed(7)
index_train <- sample(1:nrow(Data), 2 / 3 * nrow(Data))
training_set <- Data[index_train,]
test_set <- Data[-index_train,]

# logistic regression
logit <- glm(churn ~ degree + ChurnNeighbors + averageDegree, family = 'binomial', data = training_set)
summary(logit)
predict <- predict(logit, newdata = test_set, type = "response")
predict.class <- ifelse(predict > 0.5, 1, 0)
table(predict.class, test_set[,2])
mean(predict.class == test_set[,2])
sum(test_set[,2] == 0) / nrow(test_set)
