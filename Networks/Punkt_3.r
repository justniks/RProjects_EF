install.packages("DescTools")
library(DescTools)

#Центральность по степени
net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)
deg <- degree(net)
plot(net, vertex.label=NA, layout = layout_with_fr, vertex.size = deg) # гистограмма степеней вершин
hist(deg, main = "Histogram of node degree", breaks = 50, prob = TRUE) # распределение степеней вершин

mean(deg) # Средняя степень вершины по графу
Mode(deg) # Модальная степень вершин графа
median(deg) # Медиана степеней вершин
biggestdegree <- tail(sort(deg),10) # топ-10 по степеням
biggestdegree #в топе - 7 университетов, 3 гос. учреждения

# центральность по близости двумя способами

clos1 <- closeness(net, weights=NA) # Первый способ - без учета весов
clos1
plot(net, vertex.label=NA, layout = layout_with_fr, vertex.size = clos1 *50)
biggestclos1 <- tail(sort(clos1),10) # Вектор-топ 42 также будет единичным, а вот вектор-топ 43 уже нет
biggestclos1
smallestclos1 <- head(sort(clos1),10)
smallestclos1
clos1["University of Auckland"]

clos2 <- closeness(net)
clos2
plot(net, vertex.label=NA, layout = layout_with_fr, vertex.size = clos2 *50)
biggestclos2 <- tail(sort(clos2),10) 
biggestclos2
smallestclos2 <- head(sort(clos2),10)
smallestclos2

clos2["University of Auckland"]

# центральность по кратчайшему пути двумя способами

betw1 <- betweenness(net, weights = NA)
betw1

biggestbetw1 <- tail(sort(betw1),10)
biggestbetw1
smallestbetw1 <- head(sort(betw1),10)
smallestbetw1
betw1["University of Auckland"]

betw2 <- betweenness(net)
betw2

biggestbetw2 <- tail(sort(betw2),10)
biggestbetw2
smallestbetw2 <- head(sort(betw2),10)
smallestbetw2

betw2["University of Auckland"]

# центральность по собственному значению двумя способами

eig1 <- eigen_centrality(net, weights = NA)
eig1

biggesteig1 <- tail(sort(eig1$vector),10)
biggesteig1
smallesteig1 <- head(sort(eig1$vector),10)
smallesteig1

eig2 <- eigen_centrality(net)
eig2

biggesteig2 <- tail(sort(eig2$vector),10)
biggesteig2
smallesteig2 <- head(sort(eig2$vector),10)
smallesteig2

# объединим все центральности в одну матрицу и посмотрим корреляции в двух вариантах 
df1 <- data.frame (deg, clos1, betw1, eig1$vector)
cor(df1)

df2 <- data.frame (deg, clos2, betw2, eig2$vector)
cor(df2)

# Сравним, какие организации попадали в топы по центральностям (исключая центральность по близости)
sravn1 <- data.frame(names(biggestdegree), names(biggestbetw1), names(biggesteig1))
sravn1

sravn2 <- data.frame(names(biggestdegree), names(biggestbetw2), names(biggesteig2))
sravn2
