# Support Vector Machines 
# пакет e1071
## install.packages('e1071')
library(e1071)

head(iris,5) # будем использовать встроенный набор данных про цветочки
attach(iris) # далее всюду будет в качестве data этот набор данных

x <- subset(iris, select=-Species)
x

y <- Species # будем предсказывать вид цветка по 4 характеристикам

?svm # In SVR, the best fit line is the hyperplane that has the maximum number of points
# find a function f(x) that deviates from y_n by a value no greater than \varepsilon for each training point x 
# and at the same time is as flat as possible
svm_model <- svm(Species ~ ., data=iris, cross = 10, kernel="linear")
# автоматически ошибка будет считаться, если выбирать параметр кросс-валидации
summary(svm_model)
# svm_model1 <- svm(x,y) -- или можно вот так, используя x, y
# summary(svm_model1)


# можно предсказывать классы и сравнивать, насколько хорошо мы это делаем
pred <- predict(svm_model,x)
table(pred,y)


# графики: можно поглядеть, как красивее нарисовать
m <- svm(Species ~ Petal.Width + Petal.Length, data=iris, kernel='linear')
plot(m, iris, Petal.Width ~ Petal.Length, svSymbol = 4, dataSymbol = 20, 
     symbolPalette = rainbow(4), color.palette = cm.colors)



# подберем наилучшие параметры
?tune
ctrl <- tune.control(sampling = "cross", cross = 5) # будем подбирать на основе кросс-валидации 

# install.packages('tictoc')
library(tictoc)

tic()
svm_tune <- tune(svm, train.x=x, train.y=y, kernel="radial", 
                 ranges=list(cost=seq(from = 1, to = 30, by = 1), gamma=seq(from = 0.1, to = 3, by = 0.1)),
                 tunecontrol = ctrl)
toc()

print(svm_tune)
tunedModel <- svm_tune$best.model
summary(tunedModel)