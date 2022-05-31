data(mtcars)

cars <- mtcars[,-c(2,8,9,10)]

maxs <- apply(cars, 2, max)
mins <- apply(cars, 2, min)

cars <- scale(cars, center = mins, scale = maxs - mins)

names <- c("Расход (сколько миль за галлон)", "Число цилиндров", "Объём двигателя", "Мощность", "Передаточное число главной передачи", "Вес",
           "Ускорение", "Тип двигателя", "Трансмиссия", "Число передач", "Число карбюраторов")

# Матрица попарных расстояний (по умолчанию - Евклидово расстояние)
dist_cars <- dist(cars)

# Кластерный анализ
clust_cars <- hclust(dist_cars, "ward.D")

# Построение дендрограммы
plot(clust_cars, labels = rownames(mtcars), cex=0.5, 
     main="Дендрограмма кластеров",
     xlab="Марки")
rect.hclust(clust_cars, k = 4, border = "blue")

# Разбиение дендрограммы на кластеры
groups <- cutree(clust_cars, k = 4)

cars[groups==1, 1]
cars[groups==2, 1]
cars[groups==3, 1]
cars[groups==4, 1]

# Вычисляем среднее значение показателей в каждом кластере
#  в 1-ом кластере
g1<-colMeans(cars[groups==1,])
#  во 2-ом кластере
g2<-colMeans(cars[groups==2,])
#  в 3-ем кластере
g3<-colMeans(cars[groups==3,])
#  во 4-ом кластере
g4<-colMeans(cars[groups==4,])

# Построение столбчатой диаграммы
df <- data.frame(g1,g2,g3,g4)
rownames(df) <- names
colnames(df) <- c("Раритет", "D класс", "А класс", "Кроссоверы")
barplot(data.matrix(df), main="Группы автомобилей", col=rainbow(7), ylim = c(0,1.5), beside = TRUE)
legend("topright", names, col=rainbow(7), lwd=5, bty = "n",  y.intersp = 0.4, text.width = 10)

library(lattice)

# Двумерные диаграммы рассеяния
# Зависимость мощности двигателя от расхода
xyplot(hp ~ mpg, mtcars, main='Зависимость мощности двигателя от расхода',
       xlab='Расход топлива (Число миль за галлон)', ylab='Мощность двигателя')

xyplot(hp+disp ~ mpg, mtcars, auto.key = TRUE, main="Зависимость расхода от мощности и объёма двигателя",
       xlab="Расход топлива (Число миль за галлон)",
       ylab="Мощность + Объём двигателя")

library(car)
# Диаграммы рассеяния
scatterplotMatrix(cars, spread=FALSE, lty.smooth=2,
                  main='Матрица диаграмм рассеяния')
