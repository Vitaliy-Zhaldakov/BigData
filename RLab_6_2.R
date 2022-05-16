data(mtcars)

maxs <- apply(mtcars, 2, max)
mins <- apply(mtcars, 2, min)

cars <- scale(mtcars, center = mins, scale = maxs - mins)

names <- c("Расход (сколько миль за галлон)", "Число цилиндров", "Объём двигателя", "Мощность", "Передаточное число главной передачи", "Вес",
           "Ускорение", "Тип двигателя", "Трансмиссия", "Число передач", "Число карбюраторов")

# Готовые кластеры из RLab_6_1
groups

# Преобразование в фактор
groups_f <- factor(groups)

cars <- data.frame(cars)
cars <- cbind(cars, groups_f)
cars <- cars[,-c(2,8,9,10)]

#-------------------------------------------------------------
# Классификация по формуле Байеса
install.packages("klaR")
library(klaR)

# Вычисление вероятностей по всем признакам
naive_cars <- NaiveBayes(cars$groups_f ~ ., cars)
naive_cars$tables

# Ядерные функции плотности условной вероятности
plot(naive_cars,lwd = 2, legendplot=TRUE)

# Классификация по вероятностным данным
predict <- predict(naive_cars, cars[,-8])$class

# Соотношение фактического расстояния и прогноза
table(Группа = cars$groups_f, Прогноз = predict)

# Вычисление точности классификации по формуле Байеса
accuracy_bayes <- mean(predict == cars$groups_f)
accuracy_bayes
paste("Точность=", round(100*accuracy_bayes, 2), "%", sep = "")

#----------------------------------------------------------
# Классификация с помощью дерева решений Decision Tree
set.seed(1234)
# Индексирование данных, для 1 вероятность 60%, для 2 - 40%
index <- sample(2, nrow(cars), replace=TRUE, prob=c(0.6, 0.4))

# Разделение на обучающую и тестовую выборки
trainData <- cars[index==1,]
testData <- cars[index==2,]
nrow(trainData)
nrow(testData)
nrow(cars)

install.packages("party")
library(party)

# Построение модели
# Указываем зависимость групп от каждого параметра
formula <- groups_f ~ mpg + disp + hp + drat + wt + qsec + carb
cars_ctree <- ctree(formula, trainData)

# Обучение модели
table(predict(cars_ctree), trainData$groups_f)
plot(cars_ctree)

# Применение модели
test_predict <- predict(cars_ctree, newdata=testData)
table(test_predict, testData$groups_f)
accuracy_tree <- mean(test_predict == testData$groups_f)
accuracy_tree

#------------------------------------------------------------------
# Алгоритм Random Forest
install.packages("randomForest")
library(randomForest)

# Обучение модели
forest <- randomForest(groups_f ~ .,trainData, ntree=25, proximity=TRUE)
table(predict(forest), trainData$groups_f)

# Применение на тестовой выборке
test_forest <- randomForest(groups_f ~ .,testData, ntree=25, proximity=TRUE)
table(predict(test_forest), testData$groups_f)
accuracy_forest <- mean(test_forest == testData$groups_f)
accuracy_forest

