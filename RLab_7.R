olympics <- read.csv(file = "C:/Users/vzhal/OneDrive/Рабочий стол/Обработка больших данных/Athlet_Events/athlete_events.csv", 
                   sep = ",", header = TRUE, dec = ',')

#----------------------------------------------------------------------
# Спортсмены по теннису
tennis_athlets <- subset(olympics, Sport == 'Tennis')

# Вес спортсменов
weight <- as.numeric(tennis_athlets$Weight)

# Удаление пропущенных данных
# na <- is.na(weight)
# good_weight <- weight[!na]

# Гистограмма веса
hist(weight, main='Вес спорсменов',
     xlab='Вес', ylab='Частота')

# Проверка выборки на нормальность распределения с помощью Квантильно-квантильного графика
# (показывает распределение данных относительно ожидаемого нормального распределения)
qqnorm(weight)
qqline(weight, col='red')

install.packages('car')
library(car)
qqPlot(weight)

# Тест Стьюдента
t.test(weight, mu=65)

# Тест Уилкоксона
wilcox.test(weight, mu=65, conf.int = TRUE)

# Тест Шапиро-Уилкса для проверки на нормальность
shapiro.test(weight)

#-----------------------------------------------------------------------------------------
# Выборка теннисистов и пловцов
tennis_swimming_men <- subset(olympics, Sex == 'M' & (Sport == 'Tennis' | Sport == 'Swimming'))

weight <- as.numeric(tennis_swimming_men$Weight)

# Гистограмма веса
hist(weight, main='Гистограмма веса теннисистов и пловцов', xlab='Вес')

# Проверка на нормальность
qqPlot(weight)

tennis_weight <- as.numeric(subset(olympics, Sex == 'M' & Sport == 'Tennis')$Weight)
swimming_weight <- as.numeric(subset(olympics, Sex == 'M' & Sport == 'Swimming')$Weight)

mean(tennis_weight, na.rm=TRUE)
mean(swimming_weight, na.rm = TRUE)

# Тест на равенство дисперсий
bartlett.test(as.numeric(tennis_swimming_men$Weight) ~ tennis_swimming_men$Sport, data=tennis_swimming_men)

# Проверка, различаются ли выбранные средние значения с помощью теста Уэлча
t.test(as.numeric(tennis_swimming_men$Weight) ~ tennis_swimming_men$Sport)

# Проверка, при условии, что дисперсии равны
t.test(as.numeric(tennis_swimming_men$Weight) ~ tennis_swimming_men$Sport, paired = FALSE, var.equal = TRUE)
