input_table <- read.csv(file = "C:/Users/vzhal/OneDrive/Рабочий стол/Обработка больших данных/Lab8_Data.csv", 
                     sep = ",", header = TRUE, dec = ',')
definitions <- read.csv(file = "C:/Users/vzhal/OneDrive/Рабочий стол/Обработка больших данных/Lab8_Definition and Source.csv", 
                     sep = ",", header = TRUE, dec = ',')

# Данные по Австралии
australia <- subset(input_table, Country.Code == "AUS")

# Приведение датасета к необходимому формату
years <- c(1989:2017)
ind <- c("GDP", "GDP growth", "Births", "Birth rate", "Adjusted income", "Unemployment advanced", 'Unemployment basic', "Imports of goods", "Industry", "Government health",
         "Life expectancy", "Population", "Government expenditure", "Goods imports", "Exports of goods",
         "Death rate", "Educational total", "Educational female", "High-technology exports", " Best industry", "Scientific articles")
indicators <- australia[,3]
australia <- australia[,-c(1,2,3,4, 34)]
australia <- australia[-c(8,9),]

# Преобразование в числовой тип
australia <- sapply(australia, as.numeric)
# Транспонирование
australia <- t(australia)

rownames(australia) <- years
colnames(australia) <- ind
australia <- data.frame(australia)

# График распределения ВВП по годам
plot(years, australia[,1], main="Прирост ВВП", xlab='Годы', 
     ylab='ВВП (в долларах)', type='o', pch=20)

# Корреляционная матрица (учитываются все полные наблюдения для каждой пары переменных в отдельности)
cor(australia, use="pairwise.complete.obs")

install.packages(pkgs=c("ellipse"))
library(ellipse)
# Визуализация корреляционной матрицы
plotcorr(cor(australia, use="everything"), cex.lab = 0.6)

# Корреляция роста ВВП и прироста населения
cor(australia$GDP, australia$Population)
plot(australia$GDP, australia$Population, main='Зависимость популяции от ВВП',
     xlab="Прирост ВВП", ylab="Популяция")

# Корреляция прироста населения на динамику безработицы
cor(australia$Population, australia$Unemployment.basic, use='pairwise.complete.obs')
plot(australia$Population, australia$Unemployment.basic, 
     main="Корреляция прироста населения на динамику безработицы",
     xlab="Популяция", ylab="Безработица")

# Корреляция расходов на медицину на увеличение продолжительности жизни
cor(australia$Government.health, australia$Life.expectancy, use="pairwise.complete.obs")
plot(australia$Government.health, australia$Life.expectancy, 
     main="Корреляция расходов на медицину на увеличение продолжительности жизни",
     xlab='Расходы на медицину', ylab="Продолжительность жизни")

# Корреляция расходов на медицину на смертность
cor(australia$Government.health, australia$Death.rate, use="pairwise.complete.obs")
plot(australia$Government.health, australia$Death.rate,
     main="Корреляция расходов на медицину на смертность",
     xlab="Расходы на медицину", ylab="Смертность")

# Корреляция прироста людей с высшим образованием на рост экспорта товаров
cor(australia$Educational.total, australia$Exports.of.goods, use="pairwise.complete.obs")
plot(australia$Educational.total, australia$Exports.of.goods)

cor(australia$Educational.total, australia$X.Best.industry, use="pairwise.complete.obs")
plot(australia$Educational.total, australia$X.Best.industry)

cor(australia$Government.expenditure, australia$Educational.female, use="pairwise.complete.obs")
plot(australia$Government.expenditure, australia$Educational.female)

cor(australia$Educational.total, australia$Scientific.articles, use="pairwise.complete.obs")
plot(australia$Educational.total, australia$Scientific.articles)

library(car)
scatterplotMatrix(australia[,c(1,4,7,11,12)], spread=FALSE, lty.smooth=2)

model <- lm(australia$Life.expectancy ~ australia$GDP + I(australia$GDP^2), australia)
model
summary(model)

plot(australia$Life.expectancy ~ australia$GDP)
abline(model)

lines(australia$GDP, fitted(model))
