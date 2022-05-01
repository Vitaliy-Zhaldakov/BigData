# Великобритания - Теннис
# Места 1-8
places <- read.csv(file = "C:/Users/vzhal/OneDrive/Рабочий стол/Обработка больших данных/GreatBritain.csv", 
                          sep = ";", row.names = 1, header = TRUE)

allPlaces <- places[,2]
olympics <- rownames(places)

# Значение для восстановления параметров
default <- par(no.readonly = TRUE)
par(mar = c(5, 10, 4, 2))
barplot(allPlaces, names.arg = olympics, horiz = TRUE, main="Диаграмма числа 1-8 мест сборной Великобритании по теннису",
        xlab="Число мест", las=1)
par(default)

# 1-е места 
first <- subset(places, Первые > 0, select = "Первые")

percentage = round(100 * first[,1] / sum(first[,1]), 1)
pieNames <- paste(rownames(first), " (", percentage, "%)", sep="")

pie(first[,1], pieNames, radius = 1.5, col=rainbow(6), 
    main="Распределение числа первых мест сборной Великобритании по теннису")

# Призовые места мужчин и женщин
menWomen <- read.csv(file = "C:/Users/vzhal/OneDrive/Рабочий стол/Обработка больших данных/MenWomen.csv", 
                     sep = ";", row.names = 1, header = TRUE)

years <- c(1924, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)
plot(years, menWomen[,1], type='o', lty=1, ylim=c(0, 3), pch=20, col="red",
     main="Тенденции изменения количества призовых мест",
     xlab="Годы олимпиад", ylab="Число призовых мест")
lines(years, menWomen[,2], type='o', lty=1, pch=20, col='blue')
legend('topright', c('Мужчины', 'Женщины'), lty=c(1, 1), col=c('red', 'blue'))

# Первые места последние 4 летние олимпиады
firstPlaces <- read.csv(file = "C:/Users/vzhal/OneDrive/Рабочий стол/Обработка больших данных/First Places.csv", 
                   sep = ";", row.names = 1, header = TRUE)

years <- c(2008, 2012, 2016, 2020)
plot(years, firstPlaces[,1], type='o', lty=1, pch=20, col='brown',
     main='Тенденции изменения количества золотых медалей',
     xlab='Четыре последние летние олимпиады',
     ylab='Число медалей',
     ylim=c(7, 51))
lines(years, firstPlaces[,2], type='o', lty=1, pch=10, col='green')
lines(years, firstPlaces[,3], type='o', lty=1, pch=15, col='red')
lines(years, firstPlaces[,4], type='o', lty=1, pch=17, col='blue')
lines(years, firstPlaces[,5], type='o', lty=1, pch=18, col='black')
lines(years, firstPlaces[,6], type='o', lty=1, pch=12, col='pink')
lines(years, firstPlaces[,7], type='o', lty=1, pch=13, col='purple')

legend('topright', c('США', 'Великобритания', "Китай", "Россия", "Германия", "Япония", "Франция"),
       pch=c(20,10,15,17,18,12,13), lty=c(1,1,1,1,1,1,1),
       col=c('brown', 'green', 'red', 'blue', 'black', 'pink', 'purple'),
       y.intersp = 0.2, text.width = 1)

# Призовые места последние 4 летние олимпиады
prizePlaces <- read.csv(file = "C:/Users/vzhal/OneDrive/Рабочий стол/Обработка больших данных/Prize Places.csv", 
                   sep = ";", row.names = 1, header = TRUE)

plot(years, prizePlaces[,1], type='o', lty=1, pch=20, col='brown',
     main='Тенденции изменения количества призовых мест',
     xlab='Четыре последние летние олимпиады',
     ylab='Число медалей',
     ylim=c(25, 121))
lines(years, prizePlaces[,2], type='o', lty=1, pch=10, col='green')
lines(years, prizePlaces[,3], type='o', lty=1, pch=15, col='red')
lines(years, prizePlaces[,4], type='o', lty=1, pch=17, col='blue')
lines(years, prizePlaces[,5], type='o', lty=1, pch=18, col='black')
lines(years, prizePlaces[,6], type='o', lty=1, pch=12, col='pink')
lines(years, prizePlaces[,7], type='o', lty=1, pch=13, col='purple')

legend('topright', c('США', 'Великобритания', "Китай", "Россия", "Германия", "Япония", "Франция"),
       pch=c(20,10,15,17,18,12,13), lty=c(1,1,1,1,1,1,1),
       col=c('brown', 'green', 'red', 'blue', 'black', 'pink', 'purple'),
       y.intersp = 0.2, text.width = 1)

# Призовые места мужчин и женщин
menWomen <- read.csv(file = "C:/Users/vzhal/OneDrive/Рабочий стол/Обработка больших данных/menWomenPlaces.csv", 
                     sep = ";", row.names = 1, header = TRUE)

years <- c(2004, 2008, 2012, 2016, 2020)

# График
plot(years, menWomen[,1], type='o', lty=1, pch=20, col='red',
     main='Тенденции изменения количества призовых мест по теннису среди мужчин и женщин',
     xlab='Пять последних летних олимпиад',
     ylab='Число медалей',
     ylim=c(1, 5))
lines(years, menWomen[,2], type='o', lty=1, pch=20, col='blue')

legend('topright', c("Мужчины", "Женщины"),
       lty=c(1,1),
       col=c('red', 'blue'),
       y.intersp = 0.2, text.width = 1)

# Столбчатая диаграмма
barplot(data.matrix(menWomen), beside=TRUE,
        col=topo.colors(5),
        main="Призовые места мужчин и женщин по теннису",
        ylab="Число медалей", ylim = c(0, 7))

legend('topright', rownames(menWomen), pch=15,
       col = topo.colors(5),
       y.intersp = 0.4, text.width = 1.5)

# Пирожная диаграмма
default <- par(no.readonly = TRUE)
par(mfrow=c(1,2), mar=c(5,1,2,1))
pie(menWomen[,1], rownames(menWomen), radius = 1.5, col=rainbow(6), 
    main="Распределение числа призовых мест по теннису среди мужчин")

pie(menWomen[,2], rownames(menWomen), radius = 1.5, col=rainbow(6), 
    main="Распределение числа призовых мест по теннису среди женщин")
par(default)
