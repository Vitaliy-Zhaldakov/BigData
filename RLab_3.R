table <- read.csv(file = "C:/Users/vzhal/OneDrive/Рабочий стол/Обработка больших данных/colors.csv", 
                    header = TRUE, sep = ";", row.names = 1)

# Импорт из Exel
library(xlsx)
tableExel <- read.xlsx(file = "C:/Users/vzhal/OneDrive/Рабочий стол/Обработка больших данных/colors.csv", 1)

colors <- c('green', 'red', 'blue', 'white', 'purple', 'yellow', 'pink')

modaFun <- function(vec){
  moda <- unique(vec)
  moda[which.max(tabulate(match(vec, moda)))]
}

info <- summary(table)
moda <- apply(table, 2, modaFun)
median <- apply(table, 2, median)
# Стандартное отклонениe
sd <- apply(table, 2, sd)
# Дисперсия
var <- apply(table, 2, var)
# Межквартильный размах
iqr <- apply(table, 2, IQR)

# Сортировка по розовому цвету
sort_pink <- table[order(table$pink),]


boxplot(table, main="Предпочтения",
        xlab="Цвета",
        ylab="Оценки",
        col=colors)

hist(data.matrix(table),
     main="Гистограмма оценок",
     xlab="Оценки",
     ylab="Количество")

# Выборки
loveRed <- subset(table, red > 7, select = red)
newData <- subset(table, black < 7 & pink > 7, select = c(black, pink))

# Плотность
plot(density(data.matrix(table)),
     main = "Плотность оценок",
     xlab = "Оценки",
     ylab = "Плотность")
