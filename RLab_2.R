# Импорт таблицы
table <- read.table(file = "colors.txt", header = TRUE, sep = "", row.names = 1)

maxVec <- apply(table, MARGIN = 2, max)
minVec <- apply(table, MARGIN = 2, min)
meanVec <- apply(table, MARGIN = 2, mean)

summary(table)

# Подсчёт количества людей, которым понравился
like <- function(table)
{
  vec <- vector()
  for(i in 1:ncol(table)){
    vec[i] <- length(table[table[,i] > 6, i])
  }
  vec
}

# Подсчёт количества людей, которым не понравился
dont_like <- function(table)
{
  vec <- vector()
  for(i in 1:ncol(table)){
    vec[i] <- length(table[table[,i] < 4, i])
  }
  vec
}

num_like <- like(table)
num_like <- setNames(num_like, colnames(table))

num_dont_like <- dont_like(table)
num_dont_like <- setNames(num_dont_like, colnames(table))

rating <- names(sort(meanVec, decreasing = TRUE))

barplot(meanVec, space = 0, col = colnames(table),
        xlab = "Цвета",
        ylab = "Ср. значение")

hist(meanVec)
meanVec
