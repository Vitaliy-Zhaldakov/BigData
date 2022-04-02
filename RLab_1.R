# Задание 8
income = c(10000, 32000, 28000, 150000, 65000, 1573)
income_class = as.numeric(income >= mean(income))

# Задание 4
df = data.frame(var1 = c(1,2,3), var2 = c(4,5,6), var3 = c(7,8,0), var4 = c(9,10,11), row.names = c("case1", "case2", "case3"))

# 1.1
df['case1', 1:3]

# 1.2
df["case3", which(df["case3",] < 8)]

# 1.3
colnames(df)[2] = "second"; colnames(df)[4] = "forth"

# 1.4
df[["Y"]] = c(-10, 0, 11)

# 1.5
df = df[-c(2),]

# 1.6
df[,2] = df[,2]^3


# Задание 18
n = readline(); sample(1:100, n, replace = FALSE)

first = c(4,7,2)
second = c("London", "is", "capital")
third = c(TRUE, FALSE, TRUE)
fourth = c(10 + 5i, 2i, 6)

df = data.frame(first, second, third, fourth)

colnames(df) = c(class(first), class(second), class(third), class(fourth))

dim(df)