library(igraph)

#1	Создайте кольцевой граф  g со случайным числом вершин G_size (от N+10 до (N/10+5)2+5N). 
#Выведите число ребер и вершин  этого графа. Постройте граф, выведите его матрицу смежности.

G_size <- sample(c(14:40), replace=TRUE)
# Кольцевой граф со случайным числом вершин
g <- graph.ring(G_size)
plot(g, main='Кольцевой граф')

# Вершины
V(g)
# Ребра
E(g)
# Матрица смежности
g[]

#2	Создайте  граф g1 из  пустого графа с числом вершин G_size  желтого цвета.
#Добавьте ему N*8 случайных ребер, сформированных из вектора вершин, окрасьте
#ребра красным цветом, нарисуйте граф и выведите его матрицу смежности. Добавьте 
#графу g1 еще  N*10 случайных ребер, сформированных из вектора вершин, окрасьте
#ребра синим цветом, нарисуйте граф и выведите его матрицу смежности. 

g1 <- graph.empty() + vertices(c(1:length(G_size)), color="yellow") 
plot(g1, layout=layout.circle, main='Пустой граф')

g1 <- g1 + edges(sample(V(g1), 32, replace=TRUE), color="red")
plot(g1, layout=layout.circle, main="Добавили красные ребра", edge.arrow.size=.4)
g1[]

g1 <- g1 + edges(sample(V(g1), 40, replace=TRUE), color="blue") 
plot(g1, layout=layout.circle, main="Добавили синие ребра", edge.arrow.size=.4)
g1[]


#3	Добавьте ребра между вершиной 2N+23 и 2N+20, 2N+12 и N+15, 
#2N-1 и N+8, 2N и 2N+1, N+7 и N+13, окрасьте их в черный цвет 
#(предварительно проверьте существуют ли такие вершины – функцией %in% либо match, 
#для несуществующих вершин ребра не добавляйте).  Нарисуйте граф. Выведите соседей N - й 
#вершины, ребра, инцидентные этой вершине. Соединены ли вершины N+10 и N+12?
#Выведите матрицу смежности.

# Проверяем наличие требуемых вершин
new_ver <- numeric()
pairs_vertices <- data.frame(c(31, 28), c(20, 19), c(7, 12), c(8, 9), c(11, 17))
for(i in 1:length(pairs_vertices)) {
  if(pairs_vertices[1,i] %in% V(g1) && pairs_vertices[2,i] %in% V(g1))
    new_ver <- append(new_ver, pairs_vertices[,i])
}

g1 <- g1 + edges(new_ver, replace=TRUE, color="black")
plot(g1, layout=layout.circle, main='Добавили чёрные ребра', edge.arrow.size=.4)

neighbors(g1, V(g1)[4], mode = 1)
incident(g1, V(g1)[4], mode=("all"))
are_adjacent(g1, 14, 16)
g1[]

#4 	Добавьте еще одну вершину и подключите ее к той, которая имеет наибольшее количество
#связанных с ней узлов. Присвойте имена всем вершинам 
#(например, буквы в алфавитном порядке – используйте заглавные и, если не хватит,
#строчные буквы). Выведите матрицу смежности. Выберите вершины, для которых значение
#связности меньше 5 и больше 2. 

# Вершины с наибольшим значением связности
count_edges <- apply(g1[], MARGIN=1, sum)
max_ver <- which(count_edges == max(count_edges))

g1 <- g1 + vertices("Z", color="blue")
g1 <- g1 + edges(c(max_ver[[1]],"Z"), replace=TRUE, color="green")
plot(g1, layout=layout.circle, edge.arrow.size=.4, main='Добавили новую вершину')
g1[]

# Выберем вершины, для которых значение связности меньше 5 и больше 2.
count_edges <- apply(g1[], MARGIN=1, sum)
vertex <- numeric()
for (i in 1:length(count_edges)) {
  if(count_edges[[i]] < 5 && count_edges[[i]] > 2)
    vertex <- append(vertex, i)
}

#5 Испробуйте алгоритмы размещения Вашего графа
.
plot(g1, layout=layout_as_tree, edge.arrow.size=.4, main='Дерево')

plot(g1, layout=layout.kamada.kawai, edge.arrow.size=.4, main='Алгоритм Камада-Каваи')

plot(g1, layout=layout.fruchterman.reingold, edge.arrow.size=.4, main='Алгоритм Фрюхтермана-Рейнгольда')

#6 Выполните измерение диаметра графа g1, выведите список 
#самых коротких путей  для каждой вершины и откалибруйте величины вершин согласно их степеней.

diameter(g1)

# Список самых коротких путей для каждой вершины
list <- list()
for (i in 1:length(V(g1))) {
list <- append(list, all_shortest_paths(g1, i, to = V(g1), mode = c("out", "all", "in"), weights = NULL))
}

# Степени вершин
deg <- degree(g1, mode="all")
deg
# Калибровка вершин, согласно их степеней
plot(g1, vertex.size=deg*1.5, edge.arrow.size=.4, main='Калибровка вершин')


#-----------------------Второй блок задач------------------------
#4.Имеется M (M < 50) городов, в каждом из которых открылась кооперативная парикмахерская. 
#Известна стоимость стрижки в каждом городе и стоимость проезда между городами. 
#Известно, что не между всеми городами есть прямая дорога.  
#Стоимость и проезд выражаются неотрицательными вещественными числами. 
#Для агентов каждого города определить город, куда им следует съездить, 
#чтобы подстричься самым дешевым образом 
#Данные сформировать случайным образом.

library(igraph)
num <- sample(1:50, 1)
town_costs <- sample(1:50, num, replace = FALSE)
weight <- sample(1:120, 120, replace = FALSE)

# Построение графа
g <- graph.empty(num, directed=FALSE)
g <- g + edges(sample(V(g), 110, replace=TRUE), color="red", weights=weight)
plot(g, main="Схема городов")
E(g)
E(g)$weights

# Поиск кратчайших взвешенных путей из каждых городов и выбор самого дешевого
ver <- integer()
result <- integer()
for (i in 1:num) {
  list <- shortest_paths(g, from=i, to=V(g))
  for (j in 1:length(list$vpath)) {
    if(list$vpath[[1]][1] != tail(list$vpath[[j]], n=1))
    ver <- append(ver, as.numeric(tail(list$vpath[[j]], n=1)))
  }
  result <- append(result, which(town_costs == town_costs[sample(ver, 1)]))
  ver <- integer()
}
