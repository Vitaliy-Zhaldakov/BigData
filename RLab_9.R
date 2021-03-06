library(igraph)

#1	�������� ��������� ����  g �� ��������� ������ ������ G_size (�� N+10 �� (N/10+5)2+5N). 
#�������� ����� ����� � ������  ����� �����. ��������� ����, �������� ��� ������� ���������.

G_size <- sample(c(14:40), replace=TRUE)
# ��������� ���� �� ��������� ������ ������
g <- graph.ring(G_size)
plot(g, main='��������� ����')

# �������
V(g)
# �����
E(g)
# ������� ���������
g[]

#2	��������  ���� g1 ��  ������� ����� � ������ ������ G_size  ������� �����.
#�������� ��� N*8 ��������� �����, �������������� �� ������� ������, ��������
#����� ������� ������, ��������� ���� � �������� ��� ������� ���������. �������� 
#����� g1 ���  N*10 ��������� �����, �������������� �� ������� ������, ��������
#����� ����� ������, ��������� ���� � �������� ��� ������� ���������. 

g1 <- graph.empty() + vertices(c(1:length(G_size)), color="yellow") 
plot(g1, layout=layout.circle, main='������ ����')

g1 <- g1 + edges(sample(V(g1), 32, replace=TRUE), color="red")
plot(g1, layout=layout.circle, main="�������� ������� �����", edge.arrow.size=.4)
g1[]

g1 <- g1 + edges(sample(V(g1), 40, replace=TRUE), color="blue") 
plot(g1, layout=layout.circle, main="�������� ����� �����", edge.arrow.size=.4)
g1[]


#3	�������� ����� ����� �������� 2N+23 � 2N+20, 2N+12 � N+15, 
#2N-1 � N+8, 2N � 2N+1, N+7 � N+13, �������� �� � ������ ���� 
#(�������������� ��������� ���������� �� ����� ������� � �������� %in% ���� match, 
#��� �������������� ������ ����� �� ����������).  ��������� ����. �������� ������� N - � 
#�������, �����, ����������� ���� �������. ��������� �� ������� N+10 � N+12?
#�������� ������� ���������.

# ��������� ������� ��������� ������
new_ver <- numeric()
pairs_vertices <- data.frame(c(31, 28), c(20, 19), c(7, 12), c(8, 9), c(11, 17))
for(i in 1:length(pairs_vertices)) {
  if(pairs_vertices[1,i] %in% V(g1) && pairs_vertices[2,i] %in% V(g1))
    new_ver <- append(new_ver, pairs_vertices[,i])
}

g1 <- g1 + edges(new_ver, replace=TRUE, color="black")
plot(g1, layout=layout.circle, main='�������� ������ �����', edge.arrow.size=.4)

neighbors(g1, V(g1)[4], mode = 1)
incident(g1, V(g1)[4], mode=("all"))
are_adjacent(g1, 14, 16)
g1[]

#4 	�������� ��� ���� ������� � ���������� �� � ���, ������� ����� ���������� ����������
#��������� � ��� �����. ��������� ����� ���� �������� 
#(��������, ����� � ���������� ������� � ����������� ��������� �, ���� �� ������,
#�������� �����). �������� ������� ���������. �������� �������, ��� ������� ��������
#��������� ������ 5 � ������ 2. 

# ������� � ���������� ��������� ���������
count_edges <- apply(g1[], MARGIN=1, sum)
max_ver <- which(count_edges == max(count_edges))

g1 <- g1 + vertices("Z", color="blue")
g1 <- g1 + edges(c(max_ver[[1]],"Z"), replace=TRUE, color="green")
plot(g1, layout=layout.circle, edge.arrow.size=.4, main='�������� ����� �������')
g1[]

# ������� �������, ��� ������� �������� ��������� ������ 5 � ������ 2.
count_edges <- apply(g1[], MARGIN=1, sum)
vertex <- numeric()
for (i in 1:length(count_edges)) {
  if(count_edges[[i]] < 5 && count_edges[[i]] > 2)
    vertex <- append(vertex, i)
}

#5 ���������� ��������� ���������� ������ �����
.
plot(g1, layout=layout_as_tree, edge.arrow.size=.4, main='������')

plot(g1, layout=layout.kamada.kawai, edge.arrow.size=.4, main='�������� ������-�����')

plot(g1, layout=layout.fruchterman.reingold, edge.arrow.size=.4, main='�������� �����������-����������')

#6 ��������� ��������� �������� ����� g1, �������� ������ 
#����� �������� �����  ��� ������ ������� � ������������ �������� ������ �������� �� ��������.

diameter(g1)

# ������ ����� �������� ����� ��� ������ �������
list <- list()
for (i in 1:length(V(g1))) {
list <- append(list, all_shortest_paths(g1, i, to = V(g1), mode = c("out", "all", "in"), weights = NULL))
}

# ������� ������
deg <- degree(g1, mode="all")
deg
# ���������� ������, �������� �� ��������
plot(g1, vertex.size=deg*1.5, edge.arrow.size=.4, main='���������� ������')


#-----------------------������ ���� �����------------------------
#4.������� M (M < 50) �������, � ������ �� ������� ��������� ������������� ��������������. 
#�������� ��������� ������� � ������ ������ � ��������� ������� ����� ��������. 
#��������, ��� �� ����� ����� �������� ���� ������ ������.  
#��������� � ������ ���������� ���������������� ������������� �������. 
#��� ������� ������� ������ ���������� �����, ���� �� ������� ��������, 
#����� ����������� ����� ������� ������� 
#������ ������������ ��������� �������.

library(igraph)
num <- sample(1:50, 1)
town_costs <- sample(1:50, num, replace = FALSE)
weight <- sample(1:120, 120, replace = FALSE)

# ���������� �����
g <- graph.empty(num, directed=FALSE)
g <- g + edges(sample(V(g), 110, replace=TRUE), color="red", weights=weight)
plot(g, main="����� �������")
E(g)
E(g)$weights

# ����� ���������� ���������� ����� �� ������ ������� � ����� ������ ��������
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
