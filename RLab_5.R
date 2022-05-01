install.packages("rvest")
library(rvest)

# Страны
# Адрес основной веб-страницы
main_url <- "https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title="

years <- (2014:2021)

UK <- data.frame()
USA <- data.frame()
Canada <- data.frame()
India <- data.frame()
Kenya <- data.frame()

# Года Кении
Kenya_years <- data.frame()

# Создание дата фреймов для каждой страны
for(i in 1:8) {  
  # Читаю веб-страницу каждого года
  url <- read_html(paste0(main_url, years[i]))
  
  # Извлекаю табличный узел
  node <- html_node(url, '#t2')
  
  # Создаю массив таблиц
  table <- html_table(node) %>% as.data.frame()
  table <- table[-1]
  UK <- rbind(UK, subset(table, Country == 'United Kingdom'))
  USA <- rbind(USA, subset(table, Country == 'United States'))
  Canada <- rbind(Canada, subset(table, Country == 'Canada'))  
  India <- rbind(India, subset(table, Country == 'India'))
  Kenya <- rbind(Kenya, subset(table, Country == 'Kenya'))
  
  # Проверка Кении в таблице
  if (length(subset(table, Country == 'Kenya')[[i]]) == TRUE)
    Kenya_years <- rbind(Kenya_years, years[i])
}

rownames(UK) <- years
rownames(USA) <- years
rownames(Canada) <- years
rownames(India) <- years
rownames(Kenya) <- Kenya_years[,1]

# Построение графиков по индексу качества жизни
plot(years, UK[,2], type='o', lty=1, pch=20, col='blue',
      main='Изменение индекса качества жизни',
      xlab='Годы',
      ylab='Значение индекса',
     ylim=c(7, 196))
lines(years, USA[,2], type='o', lty=1, pch=20, col='red')
lines(years, Canada[,2], type='o', lty=1, pch=20, col='black')
lines(years, India[,2], type='o', lty=1, pch=20, col='green')
lines(Kenya_years[,1], Kenya[,2], type='o', lty=1, pch=20, col='orange')


legend('bottomleft', c('UK', 'USA', "Canada", "India", "Kenya"),
       pch=c(20,20,20,20,20), lty=c(1,1,1,1,1),
       col=c('blue', 'red', 'black', 'green', 'orange'),
       y.intersp = 0.2, text.width = 0.5)

# Диаграмма остальных показателей
barplot(data.matrix(UK[-c(1,2)]), beside=TRUE,
        col=rainbow(8),
        main="Диаграмма дополнительных показателей качества жизни",
        ylab="Значение индекса", ylim=c(0,140))

legend('topright', rownames(UK), pch=15,
       col = rainbow(8),
       y.intersp = 0.4, text.width = 1.5)

# Показатели стран 2021 года
df <- rbind(UK[8,])
df <- rbind(df, USA[8,])
df <- rbind(df, Canada[8,])
df <- rbind(df, India[8,])
df <- rbind(df, Kenya[3,])
rownames(df) <- df[,1] 
df <- df[,-1]

barplot(data.matrix(df), beside=TRUE,
        col=rainbow(8),
        main="Диаграмма показателей качества жизни 2021 года",
        ylab="Значение индекса", ylim=c(0,170))

legend('topright', rownames(df), pch=15,
       col = rainbow(8),
       y.intersp = 0.4, text.width = 7)


# Музеи
# Для первой страницы
main_url <- read_html('https://tonkosti.ru/Музеи_Санкт-Петербурга')

# класс ссылки с названием музея
selector <- ".places-list__item-header"
museums <- html_nodes(main_url, selector) %>% html_text() %>% as.array(); museums

selector <- ".places-list__address--rc"
addresses <- html_nodes(main_url, selector) %>% html_text() %>% as.array(); addresses

selector <- ".places-list__item-img--rc"
links <- html_nodes(main_url, selector) %>% html_attr('href'); links
links <- paste0('https://tonkosti.ru', links); links


# Для всех страниц
museums <- vector()
addresses <- vector()
links <- vector()
for(i in 1:5) {
  url <- read_html(paste0('https://tonkosti.ru/Музеи_Санкт-Петербурга?page=', i))
  
  selector <- ".places-list__item-header"
  vector <- html_nodes(url, selector) %>% html_text() %>% as.array()
  museums <- append(museums, vector)
  
  selector <- ".places-list__address--rc"
  vector <- html_nodes(url, selector) %>% html_text() %>% as.array()
  addresses <- append(museums, vector)
  
  selector <- ".places-list__item-img--rc"
  vector <- html_nodes(url, selector) %>% html_attr('href')
  links <- append(links, vector)
}
museums
addresses
links <- paste0('https://tonkosti.ru', links); links



# selector <- '.js-NavP2'
# pages <- html_node(main_url, selector) %>% html_attr('href'); pages
# selector <- '.js-paginator-tt'
# pages <- html_node(main_url, selector) %>% html_children(); pages

