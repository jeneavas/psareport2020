getwd()
setwd("C:/Users/Пыжак Юлия/Desktop/майнор/отчет1")
dir("отчет1")


# mydata <- read_excel("smalldata.xlsx")
# head(mydata)
#my_data <- read.table(file = "clipboard", 
#                      sep = "\t", header=TRUE)
#data <- read_excel("smalldata.xlsx")
#data

mydata <- (read.table("smalldata.csv", sep = ";", header = TRUE))
long <- as.character(mydata$life_longivity)
long <- as.numeric(long)
pollution <- as.numeric(as.character(mydata$air_pollution))
hap <- as.numeric(as.character(mydata$happiness))
med <- as.numeric(as.character(mydata$medicine_exp))
urb <- as.numeric(as.character(mydata$urbanization))
fail <- as.numeric(as.character(mydata$failed_ind))
country <- as.character(mydata$Country)
mydata

#Строим графики
library(ggplot2)
ggplot(data = mydata, aes(x = life_longivity, y = failed_ind)) + geom_point() +  stat_smooth(method = 'lm') + theme_bw() + labs(x = 'Средняя продолжительность жизни, годы', y = "Индекс несостоятельности")

# Ранжируем данные

# По загрязненности воздуха
order.poll <- order(mydata$air_pollution, decreasing = TRUE)
dat1 <- mydata[order.poll,]
dat1$rank <- rank(-dat1$air_pollution)
dat2 <- dat1[, -2]
rank_pollution <- dat2[ , -c(3:6)]

rank_pollution # рейтинг по загрязненности воздуха

# По продолжительности жизни
order.long <- order(mydata$life_longivity, decreasing = TRUE)
dat1 <- mydata[order.long,]
dat1$rank <- rank(-dat1$life_longivity)
rank_long <- dat1[ , -c(3:7)]

rank_long # рейтинг по продолжительности

# По уровню счастья
order.happ <- order(mydata$happiness, decreasing = TRUE)
dat1 <- mydata[order.happ,]
dat1$rank <- rank(-dat1$happiness)
rank_happ <- dat1[, -c(2, 3, 5:7)]

rank_happ # рейтинг по уровню счастья

# Урбанизация
order.urb <- order(mydata$urbanization, decreasing = TRUE)
dat1 <- mydata[order.urb,]
dat1$rank <- rank(-dat1$urbanization)
rank_urb <- dat1[, -c(2:5, 7)]

rank_urb # рейтинг по урбанизации

# По расходам не медицину
order.med <- order(mydata$medicine_exp, decreasing = TRUE)
dat1 <- mydata[order.med,]
dat1$rank <- rank(-dat1$medicine_exp)
rank_med <- dat1[, -c(2:4, 6:7)]

rank_med # по расходам на мед

# По уровню неустойчивости
order.fail <- order(mydata$failed_ind)
dat1 <- mydata[order.fail,]
dat1$rank <- rank(dat1$failed_ind)
rank_fail <- dat1[, -c(2:6)]

rank_fail  # по несостоятельности (0-самое состоятельное, 120 - наиболее несостоятельное)

head(mydata)

####################################################

# Кластерный анализ

library(cluster)
mydist <- daisy(mydata)
mydist

rownames(mydata) <-  mydata$Country
d <- dist(mydata, method = "euclidean")
d
head(mydata[,2:7])

# Ближний сосед
hc.s <- hclust(d, method = "single")
plot(hc.s, hang = -1, labels = mydata$Country)

# Дальний сосед
hc.c <- hclust(d, method = "complete")
plot(hc.c, hang = -1, labels = mydata$Country)

# Центроид
hc.cent <- hclust(d, method = "centroid")
plot(hc.cent, hang = -1, labels = mydata$Country, main = "Метод центра тяжести")

# Средняя связь
hc.a <- hclust(d, method = "average")
plot(hc.a)
plot(hc.a, hang = -1, labels = mydata$Country)

# Уорд
hc.w <- hclust(d, method = "ward.D2")
dend_w <- as.dendrogram(hc.w)
plot(dend_w)
plot(hc.w, hang = -1, labels = mydata$Country)

rect.hclust(hc.w, k =7, border = "red")

cutree(hc.w, k = 7)



groups <- cutree(hc.w, k = 7)
rect.hclust(as.dendrogram(hc.w), k=7, border="red")

fit <- as.dendrogram(hclust(d, method = "ward.D2"))
par(mar = rep(2, 4))
plot(fit)
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7, border="red")
?dist
class(mydata)

options(max.print = 1000)

# install.packages("factoextra")
library(factoextra)
fviz_dend(hc.w, cex = 0.5, sub = "")
fviz_dend(hc.c, h=90 , cex = 0.5, rect = TRUE, main = "Метод дальнего соседа")
?fviz_dend
fviz_dend(hc.a, k=7, cex = 0.5,rect = TRUE, main = "Средней связи") # скорее всего берем ее
fviz_dend(hc.w, k=7, cex = 0.5,rect = TRUE, main = "Уорд")

fviz_dend(hc.s, k=1, cex = 0.5,rect = TRUE, main = "Метод ближнего соседа")
fviz_dend(hc.cent, cex = 0.5,rect = TRUE, main = "Метод центра тяжести")


#####################################

# K-maens
kc <- kmeans(mydata[,2:7], 3)
clusters <- kc$cluster
kc$centers

kc$withinss
kc$betweenss
par(mar = rep(2, 4))
plot(life_longivity~air_pollution, mydata, col = kc$cluster)
 
fviz_nbclust(mydata[,2:7], hcut, method = "wss")



myvar <- c('long', "air", "hap", "med", "urb", "fail")
c1 <- c(69.56522, 35.60783, 4.891217, 4.926087, 39.53043, 85.58696)
c2 <- c(81.01714, 12.48457, 6.775000, 8.888571, 80.31429, 30.71143)
c3 <- c(75.25484, 23.85452, 5.529161, 6.467742, 72.08710, 65.92258)
center <- kc$centers
dist(center)

cluster <- c(1: 3)
center_df <- data.frame(myvar, c1, c2, c3)
center_df

library(ggplot2)
g.c1 <- ggplot(data = center_df, aes(x = myvar, y = c1, group = 1)) + geom_line(colour = 'green4') + theme_bw() + geom_point(colour = 'green4')
g.c2 <- ggplot(data = center_df, aes(x = myvar, y = c2, group = 1)) + geom_point(colour = "tomato2") + theme_bw() +geom_line(colour = "tomato2")
g.c3 <- ggplot(data = center_df, aes(x = myvar, y = c3, group = 1)) + geom_point(colour = "orange2") + theme_bw() +geom_line(colour = "orange2")

ggplot() + geom_line(data = center_df, aes(x = myvar, y = c1, group = 1), colour = 'green4') + geom_line(data = center_df, aes(x = myvar, y = c2, group = 1), colour = 'tomato2') + geom_line(data = center_df, aes(x = myvar, y = c3, group = 1), colour = 'orange2')+ theme_bw() + labs(x = 'Переменные', y = "Средние значения по кластерам")

library(dplyr)
ct <- mydata[,2:7] %>%mutate(Cluster = kc$cluster) %>% group_by(Cluster) %>% summarise_all("mean")
dist(ct)
cd
as.numeric(ct$life_longivity)
as.numeric(ct$air_pollution)
as.numeric(ct$happiness)
as.numeric(ct$medicine_exp)
as.numeric(ct$urbanization)
as.numeric(ct$failed_ind)

center_df
sc <- scale(center_df[,2:4])
sc.df <- data.frame(sc)
sc.df

ggplot() + geom_line(data = sc.df, aes(x = myvar, y = c1, group = 1), color = 'green4') +
  geom_line(data = sc.df, aes(x = myvar, y = c2, group = 1), color = 'tomato2') + 
  geom_line(data = sc.df, aes(x = myvar, y = c3, group = 1), color = 'orange2')+
  theme_bw() + labs(x = 'Переменные', y = "Нормализованные средние 
  значения по кластерам")

####################################################

# Расстояние между кластерами

center_df_tr <- t(center_df)
dist(center_df_tr[2:4,])

####################################################

#Проверка гипотезы о равенстве средних для c1 и c3

c1_df <- (read.table("cluster1.csv", sep = ";", header = TRUE))
c2_df <- (read.table("cluster2.csv", sep = ";", header = TRUE))
c3_df <- (read.table("cluster3.csv", sep = ";", header = TRUE))
class(c2_df$long)

t.test(c1_df$hap, c2_df$hap, alternarive = "two.sided", paired = FALSE)
t.test(c1_df$hap, c3_df$hap, alternarive = "two.sided", paired = FALSE)
t.test(c3_df$hap, c2_df$hap, alternarive = "two.sided", paired = FALSE)

t.test(c1_df$med, c2_df$med, alternarive = "two.sided", paired = FALSE)
t.test(c1_df$med, c3_df$med, alternarive = "two.sided", paired = FALSE)
t.test(c3_df$med, c2_df$med, alternarive = "two.sided", paired = FALSE)



t.test(c1_df$long, c2_df$long, alternarive = "two.sided", paired = FALSE)
t.test(c1_df$long, c3_df$long, alternarive = "two.sided", paired = FALSE)
t.test(c3_df$long, c2_df$long, alternarive = "two.sided", paired = FALSE)


x <- c(0, 0, 1, 1, 1, 1)
y <- c(1, 0, 1, 1, 0, 1)
dist(rbind(x, y), method = "binary")
rbind(x,y)

#########################################################

# Коэффициенты корреляции

cor(mydata[,2:7])
?cor
pcor(mydata[,2:7])

library(ggm)
pcor(mydata[,2:7])
?pcor
