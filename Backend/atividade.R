tabela <- read.table("./WAGE1.RAW",header=TRUE)

head(tabela)

class(tabela)

summary(tabela$wage)

hist(tabela$wage, xlab = "wage", main = "Histogram of wage", breaks = sqrt(nrow(tabela)) ) 

library(ggplot2) 

ggplot(tabela) + 
  aes(x = wage) + 	
  geom_histogram(bins = 30L, fill = "#0c4c8a") + 	theme_minimal()

boxplot(tabela$wage, ylab = "wage" )

ggplot(tabela) + 
  aes(x = "", y = wage) + 
  geom_boxplot(fill = "#0c4c8a") + 	theme_minimal()

boxplot.stats(tabela$wage)$out

out <- boxplot.stats(tabela$wage)$out 
out_ind <- which(tabela$wage %in% c(out))

boxplot(tabela$wage, 
        ylab = "wage", 
        main = "Boxplot of wage " ) 
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

-------------------------------------------------------------------------------
  
lower_bound <- median(tabela$wage) - 3 * mad(tabela$wage, constant = 1)
lower_bound

upper_bound <- median(tabela$wage) + 3 * mad(tabela$wage, constant = 1) 
upper_bound

outlier_ind <- which(tabela$wage < lower_bound | tabela$wage > upper_bound) 
outlier_ind


library(outliers) 
test <- grubbs.test(tabela$wage) 
test


test <- grubbs.test(tabela$wage, opposite = TRUE) 
test

library(mvoutlier) 
Y <- as.matrix(ggplot2::mpg[, c("cyl", "hwy")]) 
res <- aq.plot(Y)

-------------------------------------------------------------------------------
df=scale(tabela)
head(tabela, n=3)

library(cluster)
library(factoextra)
library(gridExtra)

max(tabela$wage)
min(tabela$wage)

dados.processados <- tabela[,c("wage","educ", "exper")] 

dados.k2 <- kmeans(dados.processados,centers = 2, nstart = 25, iter.max = 100)
dados.k3 <- kmeans(dados.processados,centers = 3, nstart = 25, iter.max = 100)
dados.k4 <- kmeans(dados.processados,centers = 4, nstart = 25, iter.max = 100)
dados.k5 <- kmeans(dados.processados,centers = 5, nstart = 25, iter.max = 100)

G1 = fviz_cluster(dados.k2, geom = "point", data  = dados.processados) + ggtitle("k = 2")
G2 = fviz_cluster(dados.k3, geom = "point", data  = dados.processados) + ggtitle("k = 3")
G3 = fviz_cluster(dados.k4, geom = "point", data  = dados.processados) + ggtitle("k = 4")
G4 = fviz_cluster(dados.k5, geom = "point", data  = dados.processados) + ggtitle("k = 5")

grid.arrange(G1,G2,G3,G4, nrow = 2)

-------------------------------------------------------------------------------
  
dadosFinal <- data.frame(tabela, dados.k2$cluster)

head(dadosFinal)

table(dados.k2$cluster)

par(mfrow=c(1,3))

boxplot(dadosFinal$wage ~ as.factor(dadosFinal$dados.k2.cluster), col = "blue", main = "remuneração")
boxplot(dadosFinal$educ ~ as.factor(dadosFinal$dados.k2.cluster), col = "blue", main = "educação")
boxplot(dadosFinal$exper ~ as.factor(dadosFinal$dados.k2.cluster), col = "blue", main = "experiência")

