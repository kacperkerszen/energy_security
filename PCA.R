############################### DATA ###########################################

install.packages("readxl")
library(readxl)

dane1 <- read_excel("~/indicators.xlsx")
dane1 <- as.data.frame(dane1)
str(dane1)
View(dane1)

panstwa = dane1[, 1]
panstwa

dane <- dane1[,c(2:16)]                   
dane
View(dane)

dane2 <- dane1[,-1]
row.names(dane2) <- panstwa
View(dane2)

################################################################################

install.packages("e1071")     # Packeges / Install
library(e1071)
install.packages("psych")
library(psych)

####################################################################

install.packages("corrplot")
library(corrplot)
install.packages("MASS")
library(MASS)
install.packages("ppcor")
library(ppcor)


cor(dane)                       
cor(dane[c(1, 3, 4, 5, 6, 8, 11)])        
kor=round(cor(dane), 3)
print(kor)
pcor(dane)                      
pkor=round(pcor(dane)$estimate, 3)
print(pkor)

corrplot(kor, method = "circle")
corrplot(pkor, method = "circle")
par(mfrow = c(1,2))
corrplot(kor, method = "circle")
corrplot(pkor, method = "circle")

par(mfrow = c(1,1))  

################################################################################

install.packages("corrr")
library(corrr)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")
install.packages("factoextra")
library(factoextra)
install.packages("ggrepel")
library(ggrepel)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggfortify")
library(ggfortify)

################################################################################

dane_pca <- prcomp(dane[, c(1, 3, 4, 5, 6, 8, 11)], scale = TRUE)


# wartości własne
eig.val <- get_eigenvalue(dane_pca)
eig.val
cumsum(eig.val)

# ładunki składowe
print(dane_pca)                               
View(dane_pca)

dane_pca$x                         # współrzędne przypadków

# korelacje zmienncyh ze składową
pc.dane <- princomp(dane[, c(1, 3, 4, 5, 6, 8, 11)], cor = TRUE)     
str(pc.dane)
View(pc.dane)
round(cor(dane[, c(1, 3, 4, 5, 6, 8, 11)], pc.dane$scores), 3)     

pc.dane$loadings              # ładunki składowe
pc.dane$sdev                  # TEGO RACZEJ NIE STOSOWAĆ!!! odchylenie standardowe składowych głównych
pc.dane$center                # średnie zmiennych
pc.dane$scale                 # odchylenie standardowe zmiennych
pc.dane$scores                # współrzędne przypadków
View(pc.dane$scores)

################################################################################


dev.off()
screeplot(dane_pca, type = "lines", main = "Graph of settlement", col = "#2B766D", pch = 19)
abline(1,0, col = 'red', lty = 2)   


screeplot(dane_pca, type = "barplot", col = "lightblue", main = "")                 
abline(1,0, col = 'red', lty = 2)

fviz_eig(dane_pca, addlabels = TRUE, 
ylim = c(0, 60),
main="Scree Plot")

install.packages("GGally")
library(GGally)
install.packages("ggrepel")
library(ggrepel)

pc_dane_df <- as.data.frame(pc.dane$scores)
ggplot(pc_dane_df,  aes(Comp.1, Comp.2))+
  geom_point() + 
  geom_text_repel(
    label=rownames(dane2),
    colour = "#2B766D",
    nudge_x = 0.2, nudge_y = 0.2
    )  


################################################################################
library("factoextra")

panstwa = dane1[, 1]
View(panstwa)

fviz_pca_biplot(dane_pca, 
                repel = TRUE,
                col.var = "deepskyblue",
                title = "Biplot", geom="point")

fviz_pca_biplot(dane_pca, repel = FALSE, col.var = "blue", col.ind = "black") + geom_text(
  label=row.names(dane2))

library(ggrepel)


fviz_pca_biplot(dane_pca, col.var = "#2B766D", col.ind = "black") +
  geom_text_repel(label = panstwa)



fviz_pca_var(dane_pca, repel = TRUE,
                col.var = "blue",
                col.ind = "black")

fviz_pca_var(dane_pca, col.var = "cos2", 
             gradient.cols = c("lightblue", "black", "orange"), repel = TRUE)

################################################################################



