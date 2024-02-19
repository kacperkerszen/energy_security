##################################################################

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

#################################################################

scd <- scale (dane, center = T)
scd
scd = round(scd, digits = 4)
View(scd)

################################################################################

mean(dane$X1)                                  
median(dane$X1)                                
quantile(dane$X1)                               
quantile(dane$X1, c(0.1, 0.25, 0.75, 0.9))
pvec <- seq(0, 1, 0.25)
pvec
quantile(dane$X1, pvec)
pvec <- seq(0, 1, 0.1)
pvec
quantile(dane$X1, pvec)                        
IQR(dane$X1)                                   
var(dane$X1)                                   
sd(dane$X1)                                     
v <- (sd(dane$X1) / mean(dane$X1))*100
v
sqrt(var(dane$X1))

################################################################################

install.packages(e1071)     # Packeges / Install
library(e1071)

skewness(dane$X1)

summary(dane)     

################################################################################

install.packages("psych")
library(psych)

describe(dane)

########################## GINI ################################

install.packages("EnvStats")               
library(EnvStats)
install.packages("laeken")
library(laeken)
install.packages("tidyverse")
library(tidyverse)

gini_c=c()

for(i in 1:ncol(dane))
{
  value_gini = apply(dane, 2, gini)
  search_value=value_gini[[i]]$value
  gini_c=c(gini_c, search_value)
}

result=dane %>%
  reframe(zmienne = colnames(dane),
          średnia = (colMeans(dane)),
          odchylenie = apply(dane, 2, sd),
          skośność = apply(dane, 2, skewness),
          współczynnik_zmienności = odchylenie / średnia *100)

result$gini = gini_c
for(i in 2:ncol(result))
{
  result[,i] = round(as.numeric(unlist(result[,i])), 2)
}

result

View(result)

#######################################################################
###################################################################

install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

hist(dane$X1)
hist(dane$X1, breaks = 4)

h <- hist(dane$X2, ylim = c(0,3))
h <- hist(dane$X2, ylim = c(0,6))

hist(dane$X2, 
     breaks = 5, 
     main = "Histogram of energy efficiency", 
     xlab= "X2", 
     ylab = "Number of observations")

hist(dane$X2,
     main = "Histogram of energy efficiency",
     xlab = "X2",
     xlim = c(0,10),
     ylab = "Number of observations",
     col = "darkmagenta",
     freq = TRUE)

hist(dane$X2,
     main = "Histogram of energy efficiency",
     xlab = "X2",
     xlim = c(0,10),
     ylim = c(0,6),
     col = "darkblue",
     freq = TRUE)

X1i <- ggplot(dane, aes(x = X1)) +
  geom_histogram(color = "black", fill = "blue", bins = 5)
X2i <- ggplot(dane, aes(x = X2)) +
  geom_histogram(color = "black", fill = "green", bins = 5)
X3i <- ggplot(dane, aes(x = X3)) +
  geom_histogram(color = "black", fill = "yellow", bins = 5)
X4i <- ggplot(dane, aes(x = X4)) +
  geom_histogram(color = "black", fill = "red", bins = 5)

grid.arrange(X1i, X2i, X3i, X4i)

################################################################################

epdfPlot(dane$komfort, xlab = "Nuclear power")
epdfPlot(dane$komfort, epdf.col = "red",  main = "number of observations distribution", xlab = "X4")

############################## BOXPLOT #########################################

boxplot(dane$X2)

boxplot(dane$X2,
        main = "Histogram of energy efficiency",
        xlab = "X2",
        ylab = "N of observations",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE)

boxplot(dane$X2,
        main = "Histogram of energy efficiency",
        xlab = "X2",
        ylab = "N of observations",
        col = "springgreen1",
        border = "brown",
        horizontal = FALSE,
        notch = FALSE)

boxplot(dane$X2,
        main = "Histogram of energy efficiency",
        xlab = "X2",
        ylab = "N of observations",
        col = "turquoise1",
        border = "maroon1",
        horizontal = FALSE,
        notch = TRUE)

################################################################################

install.packages("lattice")
library(lattice)

X1a <- ggplot(dane, aes(x = X1)) +
  geom_boxplot(color = "black", fill = "lightyellow")
X2a <- ggplot(dane, aes(x = X2)) +
  geom_boxplot(color = "black", fill = "lightpink")
X3a <- ggplot(dane, aes(x = X3)) +
  geom_boxplot(color = "black", fill = "darkslategray1")
X4a <- ggplot(dane, aes(x = X4)) +
  geom_boxplot(color = "black", fill = "aquamarine")

grid.arrange(X1a, X2a, X3a, X4a)

##################################################

plot(dane$X1 ~ dane$X2, lty = 4, col = "red", lwd = 5, 
     main = "WYKRES ROZRZUTU", col.main = "RED", xlab = "X1",
     ylab = "X2")

plot(dane$X1 ~ dane$X2, type = "p",
     col = "tomato", 
     lwd= 1,
     cex = 1.5,
     pch = 25,
     main = "Wykres rozrzutu", 
     xlab = "X1",
     ylab = "X2")

plot(dane$X1, dane$X2, pch = 16, col = gray(.1, .9))

############ 

install.packages("yarrr")
library(yarrr)
yarrr::piratepal("all")                     # paleta kolorów

plot(x = dane$X1, 
     y = dane$X2,
     xlab = "X1", 
     ylab = "X2", 
     pch = 16, 
     col = yarrr::piratepal("nemo", trans = .3)) 

ggplot(dane, aes(x=X1, y=X2)) +
  geom_point() + # Show dots
  geom_text(
    label=rownames(dane2), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T)  

ggplot(dane, aes(x=X1, y=X2)) +
  geom_point() + # Show dots
  geom_label(label=rownames(dane2), col = "darkmagenta",
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T)  

################################################################################

install.packages("corrplot")
library(corrplot)

cor(dane)                    # Pearson
kor=round(cor(dane), 4)
print(kor)

corrplot(kor, method = "circle")
corrplot(kor, method = "square")
corrplot(kor, method = "ellipse")
corrplot(kor, method = "number")
corrplot(kor, method = "color")
corrplot(kor, method = "pie")
corrplot.mixed(kor, lower = "ellipse", upper = "number")

###########################################


install.packages("ggExtra")
library(ggExtra)

p <-ggplot(dane, aes(X1, X2, color = X3, 
                     size=2)) + geom_point() + theme(legend.position = "none")
ggMarginal(p, type= "boxplot")

p <- ggplot(dane, aes(X1, X2, 
                      size=2)) + geom_point() + theme(legend.position = "none") 
ggMarginal(p, type="boxplot")

################################################################################

par(mfrow = c(1,2))
corrplot(kor, method = "circle")
corrplot(kor, method = "square")
corrplot(kor, method = "ellipse")
corrplot(kor, method = "number")
corrplot(kor, method = "color")
corrplot(kor, method = "pie")

par(mfrow = c(2,2))
corrplot(kor, method = "circle")
corrplot(kor, method = "square")
corrplot(kor, method = "ellipse")
corrplot(kor, method = "number")

par(mfrow = c(1,1))
corrplot(kor, method = "circle")
