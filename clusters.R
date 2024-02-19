install.packages("readxl")
install.packages("vegan")
install.packages("factoextra")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("ggrepel")
install.packages("corrplot")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("lessR")
install.packages("psych")
install.packages("EnvStats")
install.packages("ellipse")
install.packages("FactoMineR")
install.packages("cluster")
install.packages("class")
install.packages("laeken")
install.packages("stats")

library(readxl)
library(vegan)
library(factoextra)
library(ggplot2)
library(ggfortify)
library(ggrepel)
library(corrplot)
library(dplyr)
library(gridExtra)
library(lessR)
library(psych)
library(EnvStats)
library(ellipse)
library(FactoMineR)
library(cluster)
library(class)
library(laeken)
library(stats)


#################### DANE ###############################

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


###############################################################

install.packages("cluster")
library(cluster)
install.packages("dendextend")
library(dendextend)

install.packages("graphics")
library(graphics)
install.packages("utils")
library(utils)

dist(x=dane2, method = "euclidean")^2
dist(x=dane2, method = "euclidean")
dist(x=dane2, method = "manhattan")

dis <- dist(x=dane2, method = "manhattan")
x <- as.matrix(dis) [1:16, 1:16]
x
round(x, digits = 3)            


a_dend <- hclust(dist(x = dane2, method = "manhattan"), method = "ward.D")
a_dend$height
plot(a_dend, hang = -1)
plot(a_dend)

dis <- dist(x=dane2, method = "manhattan")
a_dend <- hclust(dis, method = "ward.D")
b_dend <- hclust(dis, method = "single")
c_dend <- hclust(dis, method = "complete")
d_dend <- hclust(dis, method = "average")

par(mfrow=c(2,2))
plot(a_dend, hang=-1)
plot(b_dend, hang=-1)
plot(d_dend, hang=-1)
plot(c_dend, hang=-1)




par(mfrow=c(1,1))

################################################################################

#############################################################

install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("EnvStats")
library(EnvStats)

a_dend
a_dend$height
h <- c(0, a_dend$height)
a = 0.7
Mojena <- mean(h) + a * sd(h)
Mojena

plot(a_dend, hang = -1)
abline(h=Mojena, col = "red")

####################### PODZIAŁ NA 3 ########################################### 

grupy <- cutree(a_dend, k=3)
grupy

rect.hclust(a_dend, k=3, border="green")

dane2$grupy = grupy
grupy


profile = dane2 %>%
  group_by(grupy) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
profile
View(profile)

profile_2 = t(profile)
View(profile_2)
colnames(profile_2) = c("cluster_1", "cluster_2", "cluster_3")

profile_2 = profile_2[-1,]
l = nrow(profile_2)  
profile <- round(profile_2, digits = 3) 

profile_2 <- as.data.frame(profile_2)  
dt2 = as.data.frame(list(average_value = c(profile_2$cluster_1, profile_2$cluster_2, profile_2$cluster_3),
                         grupy = c(rep("CLUSTER 1", l), rep("CLUSTER 2", l), rep("CLUSTER 3", l)),
                         variables = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15")))
dt2 = as.data.frame(list(average_value = c(profile_2$cluster_1, profile_2$cluster_2, profile_2$cluster_3),
                         grupy = c(rep("CLUSTER 1", l), rep("CLUSTER 2", l), rep("CLUSTER 3", l)),
                         variables = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15")))
df2 = dt2

ggplot(data = dt2, aes(x = variables, y = average_value, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Accent") + 
  theme_minimal()

ggplot(data = df2, aes(x = variables, y = average_value, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Set1") + 
  theme_minimal()

ggplot(data = df2, aes(x = variables, y = average_value, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Set1") + 
  theme_test()

ggplot(data = df2, aes(x = variables, y = average_value, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Set1") + 
  theme_classic()

ggplot(data = df2, aes(x = variables, y = average_value, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Set1") + 
  theme_light()

ggplot(data = df2, aes(x = variables, y = average_value, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Set1") + 
  theme_grey() + theme (legend.position = "top",
                        panel.grid.major.x = element_line (colour = "black" , linetype = "dashed" ) ,
                        panel.grid.major.y = element_blank(),
                        panel.grid.minor = element_blank() )

################################################################################

####################### PODZIAŁ NA 4 ########################################### 

plot(a_dend, hang = -1)
grupy <- cutree(a_dend, k=4)
grupy
rect.hclust(a_dend, k=4, border="green")

dane2$grupy = grupy
grupy

profile = dane2 %>%
  group_by(grupy) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
profile
View(profile)

profile_2 = t(profile)
View(profile_2)
colnames(profile_2) = c("cluster_1", "cluster_2", "cluster_3", "cluster_4")

profile_2 = profile_2[-1,]
l = nrow(profile_2)  
profile <- round(profile_2, digits = 3) 

profile_2 <- as.data.frame(profile_2)  
dt2 = as.data.frame(list(średnia_wartość = c(profile_2$skupienie_1, profile_2$skupienie_2, profile_2$skupienie_3, profile_2$skupienie_4),
                         grupy = c(rep("CLUSTER 1", l), rep("CLUSTER 2", l), rep("CLUSTER 3", l), rep("CLUSTER 4", l)),
                         zmienne = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15")))
dt2 = as.data.frame(list(średnia_wartość = c(profile_2$skupienie_1, profile_2$skupienie_2, profile_2$skupienie_3, profile_2$skupienie_4),
                         grupy = c(rep("CLUSTER 1", l), rep("CLUSTER 2", l), rep("CLUSTER 3", l), rep("CLUSTER 4", l)),
                         zmienne = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15")))
df2 = dt2

ggplot(data = dt2, aes(x = zmienne, y = średnia_wartość, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Accent") + 
  theme_minimal()

ggplot(data = df2, aes(x = zmienne, y = średnia_wartość, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Set1") + 
  theme_minimal()

ggplot(data = df2, aes(x = zmienne, y = średnia_wartość, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Set1") + 
  theme_test()

ggplot(data = df2, aes(x = zmienne, y = średnia_wartość, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Set1") + 
  theme_classic()

ggplot(data = df2, aes(x = zmienne, y = średnia_wartość, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Set1") + 
  theme_light()

ggplot(data = df2, aes(x = zmienne, y = średnia_wartość, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Set1") + 
  theme_grey() + theme (legend.position = "top",
                        panel.grid.major.x = element_line (colour = "black" , linetype = "dashed" ) ,
                        panel.grid.major.y = element_blank(),
                        panel.grid.minor = element_blank() )

################################################################################

require(factoextra)
require(vegan)

fviz_dend(x = a_dend, cex = 0.7, lwd = 0.7)

######################### PALETY BARW ##########################################

require(grDevices)
require(ggthemes)
colors()
require(scales)
palette()
show_col(palette())

show_col(hue_pal()(9))
show_col(hue_pal()(9), borders = NA)
show_col(viridis_pal()(16))   
show_col(viridis_pal()(16), labels = FALSE)

require(ggsci)
show_col(pal_jco(palette = c("default"))(10))
show_col(pal_jco("default", alpha = 0.6)(10))

##########################################################

fviz_dend(x=a_dend, cex = 0.8, lwd = 0.8, k = 3, k_colors = "jco")
fviz_dend(x=a_dend, cex = 0.8, lwd = 0.8, k = 3, k_colors = palette())

fviz_dend(a_dend, cex = 0.8, lwd = 0.8, k = 3, rect = TRUE, 
          rect_border = "gray", rect_fill = FALSE)
fviz_dend(a_dend, cex = 0.8, lwd = 0.8, k = 3, rect = TRUE, 
          rect_border = "gray", rect_fill = TRUE)
fviz_dend(a_dend, cex = 0.8, lwd = 0.8, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE)
fviz_dend(a_dend, cex = 0.8, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE, horiz = TRUE)
fviz_dend(a_dend, cex = 0.8, lwd = 0.8, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE, ggtheme = theme_void())

################################################################################

install.packages("igraph")
library(igraph)


fviz_dend(a_dend, cex = 1.2, lwd = 2, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE, type = "phylogenic")

fviz_dend(a_dend, cex = 1.2, lwd = 2, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE, type = "rectangle")

fviz_dend(a_dend, cex = 1.2, lwd = 2, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE, type = "circular")

fviz_dend(a_dend, cex = 1.2, lwd = 2, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE, type = "phylogenic", 
          repel = TRUE, phylo_layout = "layout_as_tree")

################################################################################














