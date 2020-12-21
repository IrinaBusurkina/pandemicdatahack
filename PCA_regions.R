library(foreign)
library(dplyr)
library(lubridate)
library(tidyverse)

install.packages("ggrepel")
library(ggrepel)

okruga <- read.csv("/Users/irinabusurkina/Downloads/unemployed_1/okruga.csv")

#more about PCA in R using FactoMineR
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7851&fbclid=IwAR01E5XVvCrSKnpkCdAppbpvv7YMGvxSWaSSwb4SIgrXjrxoIpMIlNblYFY
#https://www.kaggle.com/agailloty/comprehensive-pca-with-r-using-factominer

library(FactoMineR)
region_names = as.data.frame(regions[,1])
regions$regions = rownames(regions)
res = PCA(regions, scale.unit = TRUE, quali.sup = c(1), ncp = 5, graph = T)

summary(res)

res$eig

help(plot.PCA)

df = data.frame(res$eig)
plot(df$eigenvalue, type = 'h')


plot.PCA(res, axes=c(1, 2), choix="var", habillage=2)

res$var$contrib

res.hcpc<-HCPC(res ,nb.clust=4,consol=TRUE,min=4,max=10,graph=TRUE)

res.hcpc$desc.var

cluster = data.frame(res.hcpc$data.clust)
cluster1 = merge(x = cluster, y = okruga, by = "region", all.x = TRUE)

head(cluster)

df = data.frame(res.hcpc$call$X)
head(df)
df$regions = rownames(df)
df1 = merge(x = df, y = regions, by = "regions", all.x = TRUE)
df1$region = str_remove(df1$region, "область")
df1$region = str_remove(df1$region, "Республика")

library(ggplot2)
ggplot(df1, aes(Dim.1, Dim.2)) + 
  geom_text_repel(aes(label = region)) +
  geom_point(aes(col = clust)) +
  #geom_text(aes(label = region), hjust = 0, vjust = 0) +
  #scale_y_log10() +
  facet_wrap(~clust) +
  #scale_x_log10() + 
  theme_bw()


write.csv(cluster1, "cluster1.csv")
ggsave(file="pca.png",width = 35, height = 25,units="cm")

