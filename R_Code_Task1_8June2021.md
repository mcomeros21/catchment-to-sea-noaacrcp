---
title: "Code  to run nutrient, sediment analyses Task 1: NOAA CRCP"
author: "Mia Comeros"
date: "8/June/2021"
output:
   github_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

#read in library

tinytex::install_tinytex()


```{r}
library(vegan)
library(readxl)
library(dplyr)
library(lattice)
library(psych)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggstatsplot)
library(Rmisc)

```


#Read in nutrient data
```{r Water Quality - Nutrients}

wq <- read_excel("C:/Users/jc445340/Dropbox/James Cook University/Data/Chapter1_WaterQuality/WQ_4Aug20.xlsx",
    sheet = "Nutrients_2")
wq.dat <- as.data.frame(wq)

View(wq.dat)

wq.dat$Location <- factor(wq.dat$`Location`,levels=c("Stream", "Reef Flat"))
wq.dat$`Watershed Type` <- factor(wq.dat$`Watershed Type`, levels=c("Pristine", "Intermediate", "Disturbed"))
wq.dat$`Sampling Season` <- factor(wq.dat$`Sampling Season`, levels = c("Aug-18_Dry", "Nov-18_Wet", "Feb-19_Wet","May-19_Dry"))
wq.dat$Site <-factor(wq.dat$Site, levels = c("Fagatele", "Tafeu", "Fagasa", "Vatia", "Nu'uuli", "Fagaalu"))

```

#plots of nutrients - spatial patterns 
```{r Nutrients boxplot spatial patterns}

##

library(ggplot2)

ggplot(wq.dat) +
 aes(x = Site, y = Silicate, fill = `Watershed Type`) +
 geom_boxplot() +
 scale_fill_hue() +
 labs(y = "Silicate µmol/L") +
 theme_minimal() +
 facet_wrap(vars(Location)) +  facet_wrap(vars(Location)) + theme_minimal() + scale_fill_manual(values = c("#9999CC", "#009E73", "#E69F00")) + theme_bw()  + theme(axis.text.x=element_text(angle=45, hjust=1)) 


```


#plot nutrients - seasonal patterns 
```{r Nutrients - Seasons}

wq.dat %>%
 filter(Location %in% "Reef Flat") %>% ggplot( aes(x= `Sampling Season`, y=Ammonium,  color =`Watershed Type`)) +
 geom_point(size=3, aes(shape = `Site Location`, color=`Watershed Type`))  +  scale_shape_manual(values=c(3, 15, 17, 7,8,9, 12, 5)) + labs(y = "Ammonium Reef Flat")  + facet_wrap(vars(`Site`)) +
 theme_bw()  + theme(axis.text.x=element_text(angle=45, hjust=1)) + scale_colour_manual(values = c("#9999CC", "#009E73", "#E69F00"))

```

#filter by location (stream and reef flat)
```{r}

wq.flat <- wq.dat %>% filter(wq$Location=="Reef Flat")

wq.stream <- wq.dat %>% filter(wq$Location=="Stream")
View(wq.stream)

```

#Explore Nutrient water quality data to see distribution and spread of data
```{r}

library(lattice)
library(psych)

wq.dat1<- wq.dat[c(7:15)]

View(wq.dat)



pairs.panels(wq.dat1, panel=panel.smooth,
        main="Bivariate plots with histograms and smooth curves")


```
##filter by sampling location
```{r}


wq.flat1 <- wq.dat1 %>% filter(wq$Location=="Reef Flat")

wq.stream1<- wq.dat1 %>% filter(wq$Location=="Stream")


##Check distribution of variables 
pairs.panels(wq.flat1, panel=panel.smooth,
        main="Bivariate plots with histograms and smooth curves")

pairs.panels(wq.stream1, panel=panel.smooth,
        main="Bivariate plots with histograms and smooth curves")

```


#data transformation and standardization - STREAM
```{r}

stream.log<-log(wq.stream1)

stream.sqrt<-sqrt(wq.stream1)

View(stream.sqrt)


stream.sqrt1<-(stream.sqrt)[,-4][,-6] #remove highly correlated variables TP and Nitrate as indicated in bivariate plots above chunk

View(stream.sqrt1)

#Standarization of nutrient variables

#center and scale = standardize the variables (z-scores)

stream.z<-decostand(stream.sqrt1, "standardize")

apply(stream.z, 2, mean) #means = 0
apply(stream.z, 2, sd) #standard deviations = 1

#Same standarsdization using the scale() function (which returns a matrix)
stream.z<-as.data.frame(scale(stream.sqrt1))

pairs.panels(stream.z, panel=panel.smooth,
        main="Bivariate plots with histograms and smooth curves")


```

##Principal component analysis of nutrient data - STREAM
```{r}
stream.pca<-rda(stream.z, scale=TRUE) #correlation matrix not covariance###
stream.pca
summary(stream.pca)
summary(stream.pca, scaling=1)

#eigenvalues
(stream.eig<-stream.pca$CA$eig)

screeplot(stream.pca, bstick=TRUE, npcs=length(stream.pca$CA$eig))

```
#Plot 1 of PCA Nutrient Stream Data
```{r}
#plots using biplot.rda

par(mfrow=c(1,2))
biplot(stream.pca, scaling=2, main="PCA - scaling 1")
biplot(stream.pca, main="PCA - scaling 2")



```
# continue to build plot to make plot presentable
```{r}
#extract scores (x and y coordinates)
data.scores = as.data.frame(scores(stream.pca, display = "sites"))

#add factors back into data frame 
data.scores$Season = wq.stream$Season
data.scores$Site = wq.stream$`Site Location`
data.scores$Transect = wq.stream$Transect
data.scores$Site_Transect = wq.stream$Site_Transect
data.scores$Location = wq.stream$Location
data.scores$Reef.Type = wq.stream$Reef.Type
data.scores$Watershed_severity = wq.stream$`Watershed Type`
data.scores$Sampling.Month = wq.stream$Sampling_Month

head(data.scores)
```
##continue to build up plot PCA Nutrients - stream
```{r}

library(ggplot2)

xx = ggplot(data.scores, aes(x = PC1, y = PC2)) + 
    geom_point(size = 9, aes( shape = Site, colour = Watershed_severity))+ 
    theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
    axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
    legend.text = element_text(size = 12, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
    legend.title = element_text(size = 14, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank()) + 
    labs(x = "PC1", colour = "Location", y = "PC2", shape = "Site")  + 
    scale_colour_manual(values = c("red", "black", "blue")) 
 
xx

```
#Continue to build up plot
```{r}
en = envfit(stream.pca, stream.z, permutations = 1000, na.rm = TRUE)
en_coord_cont = as.data.frame(scores(en, "vectors"))

#####################################################

#reduce vectors to those where p<0.05
A <- as.list(en$vectors) #creating the dataframe
pvals<-as.data.frame(A$pvals)
C<-cbind(en_coord_cont, pvals)
Cred<-subset(C,pvals<0.05)#subset to p<0.05 - CHANGE IF NEEDED
reduced.coord.cont <- cbind(Cred, Variables = rownames(Cred))


library(ggrepel) # to stop labels overlapping cange geom_text in the 4th line to geom_text_repel.

#e.g overlapping names

xx = ggplot(data.scores, aes(x = PC1, y = PC2)) + 
    geom_point(size = 7, aes( shape = Site, colour = Watershed_severity))+ 
    geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), data = reduced.coord.cont, size =0.1, alpha = 0.5, colour = "grey30")+
    geom_text(data = reduced.coord.cont, aes(x = PC1, y = PC2), colour = "grey30", fontface = "bold", label = row.names(reduced.coord.cont)) + 
    theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
    legend.text = element_text(size = 12, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
    legend.title = element_text(size = 14, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank())+ 
    labs(x = "PC1", colour = "Location", y = "PC2", shape = "Site")  + 
   scale_colour_manual(values = c("red", "black", "blue")) 

xx


##

xx = ggplot(data.scores, aes(x = PC1, y = PC2)) + 
    geom_point(size = 9, aes( shape = Site, colour = Watershed_severity))+ 
    theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
    axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
    legend.text = element_text(size = 12, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
    legend.title = element_text(size = 14, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank()) + 
    labs(x = "PC1", colour = "Location", y = "PC2", shape = "Site")  + 
    scale_colour_manual(values = c("red", "black", "blue")) 
 




##

#non-overlapping - but less lined up with vectors
 
wq = ggplot(data.scores, aes(x = PC1, y = PC2)) + 
    geom_point(size = 4, aes( shape =interaction(Site, Watershed_severity), colour= interaction(Site, Watershed_severity))) + scale_shape_manual(values=c(2,3,7)) + 
    geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), data = reduced.coord.cont, size =0.1, alpha = 0.5, colour = "grey30") +
    geom_text_repel(data = reduced.coord.cont, aes(x = PC1, y = PC2), colour = "grey30", fontface = "bold", label = row.names(reduced.coord.cont)) + 
    theme(axis.text.y = element_text(colour = "black", size = 10, face = "bold"), axis.text.x = element_text(colour = "black", face = "bold", size = 10), 
    legend.text = element_text(size = 10, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
    legend.title = element_text(size = 12, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank()) + 
    labs(x = "PC1", colour = "Watershed type", y = "PC2", group= "Site")


wq2 <-wq + scale_y_continuous(paste(names(stream.eig[2]),
                                  sprintf('(%0.1f%% explained var.)', 100* stream.eig[2]/sum(stream.eig))))+
  scale_x_continuous(paste(names(stream.eig[1]),
                           sprintf('(%0.1f%% explained var.)', 100* stream.eig[1]/sum(stream.eig)))) + ggtitle("Nutrients - Stream") +  scale_shape_manual("", values=c(0,15,2,17,5,18,1,16)) + scale_color_manual("", values=c("#9999CC","#9999CC", "#009E73","#009E73","#009E73","#009E73","#E69F00","#E69F00")) + scale_fill_brewer("", ) + labs(color  = "Site", linetype = "Site", shape = "Site")


wq2

##


```
#Nutrients -- Reef Flat
```{r - Reef Flat Nutrients transformation and standardization}

flat.log<-log(wq.flat1)#use log transform reef flat 

View(flat.log)

flat.sqrt<-sqrt(wq.flat1)

View(flat.sqrt)

flat.log<-log(wq.flat1)[,-9]

d<-sqrt(wq.flat1$Delta15N)

Delta15N<-wq.flat1$`Delta15N`

```


```{r - Reef Flat Nutrients }
flat.log1<-cbind(flat.log, Delta15N)   

flat.log2<-flat.log1[,-4][,-6] #remove highly correlated Nitrate and Total P #use this for reef flat 11 Sep 20

flat.sqrt1<-flat.sqrt[,-4][,-6] #remove highly correlated Nitrate and Total P 


#Standarization of nutrient variables

#center and scale = standardize the variables (z-scores)

flat.x<-decostand(flat.log2, "standardize")

apply(flat.x, 2, mean) #means = 0
apply(flat.x, 2, sd) #standard deviations = 1


###

pairs.panels(flat.x, panel=panel.smooth,
        main="Bivariate plots with histograms and smooth curves")


#Same standarsdization using the scale() function (which returns a matrix)
flat.z<-as.data.frame(scale(flat.log))



##

```

#PCA nutrient data Reef Flat
```{r}
flat.pca<-rda(flat.x, scale=TRUE)
flat.pca
summary(flat.pca)
summary(flat.pca, scaling=1)

#eigenvalues
(flat.eig<-flat.pca$CA$eig)

screeplot(flat.pca, bstick=TRUE, npcs=length(flat.pca$CA$eig))

```
#Plot nutrients -Reef Flat pca
```{r}
#plots using biplot.rda

par(mfrow=c(1,2))
biplot(flat.pca, scaling=1, main="PCA - scaling 1")
biplot(flat.pca, main="PCA - scaling 2")

#Plots using cleanplot.pca
library(vegan)


```
#make plots presentable - Nutrients Reef Flat
```{r}
#extract NMDS scores (x and y coordinates)
data.scores2 = as.data.frame(scores(flat.pca, display = "sites"))

#add factors back into data frame 
data.scores2$Season = wq.flat$Season
data.scores2$Site = wq.flat$`Site Location`
data.scores2$Transect = wq.flat$Transect
data.scores2$Site_Transect = wq.flat$Site_Transect
data.scores2$Location = wq.flat$Location
data.scores2$Reef.Type = wq.flat$Reef.Type
data.scores2$Watershed.severity = wq.flat$`Watershed Type`
data.scores2$Sampling.Month = wq.flat$Sampling_Month


 
head(data.scores2)



```
#ggplot Nutrients Reef Flat
```{r}

library(ggplot2)

flat = ggplot(data.scores2, aes(x = PC1, y = PC2)) + 
    geom_point(size = 9, aes( shape = Site, colour = Watershed.severity))+ 
    theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
    axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
    legend.text = element_text(size = 12, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
    legend.title = element_text(size = 14, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank()) + 
    labs(x = "PC1", colour = "Location", y = "PC2", shape = "Site")  + 
    scale_colour_manual(values = c("red", "black", "blue")) 
 
flat
```

#plot more - Nutrients reef flat
```{r}
en = envfit(flat.pca, flat.x, permutations = 1000, na.rm = TRUE)
en_coord_cont = as.data.frame(scores(en, "vectors"))

#####################################################

#reduce vectors to those where p<0.05
A <- as.list(en$vectors) #creating the dataframe
pvals<-as.data.frame(A$pvals)
C<-cbind(en_coord_cont, pvals)
Cred<-subset(C,pvals<0.05)#subset to p<0.05 - CHANGE IF NEEDED
reduced.coord.cont <- cbind(Cred, Variables = rownames(Cred))


library(ggrepel) # to stop labels overlapping cange geom_text in the 4th line to geom_text_repel.

#e.g overlapping names

xx = ggplot(data.scores2, aes(x = PC1, y = PC2)) + 
    geom_point(size = 7, aes(shape = Site, colour = Watershed.severity))+ 
    geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), data = reduced.coord.cont, size =0.1, alpha = 0.5, colour = "grey30")+
    geom_text(data = reduced.coord.cont, aes(x = PC1, y = PC2), colour = "grey30", fontface = "bold", label = row.names(reduced.coord.cont)) + 
    theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
    legend.text = element_text(size = 12, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
    legend.title = element_text(size = 14, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank())+ 
    labs(x = "PC1", colour = "Location", y = "PC2", shape = "Site")  + 
   scale_colour_manual(values = c("red", "black", "blue")) 

xx


##

xx = ggplot(data.scores2, aes(x = PC1, y = PC2)) + 
    geom_point(size = 9, aes( shape = Site, colour = Watershed.severity))+ 
    theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
    axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
    legend.text = element_text(size = 12, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
    legend.title = element_text(size = 14, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank()) + 
    labs(x = "PC1", colour = "Location", y = "PC2", shape = "Site")  + 
    scale_colour_manual(values = c("red", "black", "blue")) 
 




##

#non-overlapping - but less lined up with vectors
 
nut.flat = ggplot(data.scores2, aes(x = PC1, y = PC2)) + 
    geom_point(size = 4, aes( shape =interaction(Site, Watershed.severity), colour= interaction(Site, Watershed.severity))) + scale_shape_manual(values=c(2,3,7)) + 
    geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), data = reduced.coord.cont, size =0.1, alpha = 0.5, colour = "grey30") +
    geom_text_repel(data = reduced.coord.cont, aes(x = PC1, y = PC2), colour = "grey30", fontface = "bold", label = row.names(reduced.coord.cont)) + 
    theme(axis.text.y = element_text(colour = "black", size = 10, face = "bold"), axis.text.x = element_text(colour = "black", face = "bold", size = 10), 
    legend.text = element_text(size = 10, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 12), 
    axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
    legend.title = element_text(size = 12, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank()) + 
    labs(x = "PC1", colour = "Watershed Type", y = "PC2", shape = "Site")  + 
   scale_colour_manual(values = c("#9999CC", "#009E73", "#E69F00"))


nut.flat + scale_y_continuous(paste(names(flat.eig[2]),
                                  sprintf('(%0.1f%% explained var.)', 100* flat.eig[2]/sum(flat.eig))))+
  scale_x_continuous(paste(names(flat.eig[1]),
                           sprintf('(%0.1f%% explained var.)', 100* flat.eig[1]/sum(flat.eig)))) + ggtitle("Nutrients - Reef Flat") + scale_shape_manual("", values=c(0,15,2,17,5,18,1,16)) + scale_color_manual("", values=c("#9999CC","#9999CC", "#009E73","#009E73","#009E73","#009E73","#E69F00","#E69F00"))  + labs(color  = "Site", linetype = "Site", shape = "Site")


###




```

#Sediments####
```{r Sediments}

sed <- read_excel("C:/Users/jc445340/Dropbox/James Cook University/Data/Chapter1_WaterQuality/WQ_4Aug20.xlsx",
    sheet = "Sediments_3")
sed.dat <- as.data.frame(sed)


View(sed.dat)

sed.dat$Watershed.Type <- factor(sed.dat$`Watershed.Type`, levels=c("Pristine", "Intermediate", "Disturbed"))
sed.dat$Site <- factor(sed.dat$`Site`, levels=c("Fagatele", "Tafeu", "Vatia", "Fagasa", "Fagaalu Matafao", "Fagaalu Reef", "Nu'uuli"))
sed.dat$Sampling_Season<- factor(sed.dat$`Sampling_Season`, levels=c("Nov-18_Wet", "Feb-19_Wet", "May-19_Dry", "Sep-19_Dry"))
sed.dat$Deployment<- factor(sed.dat$`Deployment`, levels=c("Nov-18_Feb-19_Wet", "Feb-19_May-19_Wet", "May-19_Sept-19_Dry", "Sep-19_Nov-19_Dry"))
sed.dat$Deployment_2<- factor(sed.dat$`Deployment_2`, levels=c("D1", "D2", "D3", "D4"))



```



#ggplot stacked bar sediments
```{r Stacked sediments}

View(sed.dat)


stacked<-sed.dat[c(7,11:13)] 


View(stacked)

sed.dat$Watershed.Type <- as.factor(sed.dat$`Watershed.Type`)


#gather function####

library(dplyr)
library(tidyverse)
library(Rmisc)
#gather geochem composition into one column
geochem<-sed.dat %>% 
  gather(Composition, `accumulation.rate`, c(11:13))

View(geochem)


geochem$Composition <- factor(geochem$`Composition`, levels=c("Carbonate", "Organic", "Mineral"))
geochem$Watershed.Type <- factor(geochem$`Watershed.Type`, levels=c("Pristine", "Intermediate", "Disturbed"))
geochem$Site <- factor(geochem$`Site`, levels=c("Fagatele", "Tafeu", "Vatia", "Fagasa", "Fagaalu Matafao", "Fagaalu Reef", "Nu'uuli"))


####

##Plot sediment accumulation rate by deployment####

nov20 <- geochem %>% filter(Deployment_2 %in% 
    "D1") %>% filter(Watershed.Type %in% "Intermediate") %>%
  ggplot( aes(x = Site, fill = Composition, weight = `accumulation.rate`))  + geom_bar()  +  labs(y = "Trap collection rate (mg.cm-2.d-1)") + facet_grid(~Deployment_2 ~ Watershed.Type, scales = "fixed", space = "fixed") +  theme_bw() +  theme(axis.text.x=element_text(angle=50, hjust=1, vjust=1)) + theme(axis.text=element_text(size=14),
        axis.title=element_text(size=12,face="bold")) + theme(axis.text.x = element_text(face="bold", color="black", size=14, angle=45), axis.text.y = element_text(face="bold", color="black",  size=14, angle=45)) + theme(text = element_text(size=rel(4)),
          strip.text.x = element_text(size=rel(4)),
          strip.text.y = element_text(size=rel(5))) + theme(legend.text = element_text(size = 10, face ="bold", colour ="black")) + scale_fill_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) 

nov20 #

```
####Plot just mineral and organic composition####
```{r just mineral and organic composition trap accumulation}


library(dplyr)
library(ggplot2)

m <-geochem %>%
 filter(!(Composition %in% "Carbonate")) %>% filter(Watershed.Type %in% 
    "Disturbed") %>% filter(Deployment_2 %in% 
    "D4") %>%
 ggplot() +
 aes(x = Site, fill = Composition, weight = `accumulation.rate`) +
 geom_bar() +
scale_fill_brewer(palette = "Dark2") + facet_grid(~Deployment_2 ~ Watershed.Type) +  theme_ggstatsplot() + 
 theme(axis.text.x=element_text(angle=50, hjust=1, vjust=1)) + theme(axis.text=element_text(size=14),
        axis.title=element_text(size=12,face="bold")) + theme(axis.text.x = element_text(face="bold", color="black", size=14, angle=45), axis.text.y = element_text(color="black",  size=18, angle=0)) + theme(text = element_text(size=rel(4)),
          strip.text.x = element_text(size=rel(4)),
          strip.text.y = element_text(size=rel(4))) + theme(legend.text = element_text(size = 14, face ="bold", colour ="black")) + scale_y_continuous(breaks = seq(0, 4, by=1), limits=c(0,4))


m

```


```{r read data in - sediment accumulation 28 October 2020}

sed2 <- read_excel("C:/Users/jc445340/Dropbox/James Cook University/Data/Chapter1_WaterQuality/WQ_28October20.xlsx",
    sheet = "Sediments_4") #trap accumulation rate only

sed.dat2 <- as.data.frame(sed2)

sed.dat2$Deployment_2 <- factor(sed.dat2$`Deployment_2`, levels=c("D1", "D2", "D3", "D4"))
sed.dat2$Watershed.Type <- factor(sed.dat2$`Watershed.Type`, levels=c("Pristine", "Intermediate", "Disturbed"))



View(sed.dat2)




```



#Add summary stats 
```{r add SE and SD trap accumulation}

sum  <- summarySE(sed.dat2, measurevar="trap.accumulation.rate", groupvars=c("Deployment_2", "Site", "Watershed.Type"))

sum

View(sum)

#

sum$Site <- factor(sum$`Site`, levels=c("Fagatele", "Tafeu", "Vatia", "Fagasa", "Fagaalu Matafao", "Fagaalu Reef", "Nu'uuli"))
sum$Deployment_2<- factor(sum$`Deployment_2`, levels=c("D1", "D2", "D3", "D4"))

```

```{r Stacked bar total trap accumulation rate with standard error  }

#Merge datasets: geochem and sum

total2 <- merge(geochem,sum,by=c("Deployment_2", "Site"))


View(total2)

total2$Composition <- factor(total2$`Composition`, levels=c("Carbonate", "Organic", "Mineral"))


###Plot geochem composition with standard error ####

total2 %>%  filter(Deployment_2 %in% "D1") %>% filter(Watershed.Type.x %in% "Intermediate") %>%
ggplot() +
 aes(x = Site, fill = Composition, weight = `accumulation.rate`) +
 geom_bar() +
scale_fill_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2") +  theme_ggstatsplot() + facet_grid(~Deployment_2 ~Watershed.Type.x) +
 theme(axis.text.x=element_text(angle=50, hjust=1, vjust=1)) + theme(axis.text=element_text(size=14),
        axis.title=element_text(size=12,face="bold")) + theme(axis.text.x = element_text(face="bold", color="black", size=14, angle=45), axis.text.y = element_text(color="black",  size=18, angle=0)) + theme(text = element_text(size=rel(4)),
          strip.text.x = element_text(size=rel(4)),
          strip.text.y = element_text(size=rel(4))) + theme(legend.text = element_text(size = 14, face ="bold", colour ="black")) + geom_errorbar(aes(ymin = trap.accumulation.rate-se, ymax = trap.accumulation.rate+se), width = 0.5, position="identity") + scale_y_continuous(breaks = seq(0, 20, by=5), limits=c(0,20)) + scale_fill_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) 



```

###Plot total trap accumulation 
```{r Total trap accumulation - standard Error }

mm <-sum %>% 
 ggplot() +
 aes(x = Site, fill = Watershed.Type, weight = `trap.accumulation.rate`) +
 geom_bar() +  theme_ggstatsplot() +
 theme(axis.text.x=element_text(angle=50, hjust=1, vjust=1))  + scale_y_continuous(breaks = seq(0, 20, by=5), limits=c(0,20)) +  geom_errorbar(aes(ymin = trap.accumulation.rate-se, ymax = trap.accumulation.rate+se), width = 0.5, position="identity") + facet_grid(~Deployment_2) +  scale_fill_manual(values = c("#9999CC", "#009E73", "#E69F00"))

mm



```


###Plot particle size distribution
```{r read data in - particle size}

psa <- read_excel("C:/Users/jc445340/Dropbox/James Cook University/Data/Chapter1_WaterQuality/Analyses_26_28Oct2020/ParticleSize_Table_29Oct2020.xlsx",
    sheet = "Sheet5")


psa$Site <- factor(psa$`Site`, levels=c("Fagatele", "Tafeu", "Vatia", "Fagasa", "Fagaalu Matafao", "Fagaalu Reef", "Nu'uuli"))
psa$Watershed.Type <- factor(psa$`Watershed.Type`, levels=c("Pristine", "Intermediate", "Disturbed"))


psa

psa2  <- summarySE(psa, measurevar="D10", groupvars=c("Site", "Watershed.Type"))

psa2



psa2 %>% 
ggplot() +
 aes(x = Site, weight = `D10`) +
 geom_bar()  +  theme_ggstatsplot() + theme(axis.text.x=element_text(angle=50, hjust=1, vjust=1)) + geom_errorbar(aes(ymin = D10, ymax = D10+sd), width = 0.5, position="identity") + labs(y = "D90")




```
#Ready for Principal Component Analyses
#First, explore sediment data
```{r}

library(lattice)
library(psych)

View(sed.dat)

sed.dat1<- sed.dat[c(7:10, 16,18,19)]

View(sed.dat1)


pairs.panels(sed.dat1, panel=panel.smooth,
        main="Bivariate plots with histograms and smooth curves")


```
#transformation and standardization - Sediments
```{r Sediment data transformation and standardization}

View(sed.dat1)

sed.log<-log(sed.dat1)

View(sed.log)
sed.sqrt<-sqrt(sed.dat1) 

View(sed.sqrt)

sed.log1<-sed.log[,-3]

pairs.panels(sed.sqrt, panel=panel.smooth,
        main="Bivariate plots with histograms and smooth curves")


#center and scale = standardize the variables (z-scores)

d.z<-decostand(sed.log, "standardize")

apply(d.z, 2, mean) #means = 0
apply(d.z, 2, sd) #standard deviations = 1

#Same standarsdization using the scale() function (which returns a matrix)
d.z<-as.data.frame(scale(sed.log))

```
#pca sediment data
```{r}
d.pca<-rda(sed.sqrt, scale=TRUE)
d.pca
summary(d.pca)
summary(d.pca, scaling=1)

#eigenvalues
(d.eig<-d.pca$CA$eig)

screeplot(d.pca, bstick=TRUE, npcs=length(d.pca$CA$eig))
```
#make plots presentable
```{r}
#extract NMDS scores (x and y coordinates)
data.scores3 = as.data.frame(scores(d.pca, display = "sites"))

#add factors back into data frame 
data.scores3$Season = sed.dat$Season
data.scores3$Site = sed.dat$Site
data.scores3$Watershed.severity = sed.dat$Watershed.Type
data.scores3$Sampling.Month = sed.dat$Sampling_Month


 
head(data.scores3)
```
#ggplot Sediments
```{r}

library(ggplot2)

sed = ggplot(data.scores3, aes(x = PC1, y = PC2)) + 
    geom_point(size = 9, aes( shape = Site, colour = Watershed.severity))+ 
    theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
    axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
    legend.text = element_text(size = 12, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
    legend.title = element_text(size = 14, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank()) + 
    labs(x = "PC1", colour = "Location", y = "PC2", shape = "Watershed type")  + 
    scale_colour_manual(values = c("red", "black", "blue")) 
 
sed

```
##Continue to build Sediment plot
```{r}
en = envfit(d.pca, sed.sqrt, permutations = 1000, na.rm = TRUE)
en_coord_cont = as.data.frame(scores(en, "vectors"))

#####################################################

#reduce vectors to those where p<0.05
A <- as.list(en$vectors) #creating the dataframe
pvals<-as.data.frame(A$pvals)
C<-cbind(en_coord_cont, pvals)
Cred<-subset(C,pvals<0.05)#subset to p<0.05 - CHANGE IF NEEDED
reduced.coord.cont <- cbind(Cred, Variables = rownames(Cred))


library(ggrepel) # to stop labels overlapping cange geom_text in the 4th line to geom_text_repel.

#e.g overlapping names

xx = ggplot(data.scores3, aes(x = PC1, y = PC2)) + 
    geom_point(size = 7, aes( shape = Site, colour = Watershed.severity))+ 
    geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), data = reduced.coord.cont, size =0.1, alpha = 0.5, colour = "grey30")+
    geom_text(data = reduced.coord.cont, aes(x = PC1, y = PC2), colour = "grey30", fontface = "bold", label = row.names(reduced.coord.cont)) + 
    theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
    legend.text = element_text(size = 12, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
    legend.title = element_text(size = 14, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank())+ 
    labs(x = "PC1", colour = "Location", y = "PC2", shape = "Site")  + 
   scale_colour_manual(values = c("red", "black", "blue")) 

xx



##

xx = ggplot(data.scores3, aes(x = PC1, y = PC2)) + 
    geom_point(size = 9, aes( shape = Site, colour = Watershed.severity))+ 
    theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
    axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
    legend.text = element_text(size = 12, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
    legend.title = element_text(size = 14, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank()) + 
    labs(x = "PC1", colour = "Location", y = "PC2", shape = "Site")  + 
    scale_colour_manual(values = c("red", "black", "blue")) 
 




##

#non-overlapping - but less lined up with vectors
 
sed = ggplot(data.scores3, aes(x = PC1, y = PC2)) + 
    geom_point(size = 3, aes( shape =interaction(Site, Watershed.severity), colour= interaction(Site, Watershed.severity))) +  
    geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), data = reduced.coord.cont, size =0.1, alpha = 0.5, colour = "grey30") +geom_text_repel(data = reduced.coord.cont, aes(x = PC1, y = PC2), colour = "grey30", fontface = "bold", label = row.names(reduced.coord.cont)) + 
    theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
    legend.text = element_text(size = 10, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
    legend.title = element_text(size = 12, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank()) + 
    labs(x = "PC1", colour = "Watershed Type", y = "PC2", shape = "Site") 


sed + scale_y_continuous(paste(names(d.eig[2]),
                                  sprintf('(%0.1f%% explained var.)', 100* d.eig[2]/sum(d.eig))))+
  scale_x_continuous(paste(names(d.eig[1]),
                           sprintf('(%0.1f%% explained var.)', 100* d.eig[1]/sum(d.eig)))) + ggtitle("Sediments") + scale_shape_manual("", values=c(0,15,2,17,5,18,1,16)) + scale_color_manual("", values=c("#9999CC","#9999CC", "#009E73","#009E73","#E69F00","#E69F00", "#E69F00")) + scale_fill_brewer("", ) + labs(color  = "Site", linetype = "Site", shape = "Site")



```
