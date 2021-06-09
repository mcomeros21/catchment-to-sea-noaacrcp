Analyses (distribution \[boxplots\] and PCA of % cover benthic data -
Task 1
================
Mia Comeros
8/june/2021

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

\#read in libraries

``` r
library(vegan)
```

    ## Loading required package: permute

    ## Loading required package: lattice

    ## This is vegan 2.5-6

``` r
library(readxl)
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.0.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lattice)
library(psych)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v stringr 1.4.0
    ## v tidyr   1.1.1     v forcats 0.5.0
    ## v readr   1.3.1

    ## -- Conflicts ------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x ggplot2::%+%()   masks psych::%+%()
    ## x ggplot2::alpha() masks psych::alpha()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()

``` r
library(ggstatsplot)
```

    ## Registered S3 method overwritten by 'broom.mixed':
    ##   method      from 
    ##   tidy.gamlss broom

    ## Registered S3 methods overwritten by 'lme4':
    ##   method                          from
    ##   cooks.distance.influence.merMod car 
    ##   influence.merMod                car 
    ##   dfbeta.influence.merMod         car 
    ##   dfbetas.influence.merMod        car

    ## In case you would like cite this package, cite it as:
    ##      Patil, I. (2018). ggstatsplot: "ggplot2" Based Plots with Statistical Details. CRAN.
    ##      Retrieved from https://cran.r-project.org/web/packages/ggstatsplot/index.html

``` r
library(ggpubr)
```

\#\#Read in benthic data \#Benthos

``` r
#Read in data
benthos <- read_excel("C:/Users/jc445340/Dropbox/James Cook University/Data/Chapter1_WaterQuality/WQ_4Aug20.xlsx",
    sheet = "Benthos")
```

    ## New names:
    ## * `` -> ...1

``` r
#Make data frame
benthos.dat <- as.data.frame(benthos)

View(benthos.dat)

#Convert to factor
benthos.dat$Location <- as.factor(benthos.dat$Location)
benthos.dat$`Watershed.Type` <- factor(benthos.dat$`Watershed.Type`, levels=c("Pristine", "Intermediate", "Disturbed"))
benthos.dat$Site <-factor(benthos.dat$Site, levels = c("Fagatele", "Tafeu", "Fagasa", "Vatia", "Nu'uuli", "Fagaalu"))


##Plot 
#Rubble
ggplot(benthos.dat) +
 aes(x = Site, y = Rubble, fill = Watershed.Type) +
 geom_boxplot() +
 theme_minimal() + scale_fill_manual(values = c("#9999CC", "#009E73", "#E69F00")) + theme_bw() + facet_wrap(vars(Location)) + theme(axis.text.x=element_text(angle=45, hjust=1)) 
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Plot%20benthic%20distribution-1.png)<!-- -->

``` r
#Hard Coral
ggplot(benthos.dat) +
 aes(x = Site, y = HardCoral, fill = Watershed.Type) +
 geom_boxplot() +
 theme_minimal() + scale_fill_manual(values = c("#9999CC", "#009E73", "#E69F00")) + theme_bw() + facet_wrap(vars(Location)) + theme(axis.text.x=element_text(angle=45, hjust=1)) 
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Plot%20benthic%20distribution-2.png)<!-- -->

``` r
#CCA
ggplot(benthos.dat) +
 aes(x = Site, y = CCA, fill = Watershed.Type) +
 geom_boxplot() +
 theme_minimal() + scale_fill_manual(values = c("#9999CC", "#009E73", "#E69F00")) + theme_bw() + facet_wrap(vars(Location)) + theme(axis.text.x=element_text(angle=45, hjust=1)) 
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Plot%20benthic%20distribution-3.png)<!-- -->

``` r
#Macroalgae
ggplot(benthos.dat) +
 aes(x = Site, y = Macroalgae, fill = Watershed.Type) +
 geom_boxplot() +
 theme_minimal() + scale_fill_manual(values = c("#9999CC", "#009E73", "#E69F00")) + theme_bw() + facet_wrap(vars(Location)) + theme(axis.text.x=element_text(angle=45, hjust=1)) 
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Plot%20benthic%20distribution-4.png)<!-- -->

``` r
#Turf 
ggplot(benthos.dat) +
 aes(x = Site, y = Turf, fill = Watershed.Type) +
 geom_boxplot() +
 theme_minimal() + scale_fill_manual(values = c("#9999CC", "#009E73", "#E69F00")) + theme_bw() + facet_wrap(vars(Location)) + theme(axis.text.x=element_text(angle=45, hjust=1)) 
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Plot%20benthic%20distribution-5.png)<!-- -->

\#Benthos \#Principal Component Analysis

``` r
##
library(tibble)

benthos <- read_excel("C:/Users/jc445340/Dropbox/James Cook University/Data/Chapter1_WaterQuality/WQ_4Aug20.xlsx",
    sheet = "Benthos")
```

    ## New names:
    ## * `` -> ...1

``` r
#Make data frame
benthos.dat <- as.data.frame(benthos)

View(benthos.dat)

##Just the numerical variables (i.e., % cover)
df<-benthos.dat[c(2:6)]

View(df)

#Convert to factor
benthos.dat$Location <- as.factor(benthos.dat$Location)
benthos.dat$`Watershed.Type` <- factor(benthos.dat$`Watershed.Type`, levels=c("Pristine", "Intermediate", "Disturbed"))
```

``` r
benthos.flat <- benthos.dat %>% filter(benthos$Location=="Reef Flat")

b.flat<-benthos.flat[c(2:6)]

benthos.slope <- benthos.dat %>% filter(benthos$Location=="Reef Slope")

b.slope<-benthos.slope[c(2:6)]
```

\#Check distribution of the data

``` r
library(lattice)
library(psych)

benthos.dat1<- df

View(benthos.dat1)


pairs.panels(benthos.dat1, panel=panel.smooth,
        main="Bivariate plots with histograms and smooth curves")
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/Explore%20Benthos%20data-1.png)<!-- -->
\#Continue to explore distribution of data by location

``` r
#ReefFlat

benthos.flat1<- benthos.flat[c(2:6)]

#ReefSlope

benthos.slope1<- benthos.slope[c(2:6)]
```

\#Data transformation and standardization

``` r
benthos.sqrt<-sqrt(benthos.dat1)

benthos.log<-log(benthos.dat1)

#Check distribution

pairs.panels(benthos.sqrt,panel=panel.smooth,
        main="Bivariate plots with histograms and smooth curves")
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Data%20transformation-1.png)<!-- -->

``` r
#center and scale = standardize the variables (z-scores)####

benthos.z<-decostand(benthos.sqrt, "standardize")

benthos.x<-decostand(benthos.log, "standardize")
```

    ## Warning in decostand(benthos.log, "standardize"): result contains NaN, perhaps due to impossible mathematical operation

``` r
#Check distribution
ggplot(benthos.dat) +
 aes(x = Site, y = Rubble, fill = Watershed.Type) +
 geom_boxplot() +
 theme_minimal() + scale_fill_manual(values = c("#9999CC", "#009E73", "#E69F00")) + theme_bw() + facet_wrap(vars(Location)) + theme(axis.text.x=element_text(angle=45, hjust=1)) 
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Data%20transformation-2.png)<!-- -->

``` r
#Standardize data

apply(benthos.z, 2, mean) #means = 0
```

    ##           CCA     HardCoral    Macroalgae        Rubble          Turf 
    ## -2.966422e-17  4.184879e-17  1.040834e-17  3.035766e-17 -8.824728e-17

``` r
apply(benthos.z, 2, sd) #standard deviations = 1
```

    ##        CCA  HardCoral Macroalgae     Rubble       Turf 
    ##          1          1          1          1          1

``` r
#Same standarsdization using the scale() function (which returns a matrix)
benthos.z<-as.data.frame(scale(benthos.sqrt))
```

\#Shapiro-Test to check if data are normally distributed

``` r
shapiro.test(benthos.z$`HardCoral`)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.z$HardCoral
    ## W = 0.97038, p-value = 0.2621

``` r
shapiro.test(benthos.z$`Macroalgae`) #not normally distributed
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.z$Macroalgae
    ## W = 0.86669, p-value = 6.225e-05

``` r
shapiro.test(benthos.z$`Turf`)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.z$Turf
    ## W = 0.97142, p-value = 0.2875

``` r
shapiro.test(benthos.z$`CCA`) 
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.z$CCA
    ## W = 0.95317, p-value = 0.05356

``` r
shapiro.test(benthos.z$`Rubble`) #not normally distributed
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.z$Rubble
    ## W = 0.86161, p-value = 4.487e-05

\#standardize Benthos - by reef location

``` r
benthos.sqrt.flat<-sqrt(benthos.flat1)

View(benthos.sqrt.flat)


#center and scale = standardize the variables (z-scores)####

benthos.z.flat<-decostand(benthos.sqrt.flat, "standardize")

apply(benthos.z.flat, 2, mean) #means = 0
```

    ##           CCA     HardCoral    Macroalgae        Rubble          Turf 
    ##  1.448313e-17  7.575863e-17 -1.450120e-17 -7.658985e-17  7.112366e-17

``` r
apply(benthos.z.flat, 2, sd) #standard deviations = 1
```

    ##        CCA  HardCoral Macroalgae     Rubble       Turf 
    ##          1          1          1          1          1

``` r
#Same standarsdization using the scale() function (which returns a matrix)
benthos.z.flat<-as.data.frame(scale(benthos.sqrt.flat))


#
pairs.panels(benthos.sqrt.flat, panel=panel.smooth,
        main="Bivariate plots with histograms and smooth curves")
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/standardize%20Benthos%20reef%20flat,%20Reef%20slope-1.png)<!-- -->

``` r
#Check if data are normally distributed
shapiro.test(benthos.sqrt.flat$`HardCoral`)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.sqrt.flat$HardCoral
    ## W = 0.96493, p-value = 0.5451

``` r
shapiro.test(benthos.sqrt.flat$`Macroalgae`) #not normally distributed
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.sqrt.flat$Macroalgae
    ## W = 0.86608, p-value = 0.004412

``` r
shapiro.test(benthos.sqrt.flat$`Turf`)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.sqrt.flat$Turf
    ## W = 0.95762, p-value = 0.3926

``` r
shapiro.test(benthos.sqrt.flat$`CCA`) #not normally distributed
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.sqrt.flat$CCA
    ## W = 0.89697, p-value = 0.01858

``` r
shapiro.test(benthos.sqrt.flat$`Rubble`) #not normally distributed
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.sqrt.flat$Rubble
    ## W = 0.89076, p-value = 0.01378

``` r
#REEF SLOPE BENTHOS STANDARDIZE DATA####

benthos.sqrt.slope<-sqrt(benthos.slope1)

View(benthos.sqrt.slope)

benthos.log.slope<-log(b.slope)

#center and scale = standardize the variables (z-scores)####

benthos.z.slope<-decostand(benthos.sqrt.slope, "standardize")

apply(benthos.z.slope, 2, mean) #means = 0
```

    ##           CCA     HardCoral    Macroalgae        Rubble          Turf 
    ##  1.618623e-17 -2.388565e-16  6.071532e-18  4.378370e-17  4.701823e-17

``` r
apply(benthos.z.slope, 2, sd) #standard deviations = 1
```

    ##        CCA  HardCoral Macroalgae     Rubble       Turf 
    ##          1          1          1          1          1

``` r
#Same standarsdization using the scale() function (which returns a matrix)
benthos.z.slope<-as.data.frame(scale(benthos.sqrt.slope))

#
pairs.panels(benthos.sqrt.slope, panel=panel.smooth,
        main="Bivariate plots with histograms and smooth curves")
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/standardize%20Benthos%20reef%20flat,%20Reef%20slope-2.png)<!-- -->

``` r
#Check if data are normally distributed
shapiro.test(benthos.sqrt.slope$`HardCoral`)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.sqrt.slope$HardCoral
    ## W = 0.95391, p-value = 0.3285

``` r
shapiro.test(benthos.sqrt.slope$`Macroalgae`) #not normally distributed
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.sqrt.slope$Macroalgae
    ## W = 0.85258, p-value = 0.002442

``` r
shapiro.test(benthos.sqrt.slope$`Turf`)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.sqrt.slope$Turf
    ## W = 0.95175, p-value = 0.2955

``` r
shapiro.test(benthos.sqrt.slope$`CCA`) 
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.sqrt.slope$CCA
    ## W = 0.94551, p-value = 0.2162

``` r
shapiro.test(benthos.sqrt.slope$`Rubble`) #not normally distributed
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  benthos.sqrt.slope$Rubble
    ## W = 0.80735, p-value = 0.0003894

\#PCA benthos by location (Reef flat and Reef Slope)

\#Reef flat PCA - benthos

``` r
#Reef flat benthos PCA####
benthos.pca.flat<-rda(benthos.sqrt.flat, scale=TRUE)
benthos.pca.flat
```

    ## Call: rda(X = benthos.sqrt.flat, scale = TRUE)
    ## 
    ##               Inertia Rank
    ## Total               5     
    ## Unconstrained       5    5
    ## Inertia is correlations 
    ## 
    ## Eigenvalues for unconstrained axes:
    ##    PC1    PC2    PC3    PC4    PC5 
    ## 2.0563 1.6753 0.8331 0.2829 0.1525

``` r
summary(benthos.pca.flat)
```

    ## 
    ## Call:
    ## rda(X = benthos.sqrt.flat, scale = TRUE) 
    ## 
    ## Partitioning of correlations:
    ##               Inertia Proportion
    ## Total               5          1
    ## Unconstrained       5          1
    ## 
    ## Eigenvalues, and their contribution to the correlations 
    ## 
    ## Importance of components:
    ##                          PC1    PC2    PC3     PC4    PC5
    ## Eigenvalue            2.0563 1.6753 0.8331 0.28286 0.1525
    ## Proportion Explained  0.4113 0.3351 0.1666 0.05657 0.0305
    ## Cumulative Proportion 0.4113 0.7463 0.9129 0.96950 1.0000
    ## 
    ## Scaling 2 for species and site scores
    ## * Species are scaled proportional to eigenvalues
    ## * Sites are unscaled: weighted dispersion equal on all dimensions
    ## * General scaling constant of scores:  3.274722 
    ## 
    ## 
    ## Species scores
    ## 
    ##                PC1     PC2      PC3      PC4      PC5
    ## CCA         0.9757 -0.9037 -0.26516  0.54431 -0.09764
    ## HardCoral   0.7727 -0.3308  1.19137 -0.11240 -0.07868
    ## Macroalgae -1.0285  0.7490  0.49660  0.52463 -0.06342
    ## Rubble     -1.0518 -0.9286 -0.03271 -0.13785 -0.39506
    ## Turf        0.8349  1.1151 -0.22222 -0.05944 -0.38886
    ## 
    ## 
    ## Site scores (weighted sums of species scores)
    ## 
    ##            PC1      PC2     PC3      PC4      PC5
    ## sit1  -0.91514 -1.37085 -0.4708 -1.29690  0.31005
    ## sit2  -0.49929 -1.15222 -1.4132  0.22128  1.32776
    ## sit3   0.29847 -0.71943 -0.2144  0.65068 -1.02292
    ## sit4  -0.27235 -0.83864  0.2329  0.51255 -0.03835
    ## sit5   0.12739  0.59245  0.4639 -1.74188 -0.32523
    ## sit6   0.09845  0.72271 -0.8782 -0.01871 -0.70596
    ## sit7   0.16510  0.47043  0.2480 -0.38773 -0.59344
    ## sit8   0.07482  0.08783  0.3826  0.47936 -0.74085
    ## sit9   0.54437  0.63701 -1.1996 -0.64280  0.05929
    ## sit10  0.73758 -0.29278 -0.5179  0.31544 -0.79097
    ## sit11  0.75062  0.37943 -0.8265 -0.23786 -0.05653
    ## sit12  0.82265  0.19644 -0.5431 -0.08416  0.10475
    ## sit13 -0.89248 -0.19812  0.5012 -1.23557 -0.47819
    ## sit14 -0.03918  0.19912  0.6121  0.81829 -0.14825
    ## sit15 -0.23256 -0.38575  0.2775  0.61783 -1.02712
    ## sit16 -0.81992 -0.29118 -0.1656  0.71500 -0.22167
    ## sit17  0.90934 -0.83000  0.9120  0.27910  0.97419
    ## sit18  0.70059 -0.13964  0.1093  0.41934 -0.07533
    ## sit19  0.90494 -0.08472  0.5504 -0.49657  0.59997
    ## sit20  0.77872 -0.17498  1.0283 -0.11973  0.97710
    ## sit21 -0.77095  0.40573  1.0040 -0.13722 -0.09516
    ## sit22 -1.35946  0.25787  0.1137  0.24879 -0.03786
    ## sit23 -0.49867  1.13973  0.4206  0.52579  1.11260
    ## sit24 -0.61302  1.38957 -0.6271  0.59566  0.89211

``` r
summary(benthos.pca.flat, scaling=1)
```

    ## 
    ## Call:
    ## rda(X = benthos.sqrt.flat, scale = TRUE) 
    ## 
    ## Partitioning of correlations:
    ##               Inertia Proportion
    ## Total               5          1
    ## Unconstrained       5          1
    ## 
    ## Eigenvalues, and their contribution to the correlations 
    ## 
    ## Importance of components:
    ##                          PC1    PC2    PC3     PC4    PC5
    ## Eigenvalue            2.0563 1.6753 0.8331 0.28286 0.1525
    ## Proportion Explained  0.4113 0.3351 0.1666 0.05657 0.0305
    ## Cumulative Proportion 0.4113 0.7463 0.9129 0.96950 1.0000
    ## 
    ## Scaling 1 for species and site scores
    ## * Sites are scaled proportional to eigenvalues
    ## * Species are unscaled: weighted dispersion equal on all dimensions
    ## * General scaling constant of scores:  3.274722 
    ## 
    ## 
    ## Species scores
    ## 
    ##               PC1     PC2      PC3     PC4     PC5
    ## CCA         1.521 -1.5612 -0.64961  2.2884 -0.5591
    ## HardCoral   1.205 -0.5716  2.91871 -0.4726 -0.4505
    ## Macroalgae -1.604  1.2940  1.21660  2.2057 -0.3631
    ## Rubble     -1.640 -1.6043 -0.08013 -0.5796 -2.2623
    ## Turf        1.302  1.9265 -0.54442 -0.2499 -2.2268
    ## 
    ## 
    ## Site scores (weighted sums of species scores)
    ## 
    ##            PC1      PC2      PC3       PC4       PC5
    ## sit1  -0.58688 -0.79351 -0.19217 -0.308469  0.054144
    ## sit2  -0.32020 -0.66695 -0.57685  0.052631  0.231866
    ## sit3   0.19141 -0.41643 -0.08753  0.154765 -0.178633
    ## sit4  -0.17466 -0.48544  0.09507  0.121910 -0.006697
    ## sit5   0.08169  0.34293  0.18938 -0.414307 -0.056794
    ## sit6   0.06313  0.41834 -0.35847 -0.004449 -0.123282
    ## sit7   0.10588  0.27230  0.10124 -0.092222 -0.103632
    ## sit8   0.04798  0.05084  0.15619  0.114016 -0.129374
    ## sit9   0.34910  0.36873 -0.48965 -0.152891  0.010354
    ## sit10  0.47301 -0.16947 -0.21138  0.075028 -0.138126
    ## sit11  0.48137  0.21963 -0.33736 -0.056575 -0.009871
    ## sit12  0.52756  0.11371 -0.22167 -0.020017  0.018292
    ## sit13 -0.57234 -0.11468  0.20456 -0.293880 -0.083507
    ## sit14 -0.02513  0.11526  0.24984  0.194630 -0.025888
    ## sit15 -0.14914 -0.22329  0.11327  0.146952 -0.179366
    ## sit16 -0.52581 -0.16855 -0.06762  0.170064 -0.038711
    ## sit17  0.58316 -0.48044  0.37226  0.066385  0.170124
    ## sit18  0.44929 -0.08083  0.04460  0.099741 -0.013155
    ## sit19  0.58033 -0.04904  0.22466 -0.118111  0.104773
    ## sit20  0.49939 -0.10129  0.41973 -0.028477  0.170631
    ## sit21 -0.49441  0.23485  0.40980 -0.032638 -0.016618
    ## sit22 -0.87182  0.14926  0.04639  0.059176 -0.006612
    ## sit23 -0.31980  0.65972  0.17167  0.125060  0.194294
    ## sit24 -0.39313  0.80434 -0.25595  0.141677  0.155790

``` r
#eigenvalues
(benthos.eig.flat<-benthos.pca.flat$CA$eig)
```

    ##       PC1       PC2       PC3       PC4       PC5 
    ## 2.0563002 1.6752882 0.8330685 0.2828646 0.1524785

``` r
#Scree plot  
screeplot(benthos.pca.flat, bstick=TRUE, npcs=length(benthos.pca.flat$CA$eig))
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/PCA%20benthos%20by%20location-1.png)<!-- -->
\#Reef slope PCA - benthos

``` r
#Reef Slope Benthos PCA####
benthos.pca.slope<-rda(benthos.sqrt.slope, scale=TRUE)
benthos.pca.slope
```

    ## Call: rda(X = benthos.sqrt.slope, scale = TRUE)
    ## 
    ##               Inertia Rank
    ## Total               5     
    ## Unconstrained       5    5
    ## Inertia is correlations 
    ## 
    ## Eigenvalues for unconstrained axes:
    ##    PC1    PC2    PC3    PC4    PC5 
    ## 2.9016 0.9014 0.7466 0.4046 0.0458

``` r
summary(benthos.pca.slope)
```

    ## 
    ## Call:
    ## rda(X = benthos.sqrt.slope, scale = TRUE) 
    ## 
    ## Partitioning of correlations:
    ##               Inertia Proportion
    ## Total               5          1
    ## Unconstrained       5          1
    ## 
    ## Eigenvalues, and their contribution to the correlations 
    ## 
    ## Importance of components:
    ##                          PC1    PC2    PC3     PC4      PC5
    ## Eigenvalue            2.9016 0.9014 0.7466 0.40462 0.045788
    ## Proportion Explained  0.5803 0.1803 0.1493 0.08092 0.009158
    ## Cumulative Proportion 0.5803 0.7606 0.9099 0.99084 1.000000
    ## 
    ## Scaling 2 for species and site scores
    ## * Species are scaled proportional to eigenvalues
    ## * Sites are unscaled: weighted dispersion equal on all dimensions
    ## * General scaling constant of scores:  3.274722 
    ## 
    ## 
    ## Species scores
    ## 
    ##                PC1     PC2     PC3     PC4     PC5
    ## CCA         1.2689  0.4730 -0.2130  0.4831 0.17955
    ## HardCoral   1.1456 -0.5980  0.4591 -0.4954 0.13651
    ## Macroalgae -0.8916  0.4845  1.0445  0.1400 0.06796
    ## Rubble     -1.2500  0.3590 -0.5018 -0.4112 0.18005
    ## Turf       -0.9713 -0.9941 -0.0497  0.4476 0.10146
    ## 
    ## 
    ## Site scores (weighted sums of species scores)
    ## 
    ##            PC1     PC2       PC3      PC4      PC5
    ## sit1  -0.01805 -0.4697 -0.634054 -0.84451  0.45678
    ## sit2  -0.17884 -0.2656 -0.564973  1.63240  0.13517
    ## sit3  -0.81589  0.2718 -1.219707 -0.43241 -0.49293
    ## sit4  -0.72052  0.2888 -1.583541 -0.50773 -0.26818
    ## sit5  -1.18113  0.8375 -0.005403 -0.55528 -0.46185
    ## sit6  -0.74242  0.8650  1.123785 -0.31994 -0.09719
    ## sit7  -0.97168  0.3759  0.285899  0.42806  0.97755
    ## sit8  -0.16929 -0.4571  0.517328  1.49978 -0.15839
    ## sit9   0.44749 -0.3663  0.488311 -0.06778  0.28139
    ## sit10  0.61868 -0.1134  0.304517  0.23456  1.14402
    ## sit11  0.66393 -0.3454  0.532390 -0.86598  0.04211
    ## sit12  0.90713  0.3542  0.232391 -1.29019  0.97397
    ## sit13  0.92066  1.1742 -0.341558  0.72100 -0.01624
    ## sit14  0.97228  1.1458 -0.620491  0.46255 -0.96900
    ## sit15  0.77717  0.5654 -0.124340  0.32880 -0.08917
    ## sit16  0.81677  0.3909  0.063713 -0.19459 -0.13765
    ## sit17  0.27885 -1.2563 -0.098098  0.13304 -0.29089
    ## sit18  0.52340 -0.6385 -0.103276 -0.22791 -0.66345
    ## sit19  0.23000 -1.3870  0.036664 -0.34508 -1.08576
    ## sit20 -0.43111 -0.9558 -0.960428  0.34450  1.07024
    ## sit21 -0.43717 -0.1301  0.276736 -0.57008  0.03538
    ## sit22 -0.19403  0.1274  0.740038  0.20077 -0.38635
    ## sit23 -0.55458  0.1717  0.460385  0.15926  1.15661
    ## sit24 -0.74165 -0.1833  1.193711  0.07677 -1.15616

``` r
summary(benthos.pca.slope, scaling=1)
```

    ## 
    ## Call:
    ## rda(X = benthos.sqrt.slope, scale = TRUE) 
    ## 
    ## Partitioning of correlations:
    ##               Inertia Proportion
    ## Total               5          1
    ## Unconstrained       5          1
    ## 
    ## Eigenvalues, and their contribution to the correlations 
    ## 
    ## Importance of components:
    ##                          PC1    PC2    PC3     PC4      PC5
    ## Eigenvalue            2.9016 0.9014 0.7466 0.40462 0.045788
    ## Proportion Explained  0.5803 0.1803 0.1493 0.08092 0.009158
    ## Cumulative Proportion 0.5803 0.7606 0.9099 0.99084 1.000000
    ## 
    ## Scaling 1 for species and site scores
    ## * Sites are scaled proportional to eigenvalues
    ## * Species are unscaled: weighted dispersion equal on all dimensions
    ## * General scaling constant of scores:  3.274722 
    ## 
    ## 
    ## Species scores
    ## 
    ##               PC1     PC2     PC3     PC4    PC5
    ## CCA         1.666  1.1141 -0.5513  1.6982 1.8763
    ## HardCoral   1.504 -1.4085  1.1881 -1.7413 1.4265
    ## Macroalgae -1.170  1.1411  2.7028  0.4921 0.7102
    ## Rubble     -1.641  0.8454 -1.2986 -1.4456 1.8815
    ## Turf       -1.275 -2.3413 -0.1286  1.5734 1.0602
    ## 
    ## 
    ## Site scores (weighted sums of species scores)
    ## 
    ##            PC1      PC2       PC3      PC4       PC5
    ## sit1  -0.01375 -0.19942 -0.245016 -0.24024  0.043712
    ## sit2  -0.13624 -0.11276 -0.218322  0.46437  0.012935
    ## sit3  -0.62153  0.11539 -0.471329 -0.12301 -0.047171
    ## sit4  -0.54888  0.12263 -0.611925 -0.14443 -0.025664
    ## sit5  -0.89977  0.35558 -0.002088 -0.15796 -0.044197
    ## sit6  -0.56557  0.36728  0.434262 -0.09101 -0.009301
    ## sit7  -0.74021  0.15961  0.110479  0.12177  0.093547
    ## sit8  -0.12896 -0.19408  0.199910  0.42664 -0.015157
    ## sit9   0.34089 -0.15554  0.188697 -0.01928  0.026927
    ## sit10  0.47130 -0.04817  0.117674  0.06673  0.109477
    ## sit11  0.50577 -0.14665  0.205730 -0.24635  0.004029
    ## sit12  0.69104  0.15040  0.089802 -0.36702  0.093205
    ## sit13  0.70135  0.49854 -0.131987  0.20510 -0.001554
    ## sit14  0.74067  0.48648 -0.239775  0.13158 -0.092729
    ## sit15  0.59204  0.24007 -0.048048  0.09353 -0.008534
    ## sit16  0.62221  0.16599  0.024620 -0.05536 -0.013173
    ## sit17  0.21243 -0.53339 -0.037908  0.03785 -0.027837
    ## sit18  0.39872 -0.27112 -0.039909 -0.06483 -0.063489
    ## sit19  0.17521 -0.58891  0.014168 -0.09817 -0.103902
    ## sit20 -0.32842 -0.40582 -0.371136  0.09800  0.102417
    ## sit21 -0.33303 -0.05525  0.106939 -0.16217  0.003385
    ## sit22 -0.14781  0.05410  0.285972  0.05711 -0.036971
    ## sit23 -0.42247  0.07289  0.177906  0.04531  0.110682
    ## sit24 -0.56498 -0.07784  0.461284  0.02184 -0.110639

``` r
#eigenvalues
(benthos.eig.slope<-benthos.pca.slope$CA$eig)
```

    ##        PC1        PC2        PC3        PC4        PC5 
    ## 2.90160317 0.90135744 0.74663321 0.40461845 0.04578773

``` r
screeplot(benthos.pca.slope, bstick=TRUE, npcs=length(benthos.pca.slope$CA$eig))
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/Reef%20slope%20benthos%20PCA-1.png)<!-- -->

\#making PCA plots presentable \#Reef flat benthos PCA

``` r
#extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(benthos.sqrt.flat, display = "sites"))

#add factors back into data frame 
data.scores$Site = benthos.flat$Site
data.scores$Location = benthos.flat$Location
data.scores$Watershed.severity = benthos.flat$Watershed.Type
 
head(data.scores)
```

    ##             CCA HardCoral Macroalgae    Rubble      Turf    Site  Location
    ## site1 0.1732051 0.3162278  0.0000000 0.8000000 0.2236068 Fagaalu Reef Flat
    ## site2 0.3464102 0.1414214  0.0000000 0.5099020 0.2828427 Fagaalu Reef Flat
    ## site3 0.4242641 0.4582576  0.1000000 0.4123106 0.6164414 Fagaalu Reef Flat
    ## site4 0.3162278 0.4690416  0.2236068 0.4898979 0.4123106 Fagaalu Reef Flat
    ## site5 0.0000000 0.5291503  0.1414214 0.1732051 0.8124038  Fagasa Reef Flat
    ## site6 0.1732051 0.2449490  0.2236068 0.1414214 0.9000000  Fagasa Reef Flat
    ##       Watershed.severity
    ## site1          Disturbed
    ## site2          Disturbed
    ## site3          Disturbed
    ## site4          Disturbed
    ## site5       Intermediate
    ## site6       Intermediate

``` r
#Reef Flat
#extract NMDS scores (x and y coordinates)
data.scores.flat = as.data.frame(scores(benthos.pca.flat, display = "sites"))

#add factors back into data frame 
data.scores.flat$Site = benthos.slope$Site
data.scores.flat$Location = benthos.slope$Location
data.scores.flat$Watershed.severity=benthos.slope$Watershed.Type

 
head(data.scores.flat)
```

    ##              PC1        PC2    Site   Location Watershed.severity
    ## sit1 -0.91514250 -1.3708527 Fagaalu Reef Slope          Disturbed
    ## sit2 -0.49929449 -1.1522181 Fagaalu Reef Slope          Disturbed
    ## sit3  0.29847207 -0.7194277 Fagaalu Reef Slope          Disturbed
    ## sit4 -0.27235431 -0.8386429 Fagaalu Reef Slope          Disturbed
    ## sit5  0.12738705  0.5924473  Fagasa Reef Slope       Intermediate
    ## sit6  0.09844818  0.7227148  Fagasa Reef Slope       Intermediate

\#Reef slope benthos PCA \#make plots presentable

``` r
#Reef Slope

#extract NMDS scores (x and y coordinates)
data.scores.slope = as.data.frame(scores(benthos.pca.slope, display = "sites"))

#add factors back into data frame 
data.scores.slope$Site = benthos.slope$Site
data.scores.slope$Location = benthos.slope$Location
data.scores.slope$Watershed.severity = benthos.slope$Watershed.Type


head(data.scores.slope)
```

    ##              PC1        PC2    Site   Location Watershed.severity
    ## sit1 -0.01804682 -0.4696770 Fagaalu Reef Slope          Disturbed
    ## sit2 -0.17883830 -0.2655750 Fagaalu Reef Slope          Disturbed
    ## sit3 -0.81588812  0.2717680 Fagaalu Reef Slope          Disturbed
    ## sit4 -0.72052138  0.2888208 Fagaalu Reef Slope          Disturbed
    ## sit5 -1.18113147  0.8374732  Fagasa Reef Slope       Intermediate
    ## sit6 -0.74242245  0.8650449  Fagasa Reef Slope       Intermediate

\#Plot PCA Benthos reef flat

``` r
library(ggplot2)

xx = ggplot(data.scores.flat, aes(x = PC1, y = PC2)) + 
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
 
xx
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
\#Continue to build up PCA - Reef flat benthos

``` r
en = envfit(benthos.pca.flat, benthos.sqrt.flat, permutations = 1000, na.rm = TRUE)
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

xx = ggplot(data.scores.flat, aes(x = PC1, y = PC2)) + 
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
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Benthos%20PCA%20plot-1.png)<!-- -->

``` r
##

xx = ggplot(data.scores.slope, aes(x = PC1, y = PC2)) + 
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
 

xx
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Benthos%20PCA%20plot-2.png)<!-- -->

``` r
#non-overlapping - but less lined up with vectors
 
ben.flat = ggplot(data.scores.flat, aes(x = PC1, y = PC2)) + 
    geom_point(size = 4, aes( shape =interaction(Site, Watershed.severity), colour= interaction(Site, Watershed.severity))) +
    geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), data = reduced.coord.cont, size =0.1, alpha = 0.5, colour = "grey30") +
    geom_text_repel(data = reduced.coord.cont, aes(x = PC1, y = PC2), colour = "grey30", fontface = "bold", label = row.names(reduced.coord.cont)) + 
    theme(axis.text.y = element_text(colour = "black", size = 10, face = "bold"), axis.text.x = element_text(colour = "black", face = "bold", size = 10), 
    legend.text = element_text(size = 10, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
    legend.title = element_text(size = 12, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank()) + 
    labs(x = "PC1", colour = "Watershed Type", y = "PC2", shape = "Site") 


ben.flat
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Benthos%20PCA%20plot-3.png)<!-- -->

``` r
ben.flat + scale_y_continuous(paste(names(benthos.eig.flat[2]),
                                  sprintf('(%0.1f%% explained var.)', 100*benthos.eig.flat[2]/sum(benthos.eig.flat))))+
  scale_x_continuous(paste(names(benthos.eig.flat[1]),
                           sprintf('(%0.1f%% explained var.)', 100* benthos.eig.flat[1]/sum(benthos.eig.flat)))) + ggtitle("Benthos - Reef Flat") + scale_shape_manual("", values=c(0,15,2,17,5,18,1,16)) + scale_color_manual("", values=c("#9999CC","#9999CC", "#009E73","#009E73","#E69F00","#E69F00")) + scale_fill_brewer("", ) + labs(color  = "Site", linetype = "Site", shape = "Site")
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Benthos%20PCA%20plot-4.png)<!-- -->
\#Reef Slope PCA \#Plot PCA Benthos reef slope

``` r
library(ggplot2)

xy = ggplot(data.scores.slope, aes(x = PC1, y = PC2)) + 
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
 
xy
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
\#Continue to build up PCA - Reef slope benthos

``` r
en = envfit(benthos.pca.slope, benthos.sqrt.slope, permutations = 1000, na.rm = TRUE)
en_coord_cont = as.data.frame(scores(en, "vectors"))

#####################################################

#reduce vectors to those where p<0.05
A <- as.list(en$vectors) #creating the dataframe
pvals<-as.data.frame(A$pvals)
C<-cbind(en_coord_cont, pvals)
Cred<-subset(C,pvals<0.05)#subset to p<0.05 - CHANGE IF NEEDED
reduced.coord.cont <- cbind(Cred, Variables = rownames(Cred))


library(ggrepel) # to stop labels overlapping cange geom_text in the 4th line to geom_text_repel.

##

xy = ggplot(data.scores.slope, aes(x = PC1, y = PC2)) + 
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
 

xy
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Benthos%20reef%20slope%20PCA%20plot-1.png)<!-- -->

``` r
#Continue building plot
#non-overlapping - but less lined up with vectors
 
ben.slope = ggplot(data.scores.slope, aes(x = PC1, y = PC2)) + 
    geom_point(size = 4, aes( shape =interaction(Site, Watershed.severity), colour= interaction(Site, Watershed.severity))) +
    geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), data = reduced.coord.cont, size =0.1, alpha = 0.5, colour = "grey30") +
    geom_text_repel(data = reduced.coord.cont, aes(x = PC1, y = PC2), colour = "grey30", fontface = "bold", label = row.names(reduced.coord.cont)) + 
    theme(axis.text.y = element_text(colour = "black", size = 10, face = "bold"), axis.text.x = element_text(colour = "black", face = "bold", size = 10), 
    legend.text = element_text(size = 10, face ="bold", colour ="black"), 
    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
    legend.title = element_text(size = 12, colour = "black", face = "bold"), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    legend.key=element_blank()) + 
    labs(x = "PC1", colour = "Watershed Type", y = "PC2", shape = "Site") 


ben.slope
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Benthos%20reef%20slope%20PCA%20plot-2.png)<!-- -->

``` r
ben.slope + scale_y_continuous(paste(names(benthos.eig.slope[2]),
                                  sprintf('(%0.1f%% explained var.)', 100*benthos.eig.slope[2]/sum(benthos.eig.slope))))+
  scale_x_continuous(paste(names(benthos.eig.slope[1]),
                           sprintf('(%0.1f%% explained var.)', 100* benthos.eig.slope[1]/sum(benthos.eig.slope)))) + ggtitle("Benthos - Reef Slope") + scale_shape_manual("", values=c(0,15,2,17,5,18,1,16)) + scale_color_manual("", values=c("#9999CC","#9999CC", "#009E73","#009E73","#E69F00","#E69F00")) + scale_fill_brewer("", ) + labs(color  = "Site", linetype = "Site", shape = "Site")
```

![](Benthos_Analyses_8Jun2021_files/figure-gfm/-%20Benthos%20reef%20slope%20PCA%20plot-3.png)<!-- -->
\#\#End analyses benthos - Comeros-Raynal et al. 2021.Catchment to sea
connection: Impacts of terrestrial run-off on benthic. Marine Pollution
Bulletin 169:112530 ecosystems in American Samoa
