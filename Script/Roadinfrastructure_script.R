# Written by Felicity Charles 5/2/2021
# Updated for publishing manuscript 24/11/2021
# Caveat emptor

getwd()
setwd("~/Desktop/Github_2/Roadinfra-Wildlife/Data")

# Read in data
dat <- read.table("Occurrence.txt", header = T, stringsAsFactors = T) # Read the occurrence data into R, ensuring that factor variables are assigned
beh <- read.table("Behaviour.txt", header = T, stringsAsFactors = T) # Read the behaviour data into R, ensuring that factor variables are assigned
rc <- read.table("CharacteristicsR.txt", header = T, stringsAsFactors = T) # Read the site characteristics data into R, ensuring that factor variables are assigned  

str(dat) # Check the structure of dat
head(dat) # Check the first few rows of dat
str(beh) # Check the structure of beh
head(beh); dim(beh) # Check the first few rows of beh


# Check the levels of each column of the occurrence data
levels(dat$Cam_typ)
levels(dat$Clvt_sz)
dat$Clvt_sz <- relevel(dat$Clvt_sz, "small") # Want to reorder this to make sure things look right later
levels(dat$Type)
levels(dat$Cam_num)
levels(dat$Cam_pt)
length(unique(dat$Kangaroo))
length(unique(dat$Whiptail))
length(unique(dat$Macropod))
length(unique(dat$Monitor))
length(unique(dat$Cow))
length(unique(dat$Fox))
length(unique(dat$Possum))
length(unique(dat$Hare))
length(unique(dat$Rabbit))
length(unique(dat$Dog))
length(unique(dat$Deer))
length(unique(dat$Amphibian))
length(unique(dat$Snake))
length(unique(dat$Dragon))
length(unique(dat$Rodent))
length(unique(dat$Quail))
length(unique(dat$Cat))
length(unique(dat$Unknown))
length(unique(dat$All_animals))



# Check the levels of each column of the behaviour data
levels(beh$Cam_typ)
levels(beh$Clvt_sz)
beh$Clvt_sz <- relevel(beh$Clvt_sz, "small") # Want to reorder this to make sure things look right later
length(unique(beh$Site))
levels(beh$Type)
levels(beh$Cam_num)
levels(beh$Cam_pt)
length(unique(beh$Anim_num))
levels(beh$Species)
levels(beh$Behaviour)
length(unique(beh$Day))
length(unique(beh$Month))
length(unique(beh$Year))
length(unique(beh$Hour))
length(unique(beh$Min))
length(unique(beh$Sec))
levels(beh$Age)
levels(beh$Sex)
levels(beh$Img_num)

str(rc)
# Check the levels of each column of the site characteristics data
levels(rc$Suburb)
levels(rc$Road)
levels(rc$Rd_typ)
length(unique(rc$Site))
levels(rc$Cam_typ)
levels(rc$Clvt_sz)
rc$Clvt_sz <- relevel(rc$Clvt_sz, "small") # Want to reorder this to make sure things look right later
length(unique(rc$Clvt_wdt))
length(unique(rc$Clvt_ht))
length(unique(rc$Length))
rc$Length <- sort(rc$Length)# Want to reorder this to make sure things look right later
levels(rc$Construction)
levels(rc$Veg_cov_C1)
length(unique(rc$Veg_hgt_C1_pt1))
length(unique(rc$Veg_hgt_C1_pt2))
length(unique(rc$Veg_hgt_C1_pt3))
levels(rc$Veg_cov_C2)
length(unique(rc$Veg_hgt_C2_pt1))
length(unique(rc$Veg_hgt_C2_pt2))
length(unique(rc$Veg_hgt_C2_pt3))
levels(rc$Veg_cov_C3)
length(unique(rc$Veg_hgt_C3_pt1))
length(unique(rc$Veg_hgt_C3_pt2))
length(unique(rc$Veg_hgt_C3_pt3))
levels(rc$Veg_cov_C4)
length(unique(rc$Veg_hgt_C4_pt1))
length(unique(rc$Veg_hgt_C4_pt2))
length(unique(rc$Veg_hgt_C4_pt3))
levels(rc$Veg_cov_R1)
length(unique(rc$Veg_hgt_R1_pt1))
length(unique(rc$Veg_hgt_R1_pt2))
length(unique(rc$Veg_hgt_R1_pt3))
levels(rc$Veg_cov_R2)
length(unique(rc$Veg_hgt_R2_pt1))
length(unique(rc$Veg_hgt_R2_pt2))
length(unique(rc$Veg_hgt_R2_pt3))
table(rc$Site, rc$Clvt_sz)




## Preliminary data visualisation 
levels(beh$Species) # What species are listed in the behaviour data
table(beh$Species, beh$Behaviour) 
rowSums(dat[, which(colnames(dat) == "Kangaroo"):(ncol(dat)-1)]) # Which rows contained count data for Kangaroo
rowSums(dat[, which(colnames(dat) == "Kangaroo"):(ncol(dat)-1)]) == dat$All_animals 
colSums(dat[, which(colnames(dat) == "Kangaroo"): (ncol(dat)-1)]) # What are the total number of animals observed for each species
dim(dat) # 180 camera points


plot(as.factor(dat$Type), dat$All_animals) # Plot the total number of all animals per camera type, Culvert or Road
plot(dat$All_animals~dat$Type) # Another way to produce the same plot
sum(dat$All_animals[dat$Type == "C"])/2 # How many animals were seen on Culvert cameras, divided by 2, to equalise trapping effort. There were 364.5 animals observed on culvert cameras
sum(dat$All_animals[dat$Type == "R"]) # How many animals were seen on Road cameras. There were 636 animals observed on road cameras

summary(lm(dat$All_animals~dat$Type)) # The distribution of the residuals of this model are not symmetrical, this is likely due to the zero-inflation of the data. This model suggests that there is a statistically significant difference between the numbers of animals observed on culvert cameras, although not on road cameras. 

## Accounting for zero inflation - preliminary look at occurrence data
# Load required packages
library(lattice)
library(MASS)
library(pscl)
library(lmtest)
library(mgcv)

# Poisson GLM
m1 <- glm(All_animals ~ Type, family = 'poisson', data = dat)
summary(m1)
# Check of over/underdispersion in the model
E2 <- resid(m1, type = "pearson")
N <- nrow(dat)
p <- length(coef(m1))
sum(E2^2)/(N-p)
# This model is definitely overdispersed 


# Negative Binomial GLM
m2 <- glm.nb(All_animals ~ Type, data = dat)
summary(m2)
# Dispersion statistic
E2 <- resid(m2, type = "pearson")
N <- nrow(dat)
p <- length(coef(m2)) + 1
sum(E2^2)/(N-p)
# Still overdispersed but much closer to normal than poisson glm


# Zero-inflated Poisson GLM
m3 <- zeroinfl(All_animals ~ Type |
                 Type, 
               dist = 'poisson',
               data = dat)
summary(m3)
# Dispersion statistic
E2 <- resid(m3, type = "pearson")
N <- nrow(dat)
p <- length(coef(m3))
sum(E2^2) / (N - p)
# More overinflated than negative binomial


# Zero-inflated negative binomial glm - appropriate 
m4 <- zeroinfl(All_animals ~ Type |
                 Type,
               dist = 'negbin',
               data = dat)
summary(m4)
# Dispersion statistic
E2 <- resid(m4, type = "pearson")
N <- nrow(dat)
p <- length(coef(m4)) + 1
sum(E2^2) / (N - p)
# Slightly higher overinflation than the negative binomial glm that did not take into account the zero inflation



### CREATE A SPECIES DATAFRAME
str(dat) # Check the structure of dat
str(beh) # Check the structure of beh
head(beh); dim(beh)
colnames(dat) # What are the column names in dat
levels(beh$Species) # What are the Species listed in beh
colnames(dat)[which(colnames(dat)=="Kangaroo"):which(colnames(dat)=="Unknown")]%in%levels(beh$Species) # Are all column names the same in the occurrence data and in the behaviour data

sdat <- data.frame(Species = levels(beh$Species)) # Create a species data frame
sdat$Class <- c(rep("Amphibian", 1), rep("Mammal", 4), rep("Reptile", 1), rep("Mammal", 4), rep("Reptile", 1), rep("Mammal", 1), rep("Bird", 1), rep("Mammal", 2), rep("Reptile", 1), rep("N/A", 1), rep("Mammal", 1)) # Give each species a class
sdat # Check how this looks, make sure it is correct
sdat$Origin <- c(rep("Native", 1), rep("Exotic", 4), rep("Native", 1), rep("Exotic", 2), rep("Native", 5), rep("Exotic", 1), rep("Native", 2), rep("N/A", 1), rep("Native", 1)) # Categorise each species as native or exotic
sdat # Check how this looks, make sure it is correct
sdat$Size <- c(rep("Small",2), rep("Large", 3), rep("Small", 1), rep("Large", 1), rep("Small", 1), rep("Large", 2), rep("Small",6), rep("N/A", 1), rep("Large", 1)) # Categorise each species by their body size
sdat # Check how this looks, make sure it is correct


## Exploring occurrence data
colSums(dat[, which(colnames(dat) == "Kangaroo"): (ncol(dat)-1)]) # What are the total number of animals observed for each species
# We want to analyse data for species separately if there were more than 30 occurrences of that species
# We can analyse Kangaroos, Whiptail, Fox, Hare, Dog, Rodent, and Cat seperately.
# We can analyse Kangaroo, Whiptail, and Macropod data together, and also Hare and Rabbit data together. 

# Sum native species
Native <- c("Amphibian", "Dragon", "Kangaroo", "Macropod", "Monitor", "Possum", "Quail", "Rodent", "Snake", "Whiptail")
rowSums((dat[,which(colnames(dat)%in% Native)]))
dat$Native <- rowSums(dat[,which(colnames(dat)%in% Native)]) # Add this column to the occurrence data
head(dat) # Ensure that this new column was added to the occurrence data

# Sum exotic species
Exotic <- c("Cat", "Cow", "Deer", "Dog", "Fox", "Hare", "Rabbit")
rowSums(((dat[,which(colnames(dat)%in% Exotic)])))
dat$Exotic <- rowSums(dat[,which(colnames(dat)%in% Exotic)])
head(dat)

# Sum large species
Large <- c("Kangaroo", "Macropod", "Whiptail", "Cow","Fox", "Dog","Deer")
rowSums((dat[,which(colnames(dat)%in% Large)]))
dat$Large <- rowSums(dat[,which(colnames(dat) %in% Large)])
head(dat)

# Sum small species
Small <- c("Monitor", "Possum", "Hare", "Rabbit", "Amphibian", "Snake", "Rodent", "Quail", "Dragon", "Cat")
rowSums((dat[,which(colnames(dat) %in% Small)]))
dat$Small <- rowSums(dat[,which(colnames(dat) %in% Small)])
head(dat)

## Stage 1 - analysis
# Are there more animals using culverts to cross roads than crossing over the surface of the road?

table(dat$Type) # How many culvert cameras are there compared to road cameras - more culvert cameras
table(dat$Clvt_sz) # How many large culverts are there compared to small culverts - more small culverts

table(dat$Type, dat$Exotic>0) # There is a difference between the number of Exotic species observed on culvert vs. road cameras
table(dat$Type, dat$Exotic>0)[,2]/rowSums(table(dat$Type, dat$Exotic>0)) # There are slightly more exotic species observed on road cameras than culvert cameras

table(dat$Type, dat$Native>0) # There is a difference between the number of native species observed on culvert vs road cameras
table(dat$Type, dat$Native>0)[,2]/rowSums(table(dat$Type, dat$Native>0)) # There are slightly more native species observed on road cameras than on culvert cameras. 

table(dat$Type, dat$Large>0) # There is a difference between the number of large species observed on culvert vs road cameras
table(dat$Type, dat$Large>0)[,2]/rowSums(table(dat$Type, dat$Large>0)) # There are the same number of large species observed on road cameras and culvert cameras.

table(dat$Type, dat$Small>0) # There is a difference between the number of small species observed on culvert vs road cameras
table(dat$Type, dat$Small>0)[,2]/rowSums(table(dat$Type, dat$Small>0)) # There are slightly more small species observed on road cameras than on culvert cameras. 


# Produce histograms
hist(dat$All_animals[dat$All_animals<50]) # Create a histogram of all animals occurrences less than 50, frequency over 150 for 0 occurrences
hist(dat$Exotic[dat$Exotic<50]) # Zero inflation
hist(dat$Native[dat$Native<50])
hist(dat$Large[dat$Large<50]) # Zero inflation
hist(dat$Small[dat$Small<50])# Zero inflation
# The data is zero inflated, need to determine what R model to use for poisson
# Could use binomial glm - turn the count data into presence/absence data

# Turn count data into presence/absence data
dat$Fox.pa <- ifelse(dat$Fox>0, 1, 0)
m6 <- glm(Fox.pa ~ Type * Clvt_sz, family = "binomial", data = dat)
summary(m6)
# There are significantly more foxes observed on culvert cameras, although as with exotic medium-large mammals this may be due to a higher sampling effort on culvert cameras. The size of the culvert was not statistically significant, this is likely due to Foxes not using culverts to cross roads. 


dat$Cat.pa <- ifelse(dat$Cat>0, 1, 0 )
m7 <- glm(Cat.pa ~ Type * Clvt_sz, family = "binomial", data = dat)
summary(m7)
# There a significantly more cats observed on culvert cameras, although this may be due to a higher sampling effort on culvert cameras. The size of the culvert was not statistically significant, this is likely due to the fact that the culverts monitored would be an appropriate size for cats to utilise. 


dat$Kangaroo.pa <- ifelse(dat$Kangaroo> 0, 1, 0)
m8 <- glm(Kangaroo.pa ~ Type * Clvt_sz, family = "binomial", data = dat)
summary(m8)
# There are significantly more Kangaroos observed on culvert cameras, although this may be due to a higher sampling effort on culvert cameras. The size of the culvert did not have a significant impact on kangaroo occurrences, this may be due to kangaroos often only being recorded at culverts of a size that could facilitate their use. 


dat$Whiptail.pa <- ifelse(dat$Whiptail> 0, 1, 0)
m9 <- glm(Whiptail.pa ~ Type * Clvt_sz, family = "binomial", data = dat)
summary(m9)
# The occurrences of Whiptail wallabies was not statistically significant depending on the camera type or culvert size, this is due to Whiptails only being observed at one location. 

dat$Exotic.pa <- ifelse(dat$Exotic>0,1,0)
m10 <- glm(Exotic.pa ~ Type * Clvt_sz, family = "binomial", data = dat)
summary(m10) # Not significant.
# The occurrences of exotic species did not depend on camera type or culvert size, likely as they are generalist species

dat$Native.pa <- ifelse(dat$Native>0,1,0)
m11 <- glm(Native.pa ~ Type * Clvt_sz, family = "binomial", data = dat)
summary(m11) # Not significant
# The occurrences of native species did not depend on camera type or culvert size, likely due to the large number of macropod species which are generalist species

dat$All_animals.pa <- ifelse(dat$All_animals>0,1,0)
m12 <- glm(All_animals.pa ~ Type * Clvt_sz, family = "binomial", data = dat)
summary(m12) # Not significant


dat$Large.pa <- ifelse(dat$Large>0,1,0)
m12.1 <- glm(Large.pa ~ Type * Clvt_sz, family = "binomial", data = dat)
summary(m12.1) # There is a significant influence of culvert size on large species
# Interestingly large species are significantly positively influenced by culvert cameras on small culverts. However, overall there was a significant negative influence of culvert cameras at large culverts on the probability of observing a large species. May be due to the influence of large exotic species which were not usually observed using culverts. 

dat$Small.pa <- ifelse(dat$Small>0,1,0)
m12.2 <- glm(Small.pa ~ Type * Clvt_sz, family = "binomial", data = dat)
summary(m12.2) # Significant influence of camera type on small species observations but not of culvert size.
# There are significantly less small species observed on culvert cameras at large culverts, therefore indicating that they likely avoid the use of culverts. 


# To better understand the use of culverts by wildlife, we must also consider the influence of environmental variables.
#Let's get the characteristics data into a workable format
rc2<- rc[,c("Site", "Rd_typ")] # Create a new dataset that only contains the information for site and road type
head(rc2); dim(rc2) # Check this worked, there are only 30 sites
which(duplicated(rc2$Site)) # Although this is correct, double check that there are no duplications just in case
dat2 <- merge(dat, rc2, by = "Site", all.x = T, all.y = F) # Merge the data in the new site characteristics dataset with dat2 according to Site. 


# Need to replace NA with 0 in road characteristics data
rc[is.na(rc)] <- 0 # Replace NA with 0
head(rc) # Check that this worked


# Averages of vegetation data per camera point
rc$C1 <- rowMeans(rc[ , c(14, 15, 16)], na.rm = TRUE )
rc$C2 <- rowMeans(rc[ , c(18, 19, 20)], na.rm = TRUE)
rc$C3 <- rowMeans(rc[ , c(22, 23, 24)], na.rm = TRUE)
rc$C4 <- rowMeans(rc[ , c(26, 27, 28)], na.rm = TRUE)
rc$R1 <- rowMeans(rc[ , c(30, 31, 32)], na.rm = TRUE)
rc$R2 <- rowMeans(rc[ , c(34, 35, 36)], na.rm = TRUE)
head(rc) # Ensure that this has worked



# Create a new data frame with this data in it
rc3 <- rc[,c("Site", "C1", "C2", "C3", "C4", "R1", "R2")] 
head(rc3); dim(rc3) # Check this worked, there are only 30 sites
library(dplyr)
library(tidyr)
rc3 <- rc3 %>% mutate_if(is.numeric, round, digits=2)
rc4 <- gather(rc3, key = "Cam_pt", value = "Veg_dense", "C1", "C2", "C3", "C4", "R1", "R2")
rc4 # Take a look and ensure this is correct
head(rc4); dim(rc4) # There are 180 rows of data (6*30 = 180), and 3 columns (Site, Camera point, and Vegetation density)
rc4$Veg_dense <- sort(rc4$Veg_dense)

dat2 <- merge(dat2, rc4, by = c("Site", "Cam_pt"), all.x = T, all.y = F) # Merge the data in the new site characteristics dataset with dat2 according to Site and Camera point. 
head(dat2) # Check that this has worked 


# Take just the vegetation cover data from the road characteristics data frame and add it to dat2
rc5 <- rc[,c("Site", "Veg_cov_C1","Veg_cov_C2","Veg_cov_C3", "Veg_cov_C4", "Veg_cov_R1", "Veg_cov_R2")] # Create a new dataset that only contains the information for site and vegetation 
rc5 # Check that this has worked
# To be able to stack the columns we need the package tidyr
rc6 <- gather(rc5, key = "Cam_pt", value = "Veg_cov", "Veg_cov_C1", "Veg_cov_C2", "Veg_cov_C3", "Veg_cov_C4", "Veg_cov_R1", "Veg_cov_R2") # Stack the vegetation cover columns on top of each other
rc6 # Check that this worked

# This has worked although we need to change the values in the column Cam_pt so that they can be used to merge by camera point with dat2

rc6$Cam_pt[1:30] <- "C1"
rc6$Cam_pt[31:60] <- "C2"
rc6$Cam_pt[61:90] <- "C3"
rc6$Cam_pt[91:120] <- "C4"
rc6$Cam_pt[121:150] <- "R1"
rc6$Cam_pt[151:180] <- "R2"
rc6 # Check that this has worked, all looks good can now merge with dat2

dat2 <- merge(dat2, rc6, by = c("Site", "Cam_pt"), all.x = T , all.y = F) # Merge the data in the new site characteristics dataset with dat2 according to Site and Camera point.
head(dat2); dim(dat2)


rc7 <- rc[,c("Site", "Length", "Clvt_ht")] # Create a new data frame with just the site, length,and Clvt_ht of the culvert
dat2 <- merge(dat2, rc7, by = "Site", all.x = T, all.y = F) # Merge this with beh4 but create a new data set 

# Models with type are better than those that exclude it - explore occurrence data to determine what environmental variables influence species presence/absence
head(dat2);dim(dat2) # Look at the first few rows of data
citation("lme4")

# Load packages
library(lmerTest) 
library(lme4)
library(AICcmodavg)

str(dat2) # Need to fix some issues with variables
dat2$Veg_cov <- factor(dat2$Veg_cov, levels = c("none", "low", "medium", "high")) # Reorder veg_cov
str(dat2$Rd_typ) # Need to fix this one also
dat2$Rd_typ <- factor(dat2$Rd_typ, levels = c("minor", "major"))# Reorder rd_typ


table(dat2$Length, dat2$Site) # I want to look at Road type compared to length
# Begin creating a dataframe to do so
Site <- 1:30
Road <- data.frame(Site)
Road$Rd_typ <- c(rep("major", 2), rep("minor", 1), rep("major", 3), rep("minor", 2), rep("major", 16), rep("minor", 3), rep("major", 3))
Road$Length <- c(rep("8.15", 1), rep("8.4",1), rep("7.1",1), rep("9.65", 1), rep("7.95", 1), rep("10",1), rep("11.05",1), rep("11.1",1), rep("7.3",1), rep("9.25",1), rep("9.55",1), rep("9.85",1), rep("9.35",1), rep("9.75",1), rep("7.15",1), rep("12.1",1), rep("9.9",1), rep("9.35",1), rep("11.2",1), rep("7,4",1), rep("6.65",1), rep("14.8",1), rep("9.8",1), rep("9.35",1), rep("12.6",1), rep("8.9",1), rep("9.3",1), rep("10.6",1), rep("12.9",1), rep("11.25",1)) # Need to do this otherwise it wasn't odering correctly
table(Road$Length, Road$Rd_typ) # Minor roads are not necessarily narrower roads or roads with shorter culverts, some major roads have shorter culverts on narrower roads. Road type isn't really a great indicator for road size in this instance.


# Before jumping into analysis, as there are many variables, lets first look for correlations to simplify the model set. We are really interested in how vegetation may influence culvert use.
head(dat)

cor.test(dat2$Length, dat2$Veg_dense) # Correlated, more veg dense on wider rd
# road length and vegetation density were correlated (r = 0.23, p = 0.001)
rl <- lmer(Veg_dense ~ Length + (1 |Site), data = dat2)
summary(rl) # Summarise the relationship between veg dense and length
plot(dat2$Length, dat2$Veg_dense) # How does this look as a plot

# NOTE - Vegetation cover was no longer included as we preferred to use the numerical version of vegetation measurements - vegetation density

cor.test(ifelse(dat2$Rd_typ== "major",1,0), dat2$Veg_dense) # Check correlation of veg dense with rd type
summary(lm(Veg_dense ~ Rd_typ, data = dat2)) # Relationship between veg dense and road type (not going to be included in the thesis as these are not the results we expect)


summary(lm(Veg_dense ~ Length, data = dat2)) # Relationship between veg dense and road width 




Cs <- lmer(Veg_dense ~ Clvt_sz + (1 | Site), data = dat2) # Relationship of veg dense with culvert size
summary(Cs) # Not sig.
cor.test(ifelse(dat2$Clvt_sz == "large", 1,0), dat2$Veg_dense) # 0.04, p=0.62



# Vegetation density is correlated with the other variables so will be used in their place. However, it is not correlated with culvert size, which we are also interested in determining the influence of culvert size on observing wildlife so models for this variable will also be produced. 

# Need to test the correlation of culvert size with other variables

cor.test(ifelse(dat2$Clvt_sz == "large", 1,0), dat2$Veg_dense) # 0.04, p=0.622

cor.test(ifelse(dat2$Clvt_sz == "large", 1,0), dat2$Length) # 0.024, p=0.747

cor.test(ifelse(dat2$Clvt_sz == "large",1,0), ifelse(dat2$Rd_typ == "major", 1,0)) #-0.09, p=0.199



## Create the plots for appendix to show correlations with culvert size
dev.new(width=20, height=20, dpi=80, pointsize=28, noRStudioGD = T)
par(mfrow=c(2,2), mgp=c(2.5,1,0), mar=c(4,4,3,3), cex = 1, las = 1)

plot(dat2$Clvt_sz, dat2$Veg_dense, xlab = "Culvert size", ylab = "Vegetation density (m)", las = 1)
title(main = "(a)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("top", legend = "r = 0.037,
p = 0.62", cex = 1, bty = "n", text.width = 1)

plot(dat2$Clvt_sz, dat2$Length, xlab = "Culvert size", ylab = "Road width/Culvert length (m)", las = 1)
title(main = "(b)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.024, p = 0.74", cex = 1, bty = "n", text.width = 1)



## Create the plots for appendix to show correlations with Vegetation density
dev.new(width=20, height=20, dpi=80, pointsize=28, noRStudioGD= T)
par(mfrow=c(2, 2), mgp=c(2.5,1,0), mar=c(4,4,3,3), cex = 1, las = 1)

plot(dat2$Length, dat2$Veg_dense, xlab = "Culvert length (m)", ylab = "Vegetation density (m)", las = 1) # As vegetation density increases so do length
title(main = "(a)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.23, p = 0.001", cex = 1, bty ="n", text.width = 1)


plot(dat2$Clvt_sz, dat2$Veg_dense, xlab = "Culvert size", ylab = "Vegetation density (m)", las = 1)
title(main = "(b)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("top", legend = "r = 0.03, 
p = 0.62", cex = 1, bty ="n", text.width =1)

plot(dat2$Clvt_sz, dat2$Length, xlab = "Culvert size", ylab = "Road width/Culvert length (m)", las = 1)
title(main = "(c)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.02, p = 0.74", cex = 1, bty = "n", text.width = 1)





# Model the probability of observing an animal in relation to vegetation density
Mnull <- glmer(Exotic.pa ~ 1 + (1 | Site), family = "binomial", data = dat2)
summary(Mnull) # Model if nothing is occurring

#What is the Log Likelihood of this model?
logLik(Mnull)

M1 <- glmer(Exotic.pa ~ Type * Veg_dense + (1 | Site), family = "binomial", data = dat2)
summary(M1) # Produce interactive model, Type is best included, but is not significant

#What is the Log Likelihood of this model?
logLik(M1)

M1a <- glmer(Exotic.pa ~ Veg_dense + (1 | Site), family = "binomial", data = dat2)
summary(M1a) # Model veg dense only
#What is the Log Likelihood of this model?
logLik(M1a)


M1b <- glmer(Exotic.pa ~ Type + Veg_dense + (1 | Site), family = "binomial", data = dat2)
summary(M1b) # Produce additive model
#What is the Log Likelihood of this model?
logLik(M1b)


M2 <- glmer(Exotic.pa ~ Type + (1 | Site), family = "binomial", data = dat2)
summary(M2) # Model type only 
#What is the Log Likelihood of this model?
logLik(M2)

# This is an okay model, other models explain what is occurring better with type included 
# As we have found in previous models, there are significantly less exotic species observed on culvert cameras than road cameras. Culverts are significantly less likely to observe exotic species (P=0.00254), therefore culverts are not used by exotic species!
AICc(Mnull); AICc(M2) # Null model is better


M3 <- glmer(Exotic.pa ~ Type * Clvt_sz + (1 | Site), family = "binomial", data = dat2)
summary(M3) # Significant negative influence of culvert cameras at small culverts on exotic species occurrences
#What is the Log Likelihood of this model?
logLik(M3)


M3a <- glmer(Exotic.pa ~ Clvt_sz + (1 | Site), family = "binomial", data = dat2)
summary(M3a) # Significant negative influence of small culverts on exotic species occurrences
#What is the Log Likelihood of this model?
logLik(M3a)


M3b <- glmer(Exotic.pa ~ Type + Clvt_sz + (1 | Site), family = "binomial", data = dat2)# Produce additive model
summary(M3b) 
# significant negative influence of small culverts at culvert cameras on exotic species occurrences
#What is the Log Likelihood of this model?
logLik(M3b)


# Predict from the interactive model
ndM3 <- data.frame(Type = as.factor(c(rep("C", 2), rep("R", 2))), Clvt_sz = as.factor(c(rep("small", 2), rep("large", 2))))
prM3 <- predictSE(mod = M3, newdata = ndM3, se.fit = T, type = "response")
prM3 <- data.frame(ndM3, fit = round(prM3$fit, 4), se = round(prM3$se.fit, 4))
prM3$lci <- prM3$fit - (prM3$se * 1.96)
prM3$uci <- prM3$fit + (prM3$se * 1.96)

# Plot predictions from the interactive model
plot(prM3$Clvt_sz[prM3$Type=="C"], prM3$fit[prM3$Type=="C"], pch=20, ylim=c(min(prM3$lci), max(prM3$uci)), xlab = "Culvert size", ylab = "Prob. of occurrence", type="b")
points(prM3$Clvt_sz[prM3$Type=="C"], prM3$lci[prM3$Type=="C"], lty=2)
points(prM3$Clvt_sz[prM3$Type=="C"], prM3$uci[prM3$Type=="C"], lty=2)
points(prM3$Clvt_sz[prM3$Type=="R"], prM3$fit[prM3$Type=="R"], lty=1, col="red")
points(prM3$Clvt_sz[prM3$Type=="R"], prM3$lci[prM3$Type=="R"], lty=2, col="red")
points(prM3$Clvt_sz[prM3$Type=="R"], prM3$uci[prM3$Type=="R"], lty=2, col="red")



# Predict from the additive model
ndM3b <- data.frame(Type = as.factor(c(rep("C", 2), rep("R", 2))), Clvt_sz = as.factor(c(rep("small", 2), rep("large", 2))))
prM3b <- predictSE(mod = M3b, newdata = ndM3b, se.fit = T, type = "response")
prM3b <- data.frame(ndM3b, fit = round(prM3b$fit, 4), se = round(prM3b$se.fit, 4))
prM3b$lci <- prM3b$fit - (prM3b$se * 1.96)
prM3b$uci <- prM3b$fit + (prM3b$se * 1.96)

# Plot predictions from additive model
plot(prM3b$Clvt_sz[prM3b$Type=="C"], prM3b$fit[prM3b$Type=="C"], pch=20, ylim=c(min(prM3b$lci), max(prM3b$uci)), xlab = "Culvert size", ylab = "Prob. of occurrence", type="b")
points(prM3b$Clvt_sz[prM3b$Type=="C"], prM3b$lci[prM3b$Type=="C"], lty=2)
points(prM3b$Clvt_sz[prM3b$Type=="C"], prM3b$uci[prM3b$Type=="C"], lty=2)
points(prM3b$Clvt_sz[prM3b$Type=="R"], prM3b$fit[prM3b$Type=="R"], lty=1, col="red")
points(prM3b$Clvt_sz[prM3b$Type=="R"], prM3b$lci[prM3b$Type=="R"], lty=2, col ="red")
points(prM3b$Clvt_sz[prM3b$Type=="R"], prM3b$uci[prM3b$Type=="R"], lty=2, col ="red")

AICc(Mnull); AICc(M3); AICc(M3a); AICc(M3b) # Null model is better, but M3a the size only model is second best

AICc(Mnull); AICc(M1); AICc(M1a); AICc(M1b); AICc(M2); AICc(M3); AICc(M3a); AICc(M3b) # Null model is best, M1a is best model for veg dense which is quite similar to the type only model

cand.set <- list(Mnull,M1,M1a, M1b, M2, M3, M3a, M3b)

aictab(cand.set)


# For Native species p/a 

Mnull.1 <- glmer(Native.pa ~ 1 + (1 | Site), family = "binomial", data = dat2)
summary(Mnull.1) # What is happening if nothing was occurring
#What is the Log Likelihood of this model?
logLik(Mnull.1)


M4 <- glmer(Native.pa ~ Type * Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M4) # Produce interactive model, Significant
#What is the Log Likelihood of this model?
logLik(M4)

# Predict from this interactive model
ndM4 <- data.frame(Type = as.factor(c(rep("C", 50), rep("R", 50))), Veg_dense = seq(min(dat2$Veg_dense), max(dat2$Veg_dense), length.out = 50))
prM4 <- predictSE(mod = M4, newdata = ndM4, se.fit = T, type = "response")
prM4 <- data.frame(ndM4, fit = round(prM4$fit, 4), se = round(prM4$se.fit, 4))
prM4$lci <- prM4$fit - (prM4$se * 1.96)
prM4$uci <- prM4$fit + (prM4$se * 1.96)

# Plot predictions from interactive model
plot(prM4$Veg_dense[prM4$Type=="C"], prM4$fit[prM4$Type=="C"], pch=20, ylim=c(min(prM4$lci), max(prM4$uci)), xlab = "Veg. density", ylab = "Prob. of occurrence", type="l")
lines(prM4$Veg_dense[prM4$Type=="C"], prM4$lci[prM4$Type=="C"], lty=2)
lines(prM4$Veg_dense[prM4$Type=="C"], prM4$uci[prM4$Type=="C"], lty=2)
lines(prM4$Veg_dense[prM4$Type=="R"], prM4$fit[prM4$Type=="R"], lty=1, col="red")
lines(prM4$Veg_dense[prM4$Type=="R"], prM4$lci[prM4$Type=="R"], lty=2, col ="red")
lines(prM4$Veg_dense[prM4$Type=="R"], prM4$uci[prM4$Type=="R"], lty=2, col ="red")
title(main = "(b) Native species", outer = F, adj = 0, cex.main = 1, line = 0.3)

M4a <- glmer(Native.pa ~ Veg_dense + (1 | Site), family = "binomial", data = dat2)
summary(M4a) # Model veg dense only

#What is the Log Likelihood of this model?
logLik(M4a)


M4b <- glmer(Native.pa ~ Type + Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M4b) # Produce additive model, Significant 
#What is the Log Likelihood of this model?
logLik(M4b)


# Predict from additive model
ndM4b <- data.frame(Type = as.factor(c(rep("C", 50), rep("R", 50))), Veg_dense = seq(min(dat2$Veg_dense), max(dat2$Veg_dense), length.out = 50))
prM4b <- predictSE(mod = M4b, newdata = ndM4b, se.fit = T, type = "response")
prM4b <- data.frame(ndM4b, fit = round(prM4b$fit, 4), se = round(prM4b$se.fit, 4))
prM4b$lci <- prM4b$fit - (prM4b$se * 1.96)
prM4b$uci <- prM4b$fit + (prM4b$se * 1.96)
head(prM4b)

# Plot predictions from this model
plot(prM4b$Veg_dense[prM4b$Type=="C"], prM4b$fit[prM4b$Type=="C"], pch=20, ylim=c(min(prM4b$lci), max(prM4b$uci)), xlab = "Veg. density", ylab = "Prob. of occurrence", type="l")
lines(prM4b$Veg_dense[prM4b$Type=="C"], prM4b$lci[prM4b$Type=="C"], lty=2)
lines(prM4b$Veg_dense[prM4b$Type=="C"], prM4b$uci[prM4b$Type=="C"], lty=2)
lines(prM4b$Veg_dense[prM4b$Type=="R"], prM4b$fit[prM4b$Type=="R"], lty=1, col="red")
lines(prM4b$Veg_dense[prM4b$Type=="R"], prM4b$lci[prM4b$Type=="R"], lty=2, col ="red")
lines(prM4b$Veg_dense[prM4b$Type=="R"], prM4b$uci[prM4b$Type=="R"], lty=2, col ="red")


anova(Mnull.1, M4) # Significant


M5 <- glmer(Native.pa ~ Type + (1|Site), family = "binomial", data = dat2)
summary(M5) # Model type only, it is significant although the AIC is not within 2 of the other two models
#What is the Log Likelihood of this model?
logLik(M5)


# Native species are negatively influence by culvert cameras to a significant degree (P=7.35e-05), and positively influenced by road cameras to a significant degree (P=0.0399). Therefore native species have a higher probability of being observed on road cameras than on culvert cameras. Native species, like exotic species, are not approaching and hanging around culverts, increasing the probability of their observations on road cameras. 
AICc(Mnull.1); AICc(M5) # Better than null

AICc(Mnull.1); AICc(M4); AICc(M4a); AICc(M4b); AICc(M5) # M4b is best then M5, better than null and type only


M6 <- glmer(Native.pa ~ Type * Clvt_sz + (1 | Site), family = "binomial", data = dat2)
summary(M6) # Significant negative influence of culvert cameras at small culverts on native species occurrences
#What is the Log Likelihood of this model?
logLik(M6)

M6a <- glmer(Native.pa ~ Clvt_sz + (1 | Site), family = "binomial", data = dat2)
summary(M6a) # Significant negative influence of small culverts on native species occurrences
#What is the Log Likelihood of this model?
logLik(M6a)


M6b <- glmer(Native.pa ~ Type + Clvt_sz + (1 | Site), family = "binomial", data = dat2)# Produce additive model
summary(M6b) 
# Significant negative influence of culvert cameras at small culverts and positive influence of road cameras at small culverts on native species occurrences
#What is the Log Likelihood of this model?
logLik(M6b)

# Predict from interactive model
ndM6 <- data.frame(Type = as.factor(c(rep("C", 2), rep("R", 2))), Clvt_sz = as.factor(c(rep("small", 2), rep("large", 2))))
prM6 <- predictSE(mod = M6, newdata = ndM6, se.fit = T, type = "response")
prM6 <- data.frame(ndM6, fit = round(prM6$fit, 4), se = round(prM6$se.fit, 4))
prM6$lci <- prM6$fit - (prM6$se * 1.96)
prM6$uci <- prM6$fit + (prM6$se * 1.96)

# Plot predictions from this model
plot(prM6$Clvt_sz[prM6$Type=="C"], prM6$fit[prM6$Type=="C"], pch=20, ylim=c(min(prM6$lci), max(prM6$uci)), xlab = "Culvert size", ylab = "Prob. of occurrence", type = "b")
points(prM6$Clvt_sz[prM6$Type=="C"], prM6$lci[prM6$Type=="C"], lty=2)
points(prM6$Clvt_sz[prM6$Type=="C"], prM6$uci[prM6$Type=="C"], lty=2)
points(prM6$Clvt_sz[prM6$Type=="R"], prM6$fit[prM6$Type=="R"], lty=1, col="red")
points(prM6$Clvt_sz[prM6$Type=="R"], prM6$lci[prM6$Type=="R"], lty=2, col="red")
points(prM6$Clvt_sz[prM6$Type=="R"], prM6$uci[prM6$Type=="R"], lty=2, col="red")


# Predict from the additive model
ndM6b <- data.frame(Type = as.factor(c(rep("C", 2), rep("R", 2))), Clvt_sz = as.factor(c(rep("small", 2), rep("large", 2))))
prM6b <- predictSE(mod = M6b, newdata = ndM6b, se.fit = T, type = "response")
prM6b <- data.frame(ndM6b, fit = round(prM6b$fit, 4), se = round(prM6b$se.fit, 4))
prM6b$lci <- prM6b$fit - (prM6b$se * 1.96)
prM6b$uci <- prM6b$fit + (prM6b$se * 1.96)

# Plot predictions from this model
plot(prM6b$Clvt_sz[prM6b$Type=="C"], prM6b$fit[prM6b$Type=="C"], pch = 20, ylim=c(min(prM6b$lci), max(prM6b$uci)), xlab = "Culvert size", ylab = "Prob. of occurrence", type = "b")
points(prM6b$Clvt_sz[prM6b$Type=="C"], prM6b$lci[prM6b$Type=="C"], lty=2)
points(prM6b$Clvt_sz[prM6b$Type=="C"], prM6b$uci[prM6b$Type=="C"], lty=2)
points(prM6b$Clvt_sz[prM6b$Type=="R"], prM6b$fit[prM6b$Type=="R"], lty=1, col="red")
points(prM6b$Clvt_sz[prM6b$Type=="R"], prM6b$lci[prM6b$Type=="R"], lty=2, col="red")
points(prM6b$Clvt_sz[prM6b$Type=="R"], prM6b$uci[prM6b$Type=="R"], lty=2, col="red")

AICc(Mnull.1); AICc(M6); AICc(M6a); AICc(M6b) # The additive model is the best model, and is an improvement on the null model


# Native species are significantly more likely to be observed at road cameras in proximity to large culverts than on culvert cameras at large culverts.Although as height increases, observation do slightky increase. 

AICc(Mnull.1); AICc(M4); AICc(M4a); AICc(M4b); AICc(M5); AICc(M6); AICc(M6a); AICc(M6b) # Null model is best, M1a is best model for veg dense which is quite similar to the type only model

cand.set <- list(Mnull.1,M4,M4a, M4b, M5, M6, M6a, M6b)

aictab(cand.set)


# Plots for thesis
dev.new(width=12, height=10, dpi=80, pointsize=20, noRStudioGD = T)
par(mfrow=c(2, 2), mgp=c(2.5,1,0), mar=c(4,4,2,2), oma=c(0,0,0,6), cex = 1, las = 1)

plot(prM4$Veg_dense[prM4$Type=="C"], prM4$fit[prM4$Type=="C"], pch=20, ylim=c(min(prM4$lci), max(prM4$uci)), xlab = "Veg. density", ylab = "Prob. of occurrence", type="l")
lines(prM4$Veg_dense[prM4$Type=="C"], prM4$lci[prM4$Type=="C"], lty=2)
lines(prM4$Veg_dense[prM4$Type=="C"], prM4$uci[prM4$Type=="C"], lty=2)
lines(prM4$Veg_dense[prM4$Type=="R"], prM4$fit[prM4$Type=="R"], lty=1, col="red")
lines(prM4$Veg_dense[prM4$Type=="R"], prM4$lci[prM4$Type=="R"], lty=2, col ="red")
lines(prM4$Veg_dense[prM4$Type=="R"], prM4$uci[prM4$Type=="R"], lty=2, col ="red")

plot(prM4b$Veg_dense[prM4b$Type=="C"], prM4b$fit[prM4b$Type=="C"], pch=20, ylim=c(min(prM4b$lci), max(prM4b$uci)), xlab = "Vegetation density (m)", ylab = "Probability of occurrence", type="l", las = 1)
lines(prM4b$Veg_dense[prM4b$Type=="C"], prM4b$lci[prM4b$Type=="C"], lty=2)
lines(prM4b$Veg_dense[prM4b$Type=="C"], prM4b$uci[prM4b$Type=="C"], lty=2)
lines(prM4b$Veg_dense[prM4b$Type=="R"], prM4b$fit[prM4b$Type=="R"], lty=1, col="red")
lines(prM4b$Veg_dense[prM4b$Type=="R"], prM4b$lci[prM4b$Type=="R"], lty=2, col ="red")
lines(prM4b$Veg_dense[prM4b$Type=="R"], prM4b$uci[prM4b$Type=="R"], lty=2, col ="red")

plot(prM6b$Clvt_sz[prM6b$Type=="C"], prM6b$fit[prM6b$Type=="C"], pch = 20, ylim=c(min(prM6b$lci), max(prM6b$uci)), xlab = "Culvert size", ylab = "Prob. of occurrence", type = "b")
points(prM6b$Clvt_sz[prM6b$Type=="C"], prM6b$lci[prM6b$Type=="C"], lty=2)
points(prM6b$Clvt_sz[prM6b$Type=="C"], prM6b$uci[prM6b$Type=="C"], lty=2)
points(prM6b$Clvt_sz[prM6b$Type=="R"], prM6b$fit[prM6b$Type=="R"], lty=1, col="red")
points(prM6b$Clvt_sz[prM6b$Type=="R"], prM6b$lci[prM6b$Type=="R"], lty=2, col="red")
points(prM6b$Clvt_sz[prM6b$Type=="R"], prM6b$uci[prM6b$Type=="R"], lty=2, col="red")

par(xpd=NA)
legend(x=6.5, y=1,legend =c("Culvert", "Road"), col = c("black", "red"), lty=1, lwd =1, cex = 1, bty ="n", text.width = 0.2)
par(xpd=F) # just make note this model isn't very good - something else going on as well, good for low veg but not for high, we can see there is an effect at low vegetation densities but not at high, whereas the additive model displays that the effect of vegetation and position is separate, overall more animals observed at roads than culverts but also high occurrences at low densities , additive - effect of veg and position 




# For Kangaroo p/a 
Mnull.2 <- glmer(Kangaroo.pa ~ 1 + (1|Site), family = "binomial", data = dat2)
summary(Mnull.2) # Produce null model

#What is the Log Likelihood of this model?
logLik(Mnull.2)


M7 <- glmer(Kangaroo.pa ~ Type * Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M7)  # Produce interactive model
#What is the Log Likelihood of this model?
logLik(M7)


M7a <- glmer(Kangaroo.pa ~ Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M7a) # Produce veg dense only model
#What is the Log Likelihood of this model?
logLik(M7a)

M7b <- glmer(Kangaroo.pa ~ Type + Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M7b) # Produce additive model
#What is the Log Likelihood of this model?
logLik(M7b)


# Low and high vegetation densities at culverts had a negative influence on the probability of observing Kangaroos, significantly so for increasing vegetation densities at culverts. Whereas low vegetation densities at roads had a positive influence on the probability of observing a Kangaroo, although as vegetation density increased this influence became negative. 


anova(Mnull.2, M7) # Not significant


M8 <- glmer(Kangaroo.pa ~ Type + (1|Site), family = "binomial", data = dat2)
summary(M8) # Produce type only model
# Culverts have a negative influence on the probability of observing Kangaroos, statistically significantly so (P=0.00217), whereas roads have a positive influence on the probability of observing Kangaroos.

#What is the Log Likelihood of this model?
logLik(M8)

AICc(Mnull.2); AICc(M8) # Null model is better
AICc(Mnull.2); AICc(M7); AICc(M7a); AICc(M7b); AICc(M8) # NUll model is best model so will not be plotted



M9 <- glmer(Kangaroo.pa ~ Type * Clvt_sz + (1 | Site), family = "binomial", data = dat2)
summary(M9) # Significant negative influence of culvert cameras at small culverts on kangaroo occurrences
#What is the Log Likelihood of this model?
logLik(M9)

M9a <- glmer(Kangaroo.pa ~ Clvt_sz + (1 | Site), family = "binomial", data = dat2)
summary(M9a) # Significant negative influence of small culverts on kangaroo observations
#What is the Log Likelihood of this model?
logLik(M9a)


M9b <- glmer(Kangaroo.pa ~ Type + Clvt_sz + (1 | Site), family = "binomial", data = dat2)# Produce additive model
summary(M9b) 
# Signifncant negative influence of culvert cameras at small culverts on kangaroo observations
#What is the Log Likelihood of this model?
logLik(M9b)

AICc(Mnull.2); AICc(M7); AICc(M7a); AICc(M7b); AICc(M8); AICc(M9); AICc(M9a); AICc(M9b) # Null model is best, 

cand.set <- list(Mnull.2,M7,M7a, M7b, M8, M9, M9a, M9b)

aictab(cand.set)



## For all animals
Mnull.3 <- glmer(All_animals.pa ~ 1 + (1|Site), family = "binomial", data = dat2)
summary(Mnull.3) # Produce null model
#What is the Log Likelihood of this model?
logLik(Mnull.3)

M10 <- glmer(All_animals.pa ~ Type * Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M10) # Produce interactive model, Significant
#What is the Log Likelihood of this model?
logLik(M10)

# Predict from this interactive model
ndM10 <- data.frame(Type = as.factor(c(rep("C", 50), rep("R", 50))), Veg_dense = seq(min(dat2$Veg_dense), max(dat2$Veg_dense), length.out = 50))
prM10 <- predictSE(mod = M10, newdata = ndM10, se.fit = T, type = "response")
prM10 <- data.frame(ndM10, fit = round(prM10$fit, 4), se = round(prM10$se.fit, 4))
prM10$lci <- prM10$fit - (prM10$se * 1.96)
prM10$uci <- prM10$fit + (prM10$se * 1.96)

# Plot predictions from interactive model
plot(prM10$Veg_dense[prM10$Type=="C"], prM10$fit[prM10$Type=="C"], pch=20, ylim=c(min(prM10$lci), max(prM10$uci)), xlab = "Veg. density", ylab = "Prob. of occurrence", type="l")
lines(prM10$Veg_dense[prM10$Type=="C"], prM10$lci[prM10$Type=="C"], lty=2)
lines(prM10$Veg_dense[prM10$Type=="C"], prM10$uci[prM10$Type=="C"], lty=2)
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$fit[prM10$Type=="R"], lty=1, col="red")
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$lci[prM10$Type=="R"], lty=2, col ="red")
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$uci[prM10$Type=="R"], lty=2, col ="red")


M10a <- glmer(All_animals.pa ~ Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M10a) # Produce veg dense only model
#What is the Log Likelihood of this model?
logLik(M10a)

M10b <- glmer(All_animals.pa ~ Type + Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M10b) # Produce additive model, Significant
#What is the Log Likelihood of this model?
logLik(M10b)

# Predict from the additive model
ndM10b <- data.frame(Type = as.factor(c(rep("C", 50), rep("R", 50))), Veg_dense = seq(min(dat2$Veg_dense), max(dat2$Veg_dense), length.out = 50))
prM10b <- predictSE(mod = M10b, newdata = ndM10b, se.fit = T, type = "response")
prM10b <- data.frame(ndM10b, fit = round(prM10b$fit, 4), se = round(prM10b$se.fit, 4))
prM10b$lci <- prM10b$fit - (prM10b$se * 1.96)
prM10b$uci <- prM10b$fit + (prM10b$se * 1.96)
head(prM10b)

# Plot predictions from the additive model
plot(prM10b$Veg_dense[prM10b$Type=="C"], prM10b$fit[prM10b$Type=="C"], pch=20, ylim=c(min(prM10b$lci), max(prM10b$uci)), xlab = "Veg. density", ylab = "Prob. of occurrence", type="l")
lines(prM10b$Veg_dense[prM10b$Type=="C"], prM10b$lci[prM10b$Type=="C"], lty=2)
lines(prM10b$Veg_dense[prM10b$Type=="C"], prM10b$uci[prM10b$Type=="C"], lty=2)
lines(prM10b$Veg_dense[prM10b$Type=="R"], prM10b$fit[prM10b$Type=="R"], lty=1, col="red")
lines(prM10b$Veg_dense[prM10b$Type=="R"], prM10b$lci[prM10b$Type=="R"], lty=2, col ="red")
lines(prM10b$Veg_dense[prM10b$Type=="R"], prM10b$uci[prM10b$Type=="R"], lty=2, col ="red")

AICc(Mnull.3); AICc(M10); AICc(M10a); AICc(M10b); AICc(M11) # M10b is best then M11

# Lower vegetation densities at culvert and road locations had a positive influence on the probability of observing an animal, whereas increasing vegetation densities had a negative influence on the probability of observing an animal. 
anova(Mnull.3, M10) # Significant




M11 <- glmer(All_animals.pa ~ Type + (1|Site), family = "binomial", data = dat2)
summary(M11) # Produce the type only model, Not quite significant
# Culverts and road locations both positively influence the probability of observing an animal, although road cameras are more likely to observe an animal than culvert cameras. 
#What is the Log Likelihood of this model?
logLik(M11)

anova(Mnull.3, M11) # Not significant
AICc(Mnull.3); AICc(M11)  # Only just better than null but not within 2 of the interactive and additive model so will not be plotted



M12 <- glmer(All_animals.pa ~ Type * Clvt_sz + (1 | Site), family = "binomial", data = dat2)
summary(M12) # Not sig.
#What is the Log Likelihood of this model?
logLik(M12)

M12a <- glmer(All_animals.pa ~ Clvt_sz + (1 | Site), family = "binomial", data = dat2)
summary(M12a) # Not sig.
#What is the Log Likelihood of this model?
logLik(M12a)


M12b <- glmer(All_animals.pa ~ Type + Clvt_sz + (1 | Site), family = "binomial", data = dat2)# Produce additive model
summary(M12b) # Not sig.
AICc(Mnull.3); AICc(M12);AICc(M12a);AICc(M12b)# Null model is best



AICc(Mnull.3); AICc(M10); AICc(M10a); AICc(M10b); AICc(M11); AICc(M12); AICc(M12a); AICc(M12b) # The additive vegetation model is best

cand.set <- list(Mnull.3,M10,M10a, M10b, M11, M12, M12a, M12b)

aictab(cand.set)


## Plots for thesis
dev.new(width=25, height=10, dpi=50, pointsize=35, noRStudioGD = T)
par(mfrow=c(1, 2), mgp=c(2.5,1,0), mar=c(4,4,2,2), oma=c(0,0,0,6), cex = 1, las = 1)

plot(prM10$Veg_dense[prM10$Type=="C"], prM10$fit[prM10$Type=="C"], pch=20, ylim=c(min(prM10$lci), max(prM10$uci)), xlab = "Veg. density", ylab = "Prob. of occurrence", type="l")
lines(prM10$Veg_dense[prM10$Type=="C"], prM10$lci[prM10$Type=="C"], lty=2)
lines(prM10$Veg_dense[prM10$Type=="C"], prM10$uci[prM10$Type=="C"], lty=2)
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$fit[prM10$Type=="R"], lty=1, col="red")
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$lci[prM10$Type=="R"], lty=2, col ="red")
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$uci[prM10$Type=="R"], lty=2, col ="red")

plot(prM10b$Veg_dense[prM10b$Type=="C"], prM10b$fit[prM10b$Type=="C"], pch=20, ylim=c(min(prM10b$lci), max(prM10b$uci)), xlab = "Veg. density (m)", ylab = "Probability of occurrence", type="l", las = 1)
lines(prM10b$Veg_dense[prM10b$Type=="C"], prM10b$lci[prM10b$Type=="C"], lty=2)
lines(prM10b$Veg_dense[prM10b$Type=="C"], prM10b$uci[prM10b$Type=="C"], lty=2)
lines(prM10b$Veg_dense[prM10b$Type=="R"], prM10b$fit[prM10b$Type=="R"], lty=1, col="red")
lines(prM10b$Veg_dense[prM10b$Type=="R"], prM10b$lci[prM10b$Type=="R"], lty=2, col ="red")
lines(prM10b$Veg_dense[prM10b$Type=="R"], prM10b$uci[prM10b$Type=="R"], lty=2, col ="red")

par(xpd=NA)
legend(x=1.5, y=1,legend =c("Culvert", "Road"), col = c("black", "red"), lty=1, lwd =1, cex = 1, bty ="n", text.width = 0.2)
par(xpd=F)


## For large species
Mnull.4 <- glmer(Large.pa ~ 1 + (1|Site), family = "binomial", data = dat2)
summary(Mnull.4) # Produce null model

#What is the Log Likelihood of this model?
logLik(Mnull.4)

M13 <- glmer(Large.pa ~ Type * Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M13) # Produce interactive model, not significant
#What is the Log Likelihood of this model?
logLik(M13)

M13a <- glmer(Large.pa ~ Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M13a) # Produce veg dense only model, not significant
#What is the Log Likelihood of this model?
logLik(M13a)

M13b <- glmer(Large.pa ~ Type + Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M13b) # Produce additive model, not significant
#What is the Log Likelihood of this model?
logLik(M13b)

# No influence of vegetation density on large species

M14 <- glmer(Large.pa ~ Type + (1|Site), family = "binomial", data = dat2)
summary(M14) # Produce the type only model, Not significant
# Culverts and road locations both positively influence the probability of observing an animal, although road cameras are more likely to observe an animal than culvert cameras. 
#What is the Log Likelihood of this model?
logLik(M14)


M15 <- glmer(Large.pa ~ Type * Clvt_sz + (1 | Site), family = "binomial", data = dat2)
summary(M15) # Significant negative influence of culvert cameras at small culverts on large species occurrences
#What is the Log Likelihood of this model?
logLik(M15)

M15a <- glmer(Large.pa ~ Clvt_sz + (1 | Site), family = "binomial", data = dat2)
summary(M15a) # Significant negative influence of small culverts on large species occurrences 
#What is the Log Likelihood of this model?
logLik(M15a)

# Predict from this model
ndM15a <- data.frame(Clvt_sz = as.factor(c(rep("small", 2), rep("large", 2))))
prM15a <- predictSE(mod = M15a, newdata = ndM15a, se.fit = T, type = "response")
prM15a <- data.frame(ndM15a, fit = round(prM15a$fit, 4), se = round(prM15a$se.fit, 4))
prM15a$lci <- prM15a$fit - (prM15a$se * 1.96)
prM15a$uci <- prM15a$fit + (prM15a$se * 1.96)

# Plot predictions from this model
plot(c(1,2), prM15a$fit[c(1,3)], pch = 20, ylim=c(0,1), xlim=c(0.5,2.5), xlab = "Culvert size", ylab = "Probability of occurrence", type = "p", xaxt="n")
axis(side=1, at=c(1,2), labels=c("small","large"))
arrows(c(1,2), prM15a$lci[c(1,3)], c(1,2), prM15a$uci[c(1,3)], length = 0.05, code=3, angle=90)
title(main = "(d) Large species", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main = 1)



M15b <- glmer(Large.pa ~ Type + Clvt_sz + (1 | Site), family = "binomial", data = dat2)# Produce additive model
summary(M15b) # Significant negative influence of small culverts on large species occurrences


AICc(Mnull.4); AICc(M13); AICc(M13a); AICc(M13b); AICc(M14); AICc(M15); AICc(M15a); AICc(M15b) # The null model is the best model

cand.set <- list(Mnull.4,M13,M13a, M13b, M14, M15, M15a, M15b)

aictab(cand.set)



## For small species
Mnull.5 <- glmer(Small.pa ~ 1 + (1|Site), family = "binomial", data = dat2)
summary(Mnull.5) # Produce null model

#What is the Log Likelihood of this model?
logLik(Mnull.5)

M16 <- glmer(Small.pa ~ Type * Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M16) # Produce interactive model, Significant
#What is the Log Likelihood of this model?
logLik(M16)

# Predict from the interactive model
ndM16 <- data.frame(Type = as.factor(c(rep("C", 50), rep("R", 50))), Veg_dense = seq(min(dat2$Veg_dense), max(dat2$Veg_dense), length.out = 50))
prM16 <- predictSE(mod = M16, newdata = ndM16, se.fit = T, type = "response")
prM16 <- data.frame(ndM16, fit = round(prM16$fit, 4), se = round(prM16$se.fit, 4))
prM16$lci <- prM16$fit - (prM16$se * 1.96)
prM16$uci <- prM16$fit + (prM16$se * 1.96)


# Plot predictions from this model
plot(prM16$Veg_dense[prM16$Type=="C"], prM16$fit[prM16$Type=="C"], pch=20, ylim=c(min(prM16$lci), max(prM16$uci)), xlab = "Vegetation density (m)", ylab = "Probability of occurrence", type = "l", las = 1)
lines(prM16$Veg_dense[prM16$Type=="C"], prM16$lci[prM16$Type=="C"], lty=2)
lines(prM16$Veg_dense[prM16$Type=="C"], prM16$uci[prM16$Type=="C"], lty=2)
lines(prM16$Veg_dense[prM16$Type=="R"], prM16$fit[prM16$Type=="R"], lty=1, col="red")
lines(prM16$Veg_dense[prM16$Type=="R"], prM16$lci[prM16$Type=="R"], lty=2, col="red")
lines(prM16$Veg_dense[prM16$Type=="R"], prM16$uci[prM16$Type=="R"], lty=2, col="red")
title(main = "Small species", outer = F, adj = 0, cex.main = 1, line = 0.3)


M16a <- glmer(Small.pa ~ Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M16a) # Produce veg dense only model, not significant
#What is the Log Likelihood of this model?
logLik(M16a)

M16b <- glmer(Small.pa ~ Type + Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M16b) # Produce additive model, significant
#What is the Log Likelihood of this model?
logLik(M16b)

# Predict from the additive model
ndM16b <- data.frame(Type = as.factor(c(rep("C", 50), rep("R", 50))), Veg_dense = seq(min(dat2$Veg_dense), max(dat2$Veg_dense), length.out = 50))
prM16b <- predictSE(mod = M16b, newdata = ndM16b, se.fit = T, type = "response")
prM16b <- data.frame(ndM16b, fit = round(prM16b$fit, 4), se = round(prM16b$se.fit, 4))
prM16b$lci <- prM16b$fit - (prM16b$se * 1.96)
prM16b$uci <- prM16b$fit + (prM16b$se * 1.96)

# Plot predictions from this model
plot(prM16b$Veg_dense[prM16b$Type=="C"], prM16b$fit[prM16b$Type=="C"], pch=20, ylim=c(min(prM16$lci), max(prM16b$uci)), xlab = "Vegetation density (m)", ylab = "Probability of occurrence", type = "l", las = 1)
lines(prM16b$Veg_dense[prM16b$Type=="C"], prM16b$lci[prM16b$Type=="C"], lty=2)
lines(prM16b$Veg_dense[prM16b$Type=="C"], prM16b$uci[prM16b$Type=="C"], lty=2)
lines(prM16b$Veg_dense[prM16b$Type=="R"], prM16b$fit[prM16b$Type=="R"], lty=1, col="red")
lines(prM16b$Veg_dense[prM16b$Type=="R"], prM16b$lci[prM16b$Type=="R"], lty=2, col="red")
lines(prM16b$Veg_dense[prM16b$Type=="R"], prM16b$uci[prM16b$Type=="R"], lty=2, col="red")
title(main = "Position + Veg. dense", outer = F, adj = 0, cex.main = 1, line = 0.3)



M17 <- glmer(Small.pa ~ Type + (1|Site), family = "binomial", data = dat2)
summary(M17) # Produce the type only model, Significant
# Culverts negatively influence the probability of observing an small animal and road locations positively influence the probability of observing an small animal. 
#What is the Log Likelihood of this model?
logLik(M17)

ndM17 <- data.frame(Type = as.factor(c(rep("C", 2), rep("R", 2))))
prM17 <- predictSE(mod = M17, newdata = ndM17, se.fit = T, type = "response")
prM17 <- data.frame(ndM17, fit = round(prM17$fit, 4), se = round(prM17$se.fit, 4))
prM17$lci <- prM17$fit - (prM17$se * 1.96)
prM17$uci <- prM17$fit + (prM17$se * 1.96)

# Plot predictions from this model
plot(c(1,2), prM17$fit[c(1,3)], pch = 20, ylim=c(0,1), xlim=c(0.5,2.5), xlab = "Camera position", ylab = "Probability of occurrence", type = "p", xaxt="n")
axis(side=1, at=c(1,2), labels=c("culvert","road"))
arrows(c(1,2), prM17$lci[c(1,3)], c(1,2), prM17$uci[c(1,3)], length = 0.05, code=3, angle=90)
title(main = "(c) Small species", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main = 1)

anova(Mnull.5, M17) # Significant
AICc(Mnull.5); AICc(M17)  # Model 17 is better than the null model

AICc(Mnull.5); AICc(M16); AICc(M16a); AICc(M16b); AICc(M17) # Model 17, the type only model is the best model.

M18 <- glmer(Small.pa ~ Type * Clvt_sz + (1 | Site), family = "binomial", data = dat2)
summary(M18) # Significant negative influence of culvert cameras at small culverts on small species occurrences
#What is the Log Likelihood of this model?
logLik(M18)

M18a <- glmer(Small.pa ~ Clvt_sz + (1 | Site), family = "binomial", data = dat2)
summary(M18a) # Not sig.
#What is the Log Likelihood of this model?
logLik(M18a)


M18b <- glmer(Small.pa ~ Type + Clvt_sz + (1 | Site), family = "binomial", data = dat2)# Produce additive model
summary(M18b) # Significant negative influence of culvert cameras at small culverts but a significant positive influence of road cameras at small culverts on small species observations

# Predict from this model 
ndM18b <- data.frame(Type = as.factor(c(rep("C", 2), rep("R", 2))), Clvt_sz = as.factor(c(rep("small", 2), rep("large", 2))))
prM18b <- predictSE(mod = M18b, newdata = ndM18b, se.fit = T, type = "response")
prM18b <- data.frame(ndM18b, fit = round(prM18b$fit, 4), se = round(prM18b$se.fit, 4))
prM18b$lci <- prM18b$fit - (prM18b$se * 1.96)
prM18b$uci <- prM18b$fit + (prM18b$se * 1.96)


# Plot predictions from this model
plot(prM18b$Clvt_sz[prM18b$Type=="C"], prM18b$fit[prM18b$Type=="C"], pch=20, ylim=c(min(prM18b$lci), max(prM18b$uci)), xlab = "Culvert size", ylab = "Prob. of occurrence", type = "b")
points(prM18b$Clvt_sz[prM18b$Type=="C"], prM18b$lci[prM18b$Type=="C"], lty=2)
points(prM18b$Clvt_sz[prM18b$Type=="C"], prM18b$uci[prM18b$Type=="C"], lty=2)
points(prM18b$Clvt_sz[prM18b$Type=="R"], prM18b$fit[prM18b$Type=="R"], lty=1, col="red")
points(prM18b$Clvt_sz[prM18b$Type=="R"], prM18b$lci[prM18b$Type=="R"], lty=2, col="red")
points(prM18b$Clvt_sz[prM18b$Type=="R"], prM18b$uci[prM18b$Type=="R"], lty=2, col="red")



AICc(Mnull.5); AICc(M16); AICc(M16a); AICc(M16b); AICc(M17); AICc(M18); AICc(M18a); AICc(M18b) # M17, the type only model is best.


cand.set <- list(Mnull.5,M16,M16a, M16b, M17, M18, M18a, M18b)

aictab(cand.set)





## Plots for paper - ensure that the y limit is set as 0,1. As these graphs are the probability of occurrence and probabilities are bounded between 0 and 1, we want this to be consistent and not have different y minimums and maximums


dev.new(width=12, height=10, dpi=80, pointsize=18, noRStudioGD = T)
par(mfrow=c(2,2), mgp=c(2.5,1,0), mar=c(4,4,2,2), oma=c(0,0,0,6), cex = 1, las =1)

plot(prM10b$Veg_dense[prM10b$Type=="C"], prM10b$fit[prM10b$Type=="C"], pch=20, ylim=c(0,1), xlab = "Vegetation density (m)", ylab = "Probability of occurrence", type = "l", las = 1)
lines(prM10b$Veg_dense[prM10b$Type=="C"], prM10b$lci[prM10b$Type=="C"], lty = 2)
lines(prM10b$Veg_dense[prM10b$Type=="C"], prM10b$uci[prM10b$Type=="C"], lty = 2)
lines(prM10b$Veg_dense[prM10b$Type=="R"], prM10b$fit[prM10b$Type=="R"], lty = 1, col="red")
lines(prM10b$Veg_dense[prM10b$Type=="R"], prM10b$lci[prM10b$Type=="R"], lty = 2, col = "red")
lines(prM10b$Veg_dense[prM10b$Type=="R"], prM10b$uci[prM10b$Type=="R"], lty = 2, col = "red")
title(main = "(a) All animals", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main = 1)


plot(prM4b$Veg_dense[prM4b$Type=="C"], prM4b$fit[prM4b$Type=="C"], pch=20, ylim=c(0,1), xlab = "Vegetation density (m)", ylab = "Probability of occurrence", type = "l", las = 1)
lines(prM4b$Veg_dense[prM4b$Type=="C"], prM4b$lci[prM4b$Type=="C"], lty = 2)
lines(prM4b$Veg_dense[prM4b$Type=="C"], prM4b$uci[prM4b$Type=="C"], lty = 2)
lines(prM4b$Veg_dense[prM4b$Type=="R"], prM4b$fit[prM4b$Type=="R"], lty = 1, col = "red")
lines(prM4b$Veg_dense[prM4b$Type=="R"], prM4b$lci[prM4b$Type=="R"], lty = 2, col = "red")
lines(prM4b$Veg_dense[prM4b$Type=="R"], prM4b$uci[prM4b$Type=="R"], lty = 2, col = "red")
title(main = "(b) Native species", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main = 1)



plot(c(1,2), prM17$fit[c(1,3)], pch = 20, ylim=c(0,1), xlim=c(0.5,2.5), xlab = "Camera position", ylab = "Probability of occurrence", type = "p", xaxt="n")
axis(side=1, at=c(1,2), labels=c("culvert","road"))
arrows(c(1,2), prM17$lci[c(1,3)], c(1,2), prM17$uci[c(1,3)], length = 0.05, code=3, angle=90)
title(main = "(c) Small species", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main = 1)

plot(c(1,2), prM15a$fit[c(1,3)], pch = 20, ylim=c(0,1), xlim=c(0.5,2.5), xlab = "Culvert size", ylab = "Probability of occurrence", type = "p", xaxt="n")
axis(side=1, at=c(1,2), labels=c("small","large"))
arrows(c(1,2), prM15a$lci[c(1,3)], c(1,2), prM15a$uci[c(1,3)], length = 0.05, code=3, angle=90)
title(main = "(d) Large species", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main = 1)


par(xpd=NA)
legend(x=2.8, y=2.8, legend=c("Culvert", "Road"), col=c("black", "red"), lty=1, lwd=0.5, cex=1, bty="n", text.width=0.2, title = "Camera position")
par(xpd=F)


# Plots for supplementary material - interactive models that were better than the null model and within AICc2 of the additive model. 
dev.new(width=12, height=10, dpi=80, pointsize=18, noRStudioGD = T)
par(mfrow=c(2,2), mgp=c(2.5,1,0), mar=c(4,4,2,2), oma=c(0,0,0,6), cex = 1, las =1)


plot(prM10$Veg_dense[prM10$Type=="C"], prM10$fit[prM10$Type=="C"], pch=20, ylim=c(0, 1), xlab = "Vegetation density", ylab = "Probability of occurrence", type="l")
lines(prM10$Veg_dense[prM10$Type=="C"], prM10$lci[prM10$Type=="C"], lty=2)
lines(prM10$Veg_dense[prM10$Type=="C"], prM10$uci[prM10$Type=="C"], lty=2)
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$fit[prM10$Type=="R"], lty=1, col="red")
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$lci[prM10$Type=="R"], lty=2, col ="red")
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$uci[prM10$Type=="R"], lty=2, col ="red")
title(main = "(a) All animals", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main = 1)


plot(prM4$Veg_dense[prM4$Type=="C"], prM4$fit[prM4$Type=="C"], pch=20, ylim=c(0, 1), xlab = "Vegetation density", ylab = "Probability of occurrence", type="l")
lines(prM4$Veg_dense[prM4$Type=="C"], prM4$lci[prM4$Type=="C"], lty=2)
lines(prM4$Veg_dense[prM4$Type=="C"], prM4$uci[prM4$Type=="C"], lty=2)
lines(prM4$Veg_dense[prM4$Type=="R"], prM4$fit[prM4$Type=="R"], lty=1, col="red")
lines(prM4$Veg_dense[prM4$Type=="R"], prM4$lci[prM4$Type=="R"], lty=2, col ="red")
lines(prM4$Veg_dense[prM4$Type=="R"], prM4$uci[prM4$Type=="R"], lty=2, col ="red")
title(main = "(b) Native species", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main = 1)


plot(prM16$Veg_dense[prM16$Type=="C"], prM16$fit[prM16$Type=="C"], pch=20, ylim=c(0, 1), xlab = "Vegetation density (m)", ylab = "Probability of occurrence", type = "l", las = 1)
lines(prM16$Veg_dense[prM16$Type=="C"], prM16$lci[prM16$Type=="C"], lty=2)
lines(prM16$Veg_dense[prM16$Type=="C"], prM16$uci[prM16$Type=="C"], lty=2)
lines(prM16$Veg_dense[prM16$Type=="R"], prM16$fit[prM16$Type=="R"], lty=1, col="red")
lines(prM16$Veg_dense[prM16$Type=="R"], prM16$lci[prM16$Type=="R"], lty=2, col="red")
lines(prM16$Veg_dense[prM16$Type=="R"], prM16$uci[prM16$Type=="R"], lty=2, col="red")
title(main = "(c) Small species", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main = 1)


par(xpd=NA)
legend(x=3.6, y=1, legend=c("Culvert", "Road"), col=c("black", "red"), lty=1, lwd=0.5, cex=1, bty="n", text.width=0.2)
par(xpd=F)


plot(prM16b$Veg_dense[prM16b$Type=="C"], prM16b$fit[prM16b$Type=="C"], pch = 20, ylim=c(0, 1), xlab = "Vegetation density (m)", ylab = "Probability of occurrence", type = "l", las = 1)
lines(prM16b$Veg_dense[prM16b$Type=="C"], prM16b$lci[prM16b$Type=="C"], lty = 2)
lines(prM16b$Veg_dense[prM16b$Type=="C"], prM16b$uci[prM16b$Type=="C"], lty = 2)
lines(prM16b$Veg_dense[prM16b$Type=="R"], prM16b$fit[prM16b$Type=="R"], lty = 1, col = "red")
lines(prM16b$Veg_dense[prM16b$Type=="R"], prM16b$lci[prM16b$Type=="R"], lty = 2, col = "red")
lines(prM16b$Veg_dense[prM16b$Type=="R"], prM16b$uci[prM16b$Type=="R"], lty = 2, col = "red")
title(main = "(d) Small species", outer = F, adj =0, cex.main = 1, line = 0.3, font.main = 1)













# Now lets look at how behaviours influence culvert use
# Explore behavioural data

head(beh); dim(beh) # Look at the first few rows of the data, there are 4714 individual triggers
sum(table(beh$Behaviour)) # There are 4714 rows of data
range(beh$Anim_num) # There are 1164 individual animals
table(beh$Behaviour) # There are 73 culvert crossing occurrences, 2865 foraging occurrences, 505 road crossing occurences, and 1270 sitting/standing still occurrences in the behaviour data.
sum(2865+1270) # What is the sum of the foraging and sitting still behaviours, 4135 occurences of 4714 individual triggers. 



# HOW MANY INDEPENDENT OBSERVATIONS ARE THERE FOR ALL BEHAVIOURS?


hist(table(beh$Anim_num)) # How often was the same individual observed multiple times? Mostly animals were only observed once but there were occasions when animals were observed multiple times. 
beh[which(unique(beh$Anim_num) >1), ] # We can see that some of the repeated observations are occurring on different days/hours/display different behaviours

table(beh$Anim_num) # Are there repeated observations? For animals that were observed either crossing the road or using the culvert which are contained within beh, how many have repeat observations?

str(beh) # Check the structure of this dataframe to refresh the memory
manim <- as.numeric(names(table(beh$Anim_num)[which(table(beh$Anim_num)>1)])) # Create a value containing the data with multiple animal occurrences
mdat <- beh[which(beh$Anim_num %in% manim),] # Create a data frame containing the data with multiple animal occurrences 
mdat[which(mdat$Anim_num == manim[1]),] # Look at the first individual of the multiple data occurrences data frame to see what is occurring, individual observed over multiple days and hours or with different behaviours. Data needs to be cleaned up further. 


## Explore the data
length(manim) # There are 696 repeated observations. 
length(unique(beh$Anim_num)) # Although there are 4714 observations there are only 1164 unique animals. There are more observations than we want/need. 

# How do we deal with the repeated observations? Do we take the first observation for each day or hour? No, we would loose data by filtering the data by the hour or day that it was observed in as sometimes individuals displayed different behaviours depending on the hour of the day, this would mean we could loose data for culvert crossings which are already quite low. 
# Therefore, if there is a repeated observation within same hour we remove it, but we will keep in mind the behaviour. So we want 1 observation per hour for each behaviour per individual animal. 


beh$anim_hr<-paste(beh$Anim_num,beh$Behaviour, beh$Day, beh$Month, beh$Hour, sep="_") # Create a value within the data that contains all the information needed to simply distinguish between the day, hour and behaviour of each animal
head(beh); dim(beh) # Check how this looks
unique(beh$anim_hr) # Check how this looks, not in order


# We want to order the data so that when they are subset, we can take the first observation per hour
beh2 <- beh[order(beh$Anim_num,beh$Behaviour, beh$Day, beh$Month, beh$Hour, beh$Min, beh$Sec),]
rownames(beh2)<-1:nrow(beh2)
beh2[beh2$Anim_num==42,] # Lets have a look at one of the individual animals and ensure that it is ordered correctly. 

# To begin with, we are going to be removing multiple observations from the dataset. Some animals are known to display RX on both culvert and road cameras with the culvert camera recroded first. We only want to have road crossings recorded on road cameras we need to delete any road crossings on culvert cameras for animals 35, 108, 242, and 332 lets print a table here so we can implement this. 
library(writexl)

write_xlsx(beh2, "beh2")

beh2.1 <- read.table("beh2.1.txt", header = T, stringsAsFactors = T)


# Create a new data frame with only the first observation for each animal per hour per behaviour type:
beh3 <- beh2.1[-which(duplicated(beh2.1$anim_hr)),]
beh3 <- droplevels(beh3)
rownames(beh3) <- 1:nrow(beh3)
head(beh3); dim(beh3)
table(beh3$Behaviour) # We now only have 33 culvert crossing observations, and 370 road crossing observations. We have successfully removed duplicates of the same behaviour of animals within the same hour. Although we should also think about whether an individual animal was observed at 10:52 and then again at 11:02, both of these observations would be kept as they are in seperate hours but this data cannot be considered independent. Need to figure out how to deal with this. 
table(beh3$Behaviour, beh3$Species)

beh3$behav<-ifelse(beh3$Behaviour=="RX", 0, 1) # We want to assign numeric values to the behaviour for the means of further analysis, culvert crossing = 0, road crossing = 1.  



library(writexl) # Install excel writing package
write_xlsx(beh3, "beh3.1") # Create spreadsheet for excel data filtering steps

# Import the data back into R
beh4.1 <- read.table("beh4.1.txt", header = T, stringsAsFactors = T) # Load in the new behavioural data
head(beh4.1, 10); dim(beh4.1) # Check this has worked, there are now 1671 individual independent triggers in the data
head(beh3, 10); dim(beh3) # We can see that the trigger in row 4 has now been removed as it was not independent, there were 402 individual triggers in the data although, 6 of these triggers were not independent and were removed. 
str(beh4.1) # Double check everything has been coded and imported correctly. 
dim(beh4.1); head(beh4.1) # There are 1671 observations in beh4.1
table(beh4.1$Behaviour) # There are now only 32 culvert crossings, 948 foraging, 366 road crossings, and 330 sitting still occurences 
range(beh4.1$Site) # There are still 30 sites




# HOW MANY INDEPENDENT OBSERVATIONS ARE THERE FOR ROAD CROSSING BEHAVIOURS?
# We will overwrite the above as we only want to know about crossing behaviour for this dataset


## Investigate the behavioural data
beh2 <- beh[c(which(beh$Behaviour == "CC"), which(beh$Behaviour == "RX")), ] # Create a new dataframe which contains data only for the culvert crossing and road crossing behaviours
str(beh) # Look at the structure of the original behavioural data
str(beh2) # Look at the structure of the new data just for CC and RX, the Behaviour variable still has 4 levels, check what is going on
unique(beh2$Behaviour) # Although the data only contains the data for culvert crossings or road crossings, all the levels from the original data are still present
beh2 <- droplevels(beh2) # Drop the levels for the behaviour variable that are not present within the dataframe
head(beh2) # Check how the data looks, the row numbers are not consistent 1-10 etc. due to the removal of data that was not categorised as CC or RX
rownames(beh2) <- 1: nrow(beh2) # Change the row names so that they are consistently numbered
table(beh2$Behaviour) # How many animals are observed for each behaviour in this dataframe?
table(beh2$Behaviour, beh2$Species)
table(beh$Behaviour, beh$Species)


# Discovery 2 - There are more animals crossing over the surface of the road. Although, we must consider that some animals captured on the culvert cameras were observed crossing the road and that there was double the trapping effort on culverts versus roads, as culverts had 4 cameras, and roads only 2 cameras. 
# How to deal with the difference in trap effort between road and culvert cameras, removing repeated obs. of animals?

hist(table(beh2$Anim_num)) # How often was the same individual observed multiple times? Mostly animals were only observed once but there were occasions when animals were observed multiple times. 
beh2[which(unique(beh2$Anim_num) >1), ] # We can see that some of the repeated observations are occurring on different days/hours/display different behaviours

table(beh2$Anim_num) # Are there repeated observations? For animals that were observed either crossing the road or using the culvert which are contained within beh2, how many have repeat observations. 

str(beh2) # Check the structure of this dataframe to refresh the memory
manim <- as.numeric(names(table(beh2$Anim_num)[which(table(beh2$Anim_num)>1)])) # Create a value containing the data with multiple animal occurrences
mdat <- beh2[which(beh2$Anim_num %in% manim),] # Create a data frame containing the data with multiple animal occurrences 
mdat[which(mdat$Anim_num == manim[1]),] # Look at the first individual of the multiple data occurrences data frame to see what is occurring, individual observed over multiple days and hours or with different behaviours. Data needs to be cleaned up further. 


## Explore the data
length(manim) # There are 110 repeated observations. 
dim(beh2) # There are 579 observations of road crossing and culvert crossing behaviours, with 110 repeated observations. 
length(unique(beh2$Anim_num)) # Although there are 578 observations there are only 384 unique animals. There are more observations than we want/need. 

# How do we deal with the repeated observations? Do we take the first observation for each day or hour? No, we would loose data by filtering the data by the hour or day that it was observed in as sometimes individuals displayed different behaviours depending on the hour of the day, this would mean we could loose data for culvert crossings which are already quite low. 
# Therefore, if there is a repeated observation within same hour we remove it, but we will keep in mind the behaviour. So we want 1 observation per hour for each behaviour per individual animal. 


beh2$anim_hr<-paste(beh2$Anim_num,beh2$Behaviour, beh2$Day, beh2$Month, beh2$Hour, sep="_") # Create a value within the data that contains all the information needed to simply distinguish between the day, hour and behaviour of each animal
head(beh2); dim(beh2) # Check how this looks
unique(beh2$anim_hr) # Check how this looks, not in order


# We want to order the data so that when they are subset, we can take the first observation per hour
beh2 <- beh2[order(beh2$Anim_num,beh2$Behaviour, beh2$Day, beh2$Month, beh2$Hour, beh2$Min, beh2$Sec),]
rownames(beh2)<-1:nrow(beh2)
beh2[beh2$Anim_num==25,] # Lets have a look at one of the individual animals and ensure that it is ordered correctly. 

# To begin with, we are going to be removing multiple observations from the dataset. Some animals are known to display RX on both culvert and road cameras with the culvert camera recorded first. We only want to have road crossings recorded on road cameras we need to delete any road crossings on culvert cameras for animals 35, 108, 242, and 332 lets print a table here so we can implement this. We did this before so read in the data and use from here onwards


write_xlsx(beh2, "beh2a")

beh2a <- read.table("beh2a.txt", header = T, stringsAsFactors = T)



# Create a new data frame with only the first observation for each animal per hour per behaviour type:
beh3 <- beh2a[-which(duplicated(beh2a$anim_hr)),]
beh3 <- droplevels(beh3)
rownames(beh3) <- 1:nrow(beh3)
head(beh3); dim(beh3)
table(beh3$Behaviour) # We now only have 33 culvert crossing observations, and 370 road crossing observations. We have successfully removed duplicates of the same behaviour of animals within the same hour. Although we should also think about whether an individual animal was observed at 10:52 and then again at 11:02, both of these observations would be kept as they are in seperate hours but this data cannot be considered independent. Need to figure out how to deal with this. 
table(beh3$Behaviour, beh3$Species)

beh3$behav<-ifelse(beh3$Behaviour=="RX", 0, 1) # We want to assign numeric values to the behaviour for the means of further analysis, culvert crossing = 0, road crossing = 1.  

# We know that the occurrences are still not independent, a cow is seen at 11:21 and then again at 12:07 on the same day, we want there to be an interval of an hour between occurrences so we need to fix this. Need to install package to write data into excel file and will edit the data through excel
# install.packages("writexl")
write_xlsx(beh3, "beh3") # Create spreadsheet for excel data filtering steps

# Import the data back into R
beh4 <- read.table("beh4.txt", header = T, stringsAsFactors = T) # Load in the new behavioural data
head(beh4, 10); dim(beh4) # Check this has worked, there are now 397 individual independent triggers in the data
head(beh3, 10); dim(beh3) # We can see that the trigger in row 4 has now been removed as it was not independent, there were 403 individual triggers in the data although, 6 of these triggers were not independent and were removed. 
str(beh4) # Double check everything has been coded and imported correctly. 
dim(beh4); head(beh4) # There are 397 observations in beh4
table(beh4$Behaviour) # There are 32 culvert crossings and 365 road crossings
range(beh4$Site) # There are now only 26 sites

# To work with site characteristics data better, need to create summaries of the vegetation data (average the vegetation height for each camera point). Want to determine if the probability of spotting individuals was influenced by vegetation height or density. Want to add extra columns to beh3 for road type (character), veg height (numeric), veg dense (character). Also want to determine the influence of road type on crossing behaviour. 

head(rc, 2); dim(rc) # Look at the first two rows of the data, there are 30 sites so there are not duplication 
table(rc$Rd_typ) # There are 24 sites on major roads, and 6 sites on minor roads
head(beh4); dim(beh4) # Look at beh4, as we want to add extra columns to this dataset

unique(rc$Site) # What are the variables of the column Site in the site characteristics dataset?
unique(beh4$Site) # What are the variables of the column Site in the beh4 dataset? Some of the sights are not included in this data as there were no occurrences of road crossings at these sites. 

str(rc$Rd_typ) # Check how rd type looks
rc$Rd_typ <- factor(rc$Rd_typ, levels = c("minor", "major")) # Needed to reorder how it was shown


rc2<- rc[,c("Site", "Rd_typ")] # Create a new dataset that only contains the information for site and road type
head(rc2); dim(rc2) # Check this worked, there are only 30 sites
which(duplicated(rc2$Site)) # Although this is correct, double check that there are no duplications just in case
beh4 <- merge(beh4, rc2, by = "Site", all.x = T, all.y = F) # Merge the data in the new site characteristics dataset with beh4 according to Site. 
head(beh4); dim(beh4) # Check that this has worked, all looks good. 
str(beh4$Rd_typ) # Check that rd type still looks right

# Need to replace NA with 0 in road characteristics data
rc[is.na(rc)] <- 0 # Replace NA with 0
head(rc) # Check that this worked

# Averages of vegetation data per camera point
rc$C1 <- rowMeans(rc[ , c(14, 15, 16)], na.rm = TRUE )
rc$C2 <- rowMeans(rc[ , c(18, 19, 20)], na.rm = TRUE)
rc$C3 <- rowMeans(rc[ , c(22, 23, 24)], na.rm = TRUE)
rc$C4 <- rowMeans(rc[ , c(26, 27, 28)], na.rm = TRUE)
rc$R1 <- rowMeans(rc[ , c(30, 31, 32)], na.rm = TRUE)
rc$R2 <- rowMeans(rc[ , c(34, 35, 36)], na.rm = TRUE)
head(rc) # Ensure that this has worked

# Create a new data frame with this data in it
rc3 <- rc[,c("Site", "C1", "C2", "C3", "C4", "R1", "R2")] 
head(rc3); dim(rc3) # Check this worked, there are only 30 sites

rc4 <- gather(rc3, key = "Cam_pt", value = "Veg_dense", "C1", "C2", "C3", "C4", "R1", "R2")
rc4 # Take a look and ensure this is correct
head(rc4); dim(rc4) # There are 180 rows of data (6*30 = 180), and 3 columns (Site, Camera point, and Vegetation density)
rc4$Veg_dense <- sort(rc4$Veg_dense)
beh4 <- merge(beh4, rc4, by = c("Site", "Cam_pt"), all.x = T, all.y = F) # Merge the data in the new site characteristics dataset with beh3 according to Site and Camera point. 
head(beh4) # Check that this has worked 


# Take just the vegetation cover data from the road characteristics data frame and add it to beh4

rc5 <- rc[,c("Site", "Veg_cov_C1","Veg_cov_C2","Veg_cov_C3", "Veg_cov_C4", "Veg_cov_R1", "Veg_cov_R2")] # Create a new dataset that only contains the information for site and vegetation 
rc5 # Check that this has worked
# To be able to stack the columns we need the package tidyr
library(tidyr)
rc6 <- gather(rc5, key = "Cam_pt", value = "Veg_cov", "Veg_cov_C1", "Veg_cov_C2", "Veg_cov_C3", "Veg_cov_C4", "Veg_cov_R1", "Veg_cov_R2") # Stack the vegetation cover columns on top of each other
rc6 # Check that this worked

# This has worked although we need to change the values in the column Cam_pt so that they can be used to merge by camera point with beh4

rc6$Cam_pt[1:30] <- "C1"
rc6$Cam_pt[31:60] <- "C2"
rc6$Cam_pt[61:90] <- "C3"
rc6$Cam_pt[91:120] <- "C4"
rc6$Cam_pt[121:150] <- "R1"
rc6$Cam_pt[151:180] <- "R2"
rc6 # Check that this has worked, all looks good can now merge with beh4
rc6$Veg_cov <- factor(rc6$Veg_cov, levels = c("none", "low", "medium", "high")) # Needed to reorder veg cov
str(rc6$Veg_cov) # Check that this has worked

beh4 <- merge(beh4, rc6, by = c("Site", "Cam_pt"), all.x = T , all.y = F) # Merge the data in the new site characteristics dataset with beh3 according to Site and Camera point.
head(beh4); dim(beh4) # Check that this has worked, no new rows have been added also.  

# We now have all the road characteristics data we wanted added to beh4

# EXPLORE THE BEHAVIOURAL DATA
t1 <- table(beh4$Behaviour, beh4$Species) # Shows the number of crossings for each species per behaviour.  
t1[1,]/colSums(t1) # The proportion of each species using culverts to cross roads. 7% of Kangaroos used culverts to cross roads, all and lace monitors used culverts to cross roads, 50% of cows used culverts to cross roads, only 2% of Whiptail wallabies used culverts to cross roads. Although, this is not taking into account trap effort. 
t1[1,]/sum(t1[1,], (t1[2,]*2)) #The proportion of each species using culverts to cross roads standardised for trapping effort. In reality 3-7% of Kangaroos used culverts to cross roads etc. 

# Model the data with behaviour as a function of species and type
mnull <- glm(behav ~ 1 + 1, family = "binomial", data = beh4)
summary(mnull) # Produce the null model

m11 <- glm(behav ~ Species + Type, family = "binomial", data = beh4)
summary(m11) # Doesn't look any good, need to go about this differently 

beh4$Species == "Kangaroo" # We want to look at just Kangaroo alone as they had the most culvert crossings of a single species 
beh4$Kang <- ifelse(beh4$Species == "Kangaroo", 1, 0) # Create a new column in the behavioural data which assigns 0 if the individual is not a Kangaroo and 1 if the individual is a Kangaroo
head(beh4) # Check that this worked

m12 <- glm(behav ~ Kang + Type, family = "binomial", data = beh4) # Model the behaviour of Kangaroos as a function of camera type
summary(m12) # There is a slightly lower chance of using the culvert to cross the road if you are a Kangaroo. 

# Look at t1 again
t1
all_other <- t1[,-which(colnames(t1) == "Kangaroo")] # We want to model all other species that are not Kangaroo separately as there are too low numbers of these species to model them separately
rowSums(all_other)[1]/sum(all_other) # So there are 10 culvert crossings, a larger proportion of these culvert crossings are due to all other animals than kangaroos. 
summary(m12) # There is a slightly lower chance of using the culvert if the individual is a kangaroo, more likely to use culvert if the individual is not a kangaroo, this may be influenced culvert and animal size. Need to investigate this further

m13 <- glm(behav ~ Kang * Type, family = "binomial", data = beh4) # Is the probability of a kangaroo using the culvert to cross the road dependent on if it was a culvert or road camera? 
summary(m13) # Not significant 
# Check which of the models produced fit the data the best. 
AICc(mnull); AICc(m10); AICc(m12); AICc(m13) # The model that fits the data best is still model 1, although model 1 and model 2 are quite similar



rc7 <- rc[,c("Site", "Length", "Clvt_wdt", "Clvt_ht", "Clvt_wdt")] # Create a new data frame with culvert variables
beh5 <- merge(beh4, rc7, by = "Site", all.x = T, all.y = F) # Merge this with beh4 but create a new data set 

# We want to discard any observations of road crossings on culvert cameras
dir() # Get the directory so we can access functions that are saved in separate files
source("~/Desktop/Github_2/Roadinfra-Wildlife/Functions/Felicity_Functions.R") # Load the functions to this script 
tidy.df
head(beh5); dim(beh5) # Check the head of the data
length(unique(beh2$Site)) # There are 14 sites in this dataset
beh6 <- beh5[-which(beh5$Behaviour == "RX" & beh5$Type == "C"),] # Remove RX on culvert cameras
beh6 <- tidy.df(beh6) # Clean it up
head(beh6); dim(beh6) # Check how it looks
row.names(beh6) # Redo row names

str(beh6) # Check everything is ok
length(unique(beh6$Site)) # How many sites are included in the beh6 data? 12
length(unique(beh5$Site)) # How many sites are included in the beh5 data? 14


table(beh6$Behaviour)
# Lets go back and look at the data again. 
table(beh6$Species, beh6$Behaviour) # How many animals were observed performing each road crossing behaviour? 



# This isn't giving us the results we want, we want to take it down to site level and compare behaviour at the site specific level. Go through hypothesis and ensure the new structure will answer these questions. Want to analyse the data as all animals, kangaroos only, exotic only, and native only. We will then need to standardise for trapping effort, by doubling the number of road crossings. 


head(rc3, 3); dim(rc3) # What is the structure of the road characteristics data

rc10 <- data.frame(Site = rc3$Site, VegC = apply(rc3[,2:5], 1, mean), VegR = apply(rc3[,6:7], 1, mean)) # Produce the average vegetation density for culvert cameras and road cameras
head(rc10); dim(rc10) # Check that this has worked
rc10$Vegdif <- rc10$VegC - rc10$VegR # Calculate the difference of vegetation density between the culvert and road cameras
rc10$VegC <- sort(rc10$VegC) # Reorder
rc10$VegR <- sort(rc10$VegR) # Reorder
rc10Vegdif <- sort(rc10$Vegdif) # Reorder

# If vegdif is positive then there is more vegetation at the culvert, if it is negative then there is more vegetation at the road. 


head(rc10); dim(rc10) # If it is a negative value then there is higher vegetation density at the road than at the culvert, if it is a positive value than there is a higher vegetation density at the culvert than the road.



# For analysis of the influence of vegetation on crossing behaviour, I really need this to be the same as previously in the occurence data, as an average vegetation density. In this case the average vegetation density is at the site level rather than the camera level.

rc10$Vegmean <- (rc10$VegC + rc10$VegR)/2
head(rc10) # Check that this has worked, all looks correct



rc8 <- rc[,c("Site", "Rd_typ", "Cam_typ", "Clvt_sz", "Clvt_wdt", "Clvt_ht", "Length", "Construction")]
beh.site <- rc8[, c("Site", "Rd_typ", "Cam_typ", "Clvt_sz", "Clvt_wdt", "Clvt_ht", "Length", "Construction")] # Create a site specific data set containing only the information needed to answer the hypotheses
head(beh.site); dim(beh.site) # Check how this looks
beh.site$Rd_typ <- factor(rc$Rd_typ, levels = c("minor", "major")) # Needed to be reordered
str(beh.site$Rd_typ) # Make sure this looks right
str(beh.site$Clvt_sz) # Make sure this look right
beh.site$RC <- 0 # Create a new column for road crossings, and assign 0 to all
beh.site$RC[which(beh.site$Site %in% unique(beh6$Site))] <- 1 # If there was a road crossing either a culvert or road crossing at a site assign a 1, all other sites that had no crossings will remain 0
head(beh.site); dim(beh.site) # Check how this looks
beh.site <- merge(beh.site, rc10, by = "Site", all.x=T, all.y=F) # Merge the two data sets
head(beh.site); dim(beh.site) # Check how this looks

# Look at the openness ratio as a way to also look at the culvert size instead
head(beh.site) # look at the head of the data

Open <- data.frame(Openness = beh.site$Clvt_wdt*beh.site$Clvt_ht/beh.site$Length) # Create the openness ratio for each site
Open$Site <- 1:30 # Assign the sites to the dataframe
beh.site <- merge(beh.site, Open, by = "Site", all.x = T, all.y = F) # Merge this new column with the beh.site data set
head(beh.site) # Check this has worked


table(beh6$Site, beh6$Behaviour) # How many animals were observed performing each road crossing behaviour?
RC <- data.frame(Site = as.numeric(dimnames(table(beh6$Site, beh6$Behaviour))[[1]]), CC = table(beh6$Site, beh6$Behaviour)[,1], RX = table(beh6$Site, beh6$Behaviour)[,2]) # Create a new data frame containing the information for the number of each road crossing behaviour per site. Merge this with beh.site, and change NAs to 0 if there are any
# Then calculate the proportion of crossings that were under roads, CC/sum(CC+RX), then repeat same process for other groups, use same models below with this new dataset 
head(RC) # What does the Road crossing data look like

# We want to rename these columns to say All.CC or All.RX, so that we know this information is for all animals
names(RC)[names(RC) == "CC"] <- "All.CC"
names(RC)[names(RC) == "RX"] <- "All.RX"


# Double the number of All.RX to standardise for trapping effort
RC$All.RX <- round(RC$All.RX*2,)
RC$All.RX # Has this worked, yes.

beh.site <- merge(beh.site, RC, by = "Site", all.x = T, all.y = F) # Merge this with beh.site
beh.site[is.na(beh.site)] <- 0 # Replace NA with 0
head(beh.site); dim(beh.site) # Check how this looks
beh.site$Prop_RC_clvt <- beh.site[,"All.CC"]/rowSums(beh.site[,c("All.CC","All.RX")]) # Calculate the proportion of road crossings that were under the road - figure out why this isn't working,, want proportion for all other models
head(beh.site,3); dim(beh.site) # Check that this has worked.




# We want to do a two part analysis, the first part to determine what if anything effects any sort of road crossings, the second to look at each site and determine what effected road crossings.  0 = all RC were over top of road. 
head(beh6) # Check the head of the data
beh.site$Prop_RC_clvt[beh.site$RC == 0] <- NA # For all animals when site had no RC at all it is now NA, it is 0 if it was a RX over road surface, and 1 if through a culvert. 
head(beh.site); dim(beh.site) # Check that this has worked


behKang <-beh6[which(beh6$Species %in% "Kangaroo"),] # Create the road crossings data for Kangaroos, first finding where they were observed
head(behKang) # See how this looks 
behKang <- tidy.df(data.set = behKang) # Clean up
RCKang <- data.frame(Site = as.numeric(dimnames(table(behKang$Site, behKang$Behaviour))[[1]]), CC = table(behKang$Site, behKang$Behaviour)[,1], RX = table(behKang$Site, behKang$Behaviour)[,2]) # Create road crossing information
head(RCKang); dim(RCKang) # Check how this looks
# Rename columns
names(RCKang)[names(RCKang) == "CC"] <- "Kangaroo.CC"
names(RCKang)[names(RCKang) == "RX"] <- "Kangaroo.RX"
# Double the number of road crossings to standardise for trapping effort
RCKang$Kangaroo.RX <- round(RCKang$Kangaroo.RX*2, 0)


beh.site <- merge(beh.site, RCKang, by = "Site", all.x = T, all.y = F)  # Merge with beh.site
head(beh.site) # Check this worked

beh.site$Prop_Kang_RC_clvt <- beh.site[,"Kangaroo.CC"]/rowSums(beh.site[,c("Kangaroo.CC","Kangaroo.RX")]) # Calculate the propensity for a road crossing 
 

behExotic <- beh6[which(beh6$Species %in% c("Cat", "Cow", "Dog", "Fox", "Hare")),] # Create the road crossings data for exotics, first finding where they were observed
head(behExotic) # See how this looks
behExotic <- tidy.df(data.set = behExotic) # Clean up
RCExotic <- data.frame(Site = as.numeric(dimnames(table(behExotic$Site, behExotic$Behaviour))[[1]]), CC = table(behExotic$Site, behExotic$Behaviour)[,1], RX = table(behExotic$Site, behExotic$Behaviour)[,2]) # Create road crossing information
head(RCExotic); dim(RCExotic) # Check how this looks
# Rename columns
names(RCExotic)[names(RCExotic) == "CC"] <- "Exotic.CC"
names(RCExotic)[names(RCExotic) == "RX"] <- "Exotic.RX"
# Double the number of road crossings
RCExotic$Exotic.RX <- round(RCExotic$Exotic.RX*2,0)


beh.site <- merge(beh.site, RCExotic, by = "Site", all.x = T, all.y = F) # Merge with beh.site
head(beh.site) # Check this has worked

beh.site$Prop_Exotic_RC_clvt <- beh.site[,"Exotic.CC"]/rowSums(beh.site[,c("Exotic.CC","Exotic.RX")]) # Calculate the propensity for a road crossing


behNative <- beh6[which(beh6$Species %in% c("Kangaroo", "Macropod", "Monitor", "Possum", "Whiptail")),] # Create the road crossings data for natives, first finding where they were observed
head(behNative) # Check how this looks
behNative <- tidy.df(data.set = behNative) # Clean up
RCNative <- data.frame(Site = as.numeric(dimnames(table(behNative$Site, behNative$Behaviour))[[1]]), CC = table(behNative$Site, behNative$Behaviour)[,1], RX = table(behNative$Site, behNative$Behaviour)[,2]) # Create the road crossings data
head(RCNative); dim(RCNative) # Check this worked
# Rename columns
names(RCNative)[names(RCNative) == "CC"] <- "Native.CC"
names(RCNative)[names(RCNative) == "RX"] <- "Native.RX"
# Double the number of road crossings
RCNative$Native.RX <- round(RCNative$Native.RX*2, 0)


beh.site <- merge(beh.site, RCNative, by = "Site", all.x = T, all.y = F) # Merge with beh.site
head(beh.site) # Check this has worked

beh.site$Prop_Native_RC_clvt <- beh.site[,"Native.CC"]/rowSums(beh.site[,c("Native.CC","Native.RX")]) # Calculate the propensity for a road crossing

head(beh.site) # Check how this looks, there are NAs as we can only see the first 6 sites and there were no crossings recorded until site 8


behSmall <- beh6[which(beh6$Species %in% c("Monitor", "Possum", "Hare", "Rabbit", "Amphibian", "Snake", "Rodent", "Quail", "Dragon", "Cat")),] # Create the road crossings data for small species, first finding where they were observed
head(behSmall)# How does that look
behSmall <- tidy.df(data.set= behSmall) # Clean up
RCSmall <- data.frame(Site = as.numeric(dimnames(table(behSmall$Site, behSmall$Behaviour))[[1]]), CC = table(behSmall$Site, behSmall$Behaviour)[,1], RX = table(behSmall$Site, behSmall$Behaviour)[,2]) # Create the road crossings data
head(RCSmall); dim(RCSmall) # Check this worked
#Rename columns
names(RCSmall)[names(RCSmall) == "CC"] <- "Small.CC"
names(RCSmall)[names(RCSmall) == "RX"] <- "Small.RX"
# Double the number of road crossings
RCSmall$Small.RX <- round(RCSmall$Small.RX*2, 0)

beh.site <- merge(beh.site, RCSmall, by = "Site", all.x = T, all.y = F) # Merge with beh.site
head(beh.site) # Check this has worked correctly

beh.site$Prop_Small_RC_clvt <- beh.site[,"Small.CC"]/rowSums(beh.site[,c("Small.CC","Small.RX")]) # Calculate the propensity for a road crossing
head(beh.site) # Check how this looks



behLarge <- beh6[which(beh6$Species %in% c("Kangaroo", "Macropod", "Whiptail", "Cow","Fox", "Dog","Deer")),] # Create the road crossings data for large species, first finding where they were observed.
head(behLarge) # How does that look
behLarge <- tidy.df(data.set = behLarge) # Clean up
RCLarge <-  data.frame(Site = as.numeric(dimnames(table(behLarge$Site, behLarge$Behaviour))[[1]]), CC = table(behLarge$Site, behLarge$Behaviour)[,1], RX = table(behLarge$Site, behLarge$Behaviour)[,2]) # Create the road crossings data
head(RCLarge); dim(RCLarge) # Check this worked
# Rename the columns
names(RCLarge)[names(RCLarge) == "CC"] <- "Large.CC"
names(RCLarge)[names(RCLarge) == "RX"] <- "Large.RX"
# Double the number of road crossings
RCLarge$Large.RX <- round(RCLarge$Large.RX*2, 0)


beh.site <- merge(beh.site, RCLarge, by = "Site", all.x= T, all.y = F) # Merge with beh.site
head(beh.site) # Check this has worked correctly 


beh.site$Prop_Large_RC_clvt <- beh.site[,"Large.CC"]/rowSums(beh.site[,c("Large.CC", "Large.RX")]) # Calculate the propensity for a road crossing
head(beh.site) # Check how this looks


beh.site$Clvt_sz <- relevel(beh.site$Clvt_sz, "small") # Make sure this has been organised correctly


# Check for correlations
# Correlations

cor.test(beh.site$VegC, ifelse(beh.site$Clvt_sz == "large", 1,0))  # Correlation of culvert veg dense with culvert size
plot(beh.site$Clvt_sz, beh.site$VegC) # Plot this

cor.test(beh.site$VegR, ifelse(beh.site$Clvt_sz == "large", 1,0)) # Correlation of road veg dense with culvert size
plot(beh.site$Clvt_sz, beh.site$VegR) # Plot this

cor.test(beh.site$Vegdif, ifelse(beh.site$Clvt_sz == "large", 1,0)) #Correlation of veg dense dif with culvert size
plot(beh.site$Clvt_sz, beh.site$Vegdif) # Plot this

cor.test(beh.site$Vegmean, ifelse(beh.site$Clvt_sz == "large", 1,0)) # Correlation of average veg dense with culvert size
plot(beh.site$Clvt_sz, beh.site$Vegmean)# Plot this

cor.test(beh.site$Openness, ifelse(beh.site$Clvt_sz == "large", 1,0)) # Correlation of culvert openness with culvert size
plot(beh.site$Clvt_sz, beh.site$Openness) # Plot this

cor.test(beh.site$Clvt_wdt, ifelse(beh.site$Clvt_sz == "large", 1,0)) # Correlation of culvert width with culvert size 
plot(beh.site$Clvt_sz, beh.site$Clvt_wdt) # Plot this

cor.test(beh.site$Length, ifelse(beh.site$Clvt_sz == "large", 1,0)) # Correlation of culvert length with culvert size
plot(beh.site$Clvt_sz, beh.site$Length) # Plot this

cor.test(ifelse(beh.site$Clvt_sz == "large", 1,0), beh.site$Clvt_ht) # Correlation of culvert height with culvert size
plot(beh.site$Clvt_sz, beh.site$Clvt_ht)# Plot this
table(beh.site$Site, beh.site$Clvt_sz) # What sizes are the culverts depending on the sites, does this look right? Yes, no issues





# Plots for appendix
dev.new(width=20, height=20, dpi=80, pointsize=30, noRStudioGD = T)
par(mfrow=c(3, 3), mgp=c(2.5,1,0), mar=c(4,4,3,3))

plot(beh.site$Clvt_sz, beh.site$VegC, xlab = "Culvert size", ylab = "Vegetation density at culvert (m)", las = 1) 
title(main = "(a)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = -0.11, p = 0.55", cex = 1, bty ="n", text.width = 0.4)

plot(beh.site$Clvt_sz, beh.site$VegR, xlab = "Culvert size", ylab = "Vegetation density at road (m)", las = 1)
title(main = "(b)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = -0.06, p = 0.73", cex = 1, bty = "n", text.width = 0.4)

plot(beh.site$Clvt_sz, beh.site$Vegdif, xlab = "Culvert size", ylab = "Vegetation difference (m)", las = 1)
title(main = "(c)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.16, p = 0.41", cex = 1, bty = "n", text.width = 0.52)

plot(beh.site$Clvt_sz, beh.site$Vegmean, xlab = "Culvert size", ylab = "Average vegetation density (m)", las = 1)
title(main = "(d)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = -0.09, p = 0.62", cex = 1, bty = "n", text.width = 0.4)

plot(beh.site$Clvt_sz, beh.site$Openness, xlab = "Culvert size", ylab = "Culvert openness ratio", las = 1)
title(main = "(e)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.68, p <0.001", cex = 1, bty = "n", text.width = 0.37)

plot(beh.site$Clvt_sz, beh.site$Clvt_ht, xlab = "Culvert size", ylab = "Culvert height (m)", las = 1)
title(main = "(f)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.82, p <0.001", cex = 1, bty = "n", text.width = 1)

plot(beh.site$Clvt_sz, beh.site$Clvt_wdt, xlab = "Culvert size", ylab = "Culvert width (m)", las = 1)
title(main = "(g)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topright", legend = "r = 0.62, p = <0.001", cex = 1, bty = "n", text.width = )

plot(beh.site$Clvt_sz, beh.site$Length, xlab = "Culvert size", ylab = "Culvert Length (m)", las = 1)
title(main = "(h)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = -0.20, p = 0.29", cex = 1, bty = "n", text.width = 5)



# How does culvert size influence the proportion of crossings under the culvert
# To analyse this, as these are proportions we will be undertaking beta regressions instead of the generalized linear models used above. 

?mgcv
gam(Prop_RC_clvt ~ 1 , family=betar(link="logit"), data = beh.site) 

gam(Prop_RC_clvt ~ Clvt_sz , family=betar(link="logit"), data = beh.site) 

head(beh.site)


# All animals
m_null.2 <- gam(Prop_RC_clvt ~ 1, family=betar(link="logit"), data = beh.site) 
summary(m_null.2) # Null model


m5 <- gam(Prop_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m5) # Not significant
AICc(m_null.2); AICc(m5)


AICc(m_null.2); AICc(m5) # Null model is better, the lower the value the better

# Kangaroos
m_null.3 <- gam(Prop_Kang_RC_clvt ~ 1, family = betar(link="logit"), data = beh.site)
summary(m_null.3) # Null model

m6 <- gam(Prop_Kang_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m6) # Not sig.
AICc(m_null.3); AICc(m6) #Null model is better


# Exotics
m_null.4 <- gam(Prop_Exotic_RC_clvt ~ 1, family = betar(link="logit"), data = beh.site)
summary(m_null.4) # Null model


m7 <- gam(Prop_Exotic_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m7)# Not sig.
AICc(m_null.4); AICc(m7) # null model is better


# Natives
m_null.5 <- gam(Prop_Native_RC_clvt ~ 1, family = betar(link="logit"), data = beh.site)
summary(m_null.5) # Null model

m8 <- gam(Prop_Native_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m8)# Not sig.
AICc(m_null.5); AICc(m8) # Null model is better


# Small species

m_null.6 <- gam(Prop_Small_RC_clvt ~ 1, family = betar(link="logit"), data = beh.site)
summary(m_null.6) # Null model

m9 <- gam(Prop_Small_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m9)# Not sig.
AICc(m_null.6); AICc(m9) # Null model is better


# Large species

m_null.7 <- gam(Prop_Large_RC_clvt ~ 1, family = betar(link="logit"), data = beh.site)
summary(m_null.7) # Null model

m10 <- gam(Prop_Large_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m10)# Not significant
AICc(m_null.7); AICc(m10) # Null model is better




# What if we look at vegetation, does that influence road crossings in any way? 

# Check for correlations
# Correlations

cor.test(beh.site$VegC, beh.site$Vegmean)  # Correlation of culvert veg dense with average veg dense
plot(beh.site$Vegmean, beh.site$VegC) # Plot this

cor.test(beh.site$VegR, beh.site$Vegmean) # Correlation of road veg dense with average veg dense
plot(beh.site$Vegmean, beh.site$VegR) # Plot this

cor.test(beh.site$Vegdif, beh.site$Vegmean) # Correlation of the veg dif between culverts and roads and the average veg dense
plot(beh.site$Vegmean, beh.site$Vegdif) # Plot this

cor.test(ifelse(beh.site$Clvt_sz == "large", 1,0), beh.site$Vegmean) #Correlation of average veg dense with culvert size
plot(beh.site$Clvt_sz, beh.site$Vegmean) # Plot this

cor.test(beh.site$Openness, beh.site$Vegmean) # Correlation of culvert openness with average veg dense
plot(beh.site$Vegmean, beh.site$Openness) # Plot this

cor.test(beh.site$Clvt_wdt, beh.site$Vegmean) # Correlation of culvert width with average veg dense
plot(beh.site$Vegmean, beh.site$Clvt_wdt) # Plot this

cor.test(beh.site$Length, beh.site$Vegmean) # Correlation of culvert length with average veg dense
plot(beh.site$Vegmean, beh.site$Length) # Plot this

cor.test(beh.site$Clvt_ht, beh.site$Vegmean) # Correlation of culvert height with average veg dense
plot(beh.site$Vegmean, beh.site$Clvt_ht)# Plot this






# Plots for appendix
dev.new(width=20, height=20, dpi=80, pointsize=30, noRStudioGD = T)
par(mfrow=c(3, 3), mgp=c(2.5,1,0), mar=c(4,4,3,3))

plot(beh.site$Vegmean, beh.site$VegC, xlab = "Average vegetation density (m)", ylab = "Vegetation density at culvert (m)", las = 1)
title(main = "(a)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.99, p = <0.001", cex = 1, bty ="n", text.width = 0.5)

plot(beh.site$Vegmean, beh.site$VegR, xlab = "Average vegetation density (m)", ylab = "Vegetation density at road (m)", las = 1)
title(main = "(b)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.99, p = <0.001", cex = 1, bty = "n", text.width = 0.4)

plot(beh.site$Vegmean, beh.site$Vegdif, xlab = "Average vegetation density (m)", ylab = "Vegetation density difference (m)", las = 1)
title(main = "(c)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r= 0.09, p = 0.64", cex = 1, bty = "n", text.width = 0.4)

plot(beh.site$Clvt_sz, beh.site$Vegmean, xlab = "Culvert size", ylab = "Average vegetation density (m)", las = 1)
title(main = "(c)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = -0.09, p = 0.62", cex = 1, bty = "n", text.width = 0.52)

plot(beh.site$Vegmean, beh.site$Openness, xlab = "Average vegetation density (m)", ylab = "Culvert openness ratio", las = 1)
title(main = "(d)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = -0.15, p = 0.41", cex = 1, bty = "n", text.width = 0.37)

plot(beh.site$Vegmean, beh.site$Clvt_ht, xlab = "Average vegetation density (m)", ylab = "Culvert height (m)", las = 1)
title(main = "(e)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.06, p = 0.76", cex = 1, bty = "n", text.width = 1)

plot(beh.site$Vegmean, beh.site$Clvt_wdt, xlab = "Average vegetation density (m)", ylab = "Culvert width (m)", las = 1)
title(main = "(f)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = -0.009, p = 0.96", cex = 1, bty = "n", text.width = )

plot(beh.site$Vegmean, beh.site$Length, xlab = "Average vegetation density (m)", ylab = "Culvert Length (m)", las = 1)
title(main = "(g)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.97, p = <0.001", cex = 1, bty = "n", text.width = 5)



# Models

# All animals
m11 <- gam(Prop_RC_clvt ~ Vegmean, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m11) # Not sig.
AICc(m_null.2); AICc(m11) # null model is better

# Kangaroos

m12 <- gam(Prop_Kang_RC_clvt ~ Vegmean, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m12) # Not sig.
AICc(m_null.3); AICc(m12) # Null model is better



# Exotics

m13 <- gam(Prop_Exotic_RC_clvt ~ Vegmean, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m13)# Not sig.
AICc(m_null.4); AICc(m13) # Null model is better



# Natives
m14 <- gam(Prop_Native_RC_clvt ~ Vegmean, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m14)# Not sig.
AICc(m_null.5); AICc(m14) # null model is better


# Small species

m15 <- gam(Prop_Small_RC_clvt ~ Vegmean, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m15)# Significant influence of higher average vegetation density
AICc(m_null.6); AICc(m15) # Null model is still better



# Large species 
m16 <- gam(Prop_Large_RC_clvt ~ Vegmean, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m16)# Not sig.
AICc(m_null.7); AICc(m16) # Null model is better



# Model selection framework information - smaller AICc the better it is (more negative). Culvert size was first then vegetation
#All animals
AICc(m_null.2); AICc(m5); AICc(m11) # Null model is best
logLik.gam(m_null.2)
logLik.gam(m11)
logLik.gam(m5)



# Exotic species
AICc(m_null.4); AICc(m7); AICc(m13) #Null model is best
logLik.gam(m_null.4)
logLik.gam(m13)
logLik.gam(m7)




# Native species
AICc(m_null.5); AICc(m8); AICc(m14) # Null model is best
logLik.gam(m_null.5)
logLik.gam(m14)
logLik.gam(m8)



# Kangaroos
AICc(m_null.3); AICc(m6); AICc(m12) #Null model is best
logLik.gam(m_null.3)
logLik.gam(m6)
logLik.gam(m12)



# Small species
AICc(m_null.6); AICc(m9); AICc(m15) #Null model is best
logLik.gam(m_null.6)
logLik.gam(m15)
logLik.gam(m9)



# Large species
AICc(m_null.7); AICc(m10); AICc(m16) # Null model is best
logLik.gam(m_null.7)
logLik.gam(m10)
logLik.gam(m16)


