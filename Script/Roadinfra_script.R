# Written by Felicity Charles 5/2/2021
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
head(beh) # Check the first few rows of beh





# Check the levels of each column of the occurence data
levels(dat$Cam_typ)
levels(dat$Clvt_sz)
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
length(unique(rc$Nat_ref))
rc$Nat_ref <- as.factor(rc$Nat_ref) # Make it a factor variable
table(rc$Site, rc$Nat_ref) # Check this has worked correctly
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


## Exploring occurrence data
colSums(dat[, which(colnames(dat) == "Kangaroo"): (ncol(dat)-1)]) # What are the total number of animals observed for each species
# We want to analyse data for species separately if there were more than 30 occurrences of that species
# We can analyse Kangaroos, Whiptail, Fox, Hare, Dog, Rodent, and Cat seperately.
# We can analyse Kangaroo, Whiptail, and Macropod data together, and also Hare and Rabbit data together. 


# Sum medium-large exotic mammals (Cat, Fox, Hare, Rabbit, Dogs)
med.mam <- c("Cat", "Fox", "Hare", "Rabbit", "Dog") # Create a value that contains the medium-large exotic mammals
rowSums(dat[,which(colnames(dat)%in% med.mam)])
dat$xmed.mam <- rowSums(dat[,which(colnames(dat)%in% med.mam)]) # Add this column to the occurrence data 
head(dat) # Ensure that this new column was added to the occurrence data

# Sum native macropod species (Kangaroo, Whiptail, and Macropod)
nat.mac <- c("Kangaroo", "Whiptail", "Macropod") # Create a value that contains the native macropod species
rowSums(dat[,which(colnames(dat)%in% nat.mac)])
dat$nat.mac <- rowSums(dat[,which(colnames(dat)%in% nat.mac)]) # Add this column to the occurrence data
head(dat) # Ensure that this new column was added to the occurrence data


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

## Stage 1 - analysis
# Are there more animals using culverts to cross roads than crossing over the surface of the road?
# Is culvert use affected by culvert size and road type? 
table(dat$Type) # How many culvert cameras are there compared to road cameras - more culvert cameras
table(dat$Clvt_sz) # How many large culverts are there compared to small culverts - more small culverts

table(dat$Type, dat$nat.mac>0) # What is the proportion of native mammals observed on culvert cameras as compared to road cameras, greater than zero. 
table(dat$Type, dat$nat.mac>0)[, 2]/rowSums(table(dat$Type, dat$nat.mac>0)) # Equal number of occurrences for Macropod species between cameras

table(dat$Type, dat$xmed.mam>0) # There is a difference between the number of exotic medium-large sized mammals observed on culvert cameras vs. road cameras 
table(dat$Type, dat$xmed.mam>0)[, 2]/rowSums(table(dat$Type, dat$xmed.mam>0)) # There is a difference between the number of exotic medium-large sized mammals that were observed on culvert cameras than road cameras 

table(dat$Type, dat$Exotic>0) # There is a difference between the number of Exotic species observed on culvert vs. road cameras
table(dat$Type, dat$Exotic>0)[,2]/rowSums(table(dat$Type, dat$Exotic>0)) # There are slightly more exotic species observed on road cameras than culvert cameras

table(dat$Type, dat$Native>0) # There is a difference between the number of native species observed on culvert vs road cameras
table(dat$Type, dat$Native>0)[,2]/rowSums(table(dat$Type, dat$Native>0)) # There are slightly more native species observed on road cameras than on culvert cameras. 


# Produce histograms
hist(dat$nat.mac[dat$nat.mac<20]) # Create a histogram of native Macropod species occurrences less than 20, frequency over 150 for 0 occurrences
hist(dat$xmed.mam[dat$xmed.mam<50]) # Create a histogram of exotic medium-large sized mammal occurrences less than 50, frequency over 150 for 0 occurrences
hist(dat$All_animals[dat$All_animals<50]) # Create a histogram of all animals occurrences less than 50, frequency over 150 for 0 occurrences
hist(dat$Exotic[dat$Exotic<50]) # Zero inflation
hist(dat$Native[dat$Native<50])
# The data is zero inflated, need to determine what R model to use for poisson
# Could use binomial glm - turn the count data into presence/absence data


# Turn count data into presence/absence data
dat$nat.mac.pa<- ifelse(dat$nat.mac>0,1,0)
m4 <- glm(nat.mac.pa ~ Type *Clvt_sz, family = "binomial", data = dat)
summary(m4) # Not significant

dat$xmed.mam.pa <- ifelse(dat$xmed.mam>0, 1, 0)
m5 <- glm(xmed.mam.pa ~ Type * Clvt_sz, family = "binomial", data = dat) # Test the interaction between camera type and culvert size for the presence/absence of medium-large size exotic mammals
summary(m5) 
# There are significantly more exotic medium-large mammals observed on culverts cameras, although this may be due to a higher sampling effort on culvert cameras. The size of the culvert was not statistically significant, this is likely due to these species not using culverts to cross roads.  


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
summary(m10) # Not significant

dat$Native.pa <- ifelse(dat$Native>0,1,0)
m11 <- glm(Native.pa ~ Type * Clvt_sz, family = "binomial", data = dat)
summary(m11) # Not significant

dat$All_animals.pa <- ifelse(dat$All_animals>0,1,0)
m12 <- glm(All_animals.pa ~ Type * Clvt_sz, family = "binomial", data = dat)
summary(m12) # Not significant



# Plot the average number of native macropod species per camera type
tapply(X = dat$nat.mac, INDEX = dat$Type, FUN = mean)
plot(dat$Type, dat$nat.mac) # The box plots look quite similar due to outliers with great values, although there is a difference between the average number of macropods observed on culvert cameras (5.741667) compared to road cameras (6.716667)

# Plot the average number of exotic medium-large sized mammals per camera type
tapply(X = dat$xmed.mam, INDEX = dat$Type, FUN = mean)
plot(dat$Type, dat$xmed.mam) # The box plots look quite similar, although there is a difference between the average number of exotic medium-large size mammals observed on culvert cameras (0.975) compared to road cameras (2.000)


# Discovery 1 - slight increase in the number of exotic mammals, and also macropods observed on road cameras compared to culvert cameras, is this worth investigating statistically?




rc2<- rc[,c("Site", "Rd_typ")] # Create a new dataset that only contains the information for site and road type
head(rc2); dim(rc2) # Check this worked, there are only 30 sites
which(duplicated(rc2$Site)) # Although this is correct, double check that there are no duplications just in case
dat2 <- merge(dat, rc2, by = "Site", all.x = T, all.y = F) # Merge the data in the new site characteristics dataset with dat2 according to Site. 
head(dat2); dim(dat2) # Check that this has worked, all looks good. 

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


# Take just the vegetation cover data from the road characteristics data frame and add it to beh4
rc5 <- rc[,c("Site", "Veg_cov_C1","Veg_cov_C2","Veg_cov_C3", "Veg_cov_C4", "Veg_cov_R1", "Veg_cov_R2")] # Create a new dataset that only contains the information for site and vegetation 
rc5 # Check that this has worked
# To be able to stack the columns we need the package tidyr
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

dat2 <- merge(dat2, rc6, by = c("Site", "Cam_pt"), all.x = T , all.y = F) # Merge the data in the new site characteristics dataset with dat2 according to Site and Camera point.
head(dat2); dim(dat2)


rc7 <- rc[,c("Site", "Length", "Nat_ref", "Clvt_ht")] # Create a new data frame with just the site, length, nat_ref, and Clvt_ht of the culvert
dat2 <- merge(dat2, rc7, by = "Site", all.x = T, all.y = F) # Merge this with beh4 but create a new data set 

# Models with type are better than those that exclude it - explore occurrence data to determine what factors influence species presence/absence
head(dat2);dim(dat2) # Look at the first few rows of data
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



# Checking for correlations
head(dat2); dim(dat2)
plot(dat2$Length, dat2$Veg_dense) # How does this look as a plot
cor.test(dat2$Length, dat2$Veg_dense) # Correlated, more veg dense on wider rd, first looked at correlation so then simplified model set to only look at length add to methods, present this in thesis
# road length and vegetation density were correlated (r = 0.23, p = 0.001)
rl <- lmer(Veg_dense ~ Length + (1 |Site), data = dat2)
summary(rl) # Summarise the relationship between veg dense and length


# Vegetation cover was no longer included as we preferred to use the numerical version of vegetation measurements - vegetation density

head(dat2)
cor.test(ifelse(dat2$Rd_typ== "major",1,0), dat2$Veg_dense) # Check correlation of veg dense with rd type
summary(lm(Veg_dense ~ Rd_typ, data = dat2)) # Relationship between veg dense and road type (not going to be included in the thesis as these are not the results we expect)


summary(lm(Veg_dense ~ Length, data = dat2)) # Relationship between veg dense and road width 

Nr <- lmer(Veg_dense ~ Nat_ref+ (1 |Site), data = dat2) # Check this one also and do for behavioural
summary(Nr) # Relationship of veg dense and nat ref, nat ref has less veg dense
cor.test(ifelse(dat2$Nat_ref == "1", 1,0), dat2$Veg_dense) # -0.19, p=0.009


Cs <- lmer(Veg_dense ~ Clvt_sz + (1 | Site), data = dat2) # Relationship of veg dense with culvert size
summary(Cs) # Not sig.
cor.test(ifelse(dat2$Clvt_sz == "large", 1,0), dat2$Veg_dense) # 0.04, p=0.62

cor.test(ifelse(dat2$Nat_ref == "1", 1,0), dat2$Length) # -0.72, p = < 0.001 (not significant)



## Create the plots for appendix
dev.new(width=20, height=20, dpi=80, pointsize=28, noRStudioGD= T)
par(mfrow=c(2, 2), mgp=c(2.5,1,0), mar=c(4,4,3,3), cex = 1, las = 1)

plot(dat2$Length, dat2$Veg_dense, xlab = "Road width (m)", ylab = "Vegetation density (m)", las = 1) # As vegetation density increases so do length
title(main = "(a)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.23, p = 0.001", cex = 1, bty ="n", text.width = 1)

plot(dat2$Nat_ref, dat2$Veg_dense, xaxt = "n", xlab = "Nature refuge", ylab = "Vegetation density (m)", las = 1)# Nature refuge areas had lower vegetation densities
axis(side = 1, at = 1:2, labels = c("No", "Yes"))
title(main = "(b)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topright", legend = "r = -0.20, p = 0.009", cex = 1, bty ="n", text.width = 1.4)

plot(dat2$Clvt_sz, dat2$Veg_dense, xlab = "Culvert size", ylab = "Vegetation density (m)", las = 1)
title(main = "(c)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("top", legend = "r = 0.03, p = 0.62", cex = 1, bty ="n", text.width =1)

plot(dat2$Nat_ref, dat2$Length, xaxt = "n", xlab = "Nature refuge", ylab = "Road width (m)", las = 1)
title(main = "(d)", outer = F, adj = 0, cex.main = 1, line = 0.3)
axis(side = 1, at = 1:2, labels = c("No", "Yes"))
legend("top", legend = "r = -0.72, p = <0.001", cex = 1, bty ="n", text.width =1)




# For exotic species p/a
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
summary(M1b) # Prodeuc additive model
#What is the Log Likelihood of this model?
logLik(M1b)


AICc(Mnull); AICc(M1); AICc(M1a); AICc(M1b); AICc(M2) # Null model is best, M3a is best model for veg dense which is quite similar to the type only model

cand.set <- list(Mnull,M1,M1a, M1b, M2)

aictab(cand.set)


M2 <- glmer(Exotic.pa ~ Type + (1 | Site), family = "binomial", data = dat2)
summary(M2) # Model type only 
#What is the Log Likelihood of this model?
logLik(M2)


# This is an okay model, other models explain what is occurring better with type included 
# As we have found in previous models, there are significantly less exotic species observed on culvert cameras than road cameras. Culverts are significantly less likely to observe exotic species (P=0.00254), therefore culverts are not used by exotic species!
AICc(Mnull); AICc(M2) # Null model is better



# For Native species p/a 

Mnull.1 <- glmer(Native.pa ~ 1 + (1 | Site), family = "binomial", data = dat2)
summary(Mnull.1) # What is happening if nothing was occurring
#What is the Log Likelihood of this model?
logLik(Mnull.1)


M3 <- glmer(Native.pa ~ Type * Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M3) # Produce interactive model, Significant
#What is the Log Likelihood of this model?
logLik(M3)

# Predict from this interactive model
ndM3 <- data.frame(Type = as.factor(c(rep("C", 50), rep("R", 50))), Veg_dense = seq(min(dat2$Veg_dense), max(dat2$Veg_dense), length.out = 50))
prM3 <- predictSE(mod = M3, newdata = ndM3, se.fit = T, type = "response")
prM3 <- data.frame(ndM3, fit = round(prM3$fit, 4), se = round(prM3$se.fit, 4))
prM3$lci <- prM3$fit - (prM3$se * 1.96)
prM3$uci <- prM3$fit + (prM3$se * 1.96)

# Plot predictions from interactive model
plot(prM3$Veg_dense[prM3$Type=="C"], prM3$fit[prM3$Type=="C"], pch=20, ylim=c(min(prM3$lci), max(prM3$uci)), xlab = "Veg. density", ylab = "Prob. of occurrence", type="l")
lines(prM3$Veg_dense[prM3$Type=="C"], prM3$lci[prM3$Type=="C"], lty=2)
lines(prM3$Veg_dense[prM3$Type=="C"], prM3$uci[prM3$Type=="C"], lty=2)
lines(prM3$Veg_dense[prM3$Type=="R"], prM3$fit[prM3$Type=="R"], lty=1, col="red")
lines(prM3$Veg_dense[prM3$Type=="R"], prM3$lci[prM3$Type=="R"], lty=2, col ="red")
lines(prM3$Veg_dense[prM3$Type=="R"], prM3$uci[prM3$Type=="R"], lty=2, col ="red")


M3a <- glmer(Native.pa ~ Veg_dense + (1 | Site), family = "binomial", data = dat2)
summary(M3a) # Model veg dense only

#What is the Log Likelihood of this model?
logLik(M3a)


M3b <- glmer(Native.pa ~ Type + Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M3b) # Produce additive model, Significant 
#What is the Log Likelihood of this model?
logLik(M3b)


# Predict from additive model
ndM3b <- data.frame(Type = as.factor(c(rep("C", 50), rep("R", 50))), Veg_dense = seq(min(dat2$Veg_dense), max(dat2$Veg_dense), length.out = 50))
prM3b <- predictSE(mod = M3b, newdata = ndM3b, se.fit = T, type = "response")
prM3b <- data.frame(ndM3b, fit = round(prM3b$fit, 4), se = round(prM3b$se.fit, 4))
prM3b$lci <- prM3b$fit - (prM3b$se * 1.96)
prM3b$uci <- prM3b$fit + (prM3b$se * 1.96)
head(prM3b)

# Plot predictions from this model
plot(prM3b$Veg_dense[prM3b$Type=="C"], prM3b$fit[prM3b$Type=="C"], pch=20, ylim=c(min(prM3b$lci), max(prM3b$uci)), xlab = "Veg. density", ylab = "Prob. of occurrence", type="l")
lines(prM3b$Veg_dense[prM3b$Type=="C"], prM3b$lci[prM3b$Type=="C"], lty=2)
lines(prM3b$Veg_dense[prM3b$Type=="C"], prM3b$uci[prM3b$Type=="C"], lty=2)
lines(prM3b$Veg_dense[prM3b$Type=="R"], prM3b$fit[prM3b$Type=="R"], lty=1, col="red")
lines(prM3b$Veg_dense[prM3b$Type=="R"], prM3b$lci[prM3b$Type=="R"], lty=2, col ="red")
lines(prM3b$Veg_dense[prM3b$Type=="R"], prM3b$uci[prM3b$Type=="R"], lty=2, col ="red")

AICc(Mnull.1); AICc(M3); AICc(M3a); AICc(M3b); AICc(M4) # M3b is best then M3, better than null and type only

anova(Mnull.1, M3) # Significant


M4 <- glmer(Native.pa ~ Type + (1|Site), family = "binomial", data = dat2)
summary(M4) # Model type only, it is significant although the AIC is not within 2 of the other two models
#What is the Log Likelihood of this model?
logLik(M4)


# Native species are negatively influence by culvert cameras to a significant degree (P=7.35e-05), and positively influenced by road cameras to a significant degree (P=0.0399). Therefore native species have a higher probability of being observed on road cameras than on culvert cameras. Native species, like exotic species, are not approaching and hanging around culverts, increasing the probability of their observations on road cameras. 
AICc(Mnull.1); AICc(M4) # Better than null




# Plots for thesis
dev.new(width=12, height=10, dpi=80, pointsize=20, noRStudioGD = T)
par(mfrow=c(1, 1), mgp=c(2.5,1,0), mar=c(4,4,2,2), oma=c(0,0,0,6), cex = 1, las = 1)


plot(prM3b$Veg_dense[prM3b$Type=="C"], prM3b$fit[prM3b$Type=="C"], pch=20, ylim=c(min(prM3b$lci), max(prM3b$uci)), xlab = "Vegetation density (m)", ylab = "Probability of occurrence", type="l", las = 1)
lines(prM3b$Veg_dense[prM3b$Type=="C"], prM3b$lci[prM3b$Type=="C"], lty=2)
lines(prM3b$Veg_dense[prM3b$Type=="C"], prM3b$uci[prM3b$Type=="C"], lty=2)
lines(prM3b$Veg_dense[prM3b$Type=="R"], prM3b$fit[prM3b$Type=="R"], lty=1, col="red")
lines(prM3b$Veg_dense[prM3b$Type=="R"], prM3b$lci[prM3b$Type=="R"], lty=2, col ="red")
lines(prM3b$Veg_dense[prM3b$Type=="R"], prM3b$uci[prM3b$Type=="R"], lty=2, col ="red")

par(xpd=NA)
legend(x=1.5, y=1,legend =c("Culvert", "Road"), col = c("black", "red"), lty=1, lwd =1, cex = 1, bty ="n", text.width = 0.2)
par(xpd=F) # just make note this model isn't very good - something else going on as well, good for low veg but not for high, we can see there is an effect at low vegetation densities but not at high, whereas the additive model displays that the effect of vegetation and position is separate, overall more animals observed at roads than culverts but also high occurrences at low densities , additive - effect of veg and position 




# For Kangaroo p/a 
Mnull.2 <- glmer(Kangaroo.pa ~ 1 + (1|Site), family = "binomial", data = dat2)
summary(Mnull.2) # Produce null model

#What is the Log Likelihood of this model?
logLik(Mnull.2)


M5 <- glmer(Kangaroo.pa ~ Type * Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M5)  # Produce interactive model
#What is the Log Likelihood of this model?
logLik(M5)


M5a <- glmer(Kangaroo.pa ~ Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M5a) # Produce veg dense only model
#What is the Log Likelihood of this model?
logLik(M5a)

M5b <- glmer(Kangaroo.pa ~ Type + Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M5b) # Produce additive model
#What is the Log Likelihood of this model?
logLik(M5b)


AICc(Mnull.2); AICc(M5); AICc(M5a); AICc(M5b); AICc(M6) # NUll model is best model so will not be plotted

# Low and high vegetation densities at culverts had a negative influence on the probability of observing Kangaroos, significantly so for increasing vegetation densities at culverts. Whereas low vegetation densities at roads had a positive influence on the probability of observing a Kangaroo, although as vegetation density increased this influence became negative. 


anova(Mnull.2, M5) # Not significant
AICc(Mnull.2); AICc(M6) # Null model is better


M6 <- glmer(Kangaroo.pa ~ Type + (1|Site), family = "binomial", data = dat2)
summary(M6) # Produce type only model
# Culverts have a negative influence on the probability of observing Kangaroos, statistically significantly so (P=0.00217), whereas roads have a positive influence on the probability of observing Kangaroos.

#What is the Log Likelihood of this model?
logLik(M6)



## For all animals
Mnull.3 <- glmer(All_animals.pa ~ 1 + (1|Site), family = "binomial", data = dat2)
summary(Mnull.3) # Produce null model
#What is the Log Likelihood of this model?
logLik(Mnull.3)

M7 <- glmer(All_animals.pa ~ Type * Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M7) # Produce interactive model, Significant
#What is the Log Likelihood of this model?
logLik(M7)

M7a <- glmer(All_animals.pa ~ Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M7a) # Produce veg dense only model
#What is the Log Likelihood of this model?
logLik(M7a)

M7b <- glmer(All_animals.pa ~ Type + Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M7b) # Produce additive model, Significant
#What is the Log Likelihood of this model?
logLik(M7b)

# Predict from the additive model
ndM7b <- data.frame(Type = as.factor(c(rep("C", 50), rep("R", 50))), Veg_dense = seq(min(dat2$Veg_dense), max(dat2$Veg_dense), length.out = 50))
prM7b <- predictSE(mod = M7b, newdata = ndM7b, se.fit = T, type = "response")
prM7b <- data.frame(ndM7b, fit = round(prM7b$fit, 4), se = round(prM7b$se.fit, 4))
prM7b$lci <- prM7b$fit - (prM7b$se * 1.96)
prM7b$uci <- prM7b$fit + (prM7b$se * 1.96)
head(prM7b)

# Plot predictions from the additive model
plot(prM7b$Veg_dense[prM7b$Type=="C"], prM7b$fit[prM7b$Type=="C"], pch=20, ylim=c(min(prM7b$lci), max(prM7b$uci)), xlab = "Veg. density", ylab = "Prob. of occurrence", type="l")
lines(prM7b$Veg_dense[prM7b$Type=="C"], prM7b$lci[prM7b$Type=="C"], lty=2)
lines(prM7b$Veg_dense[prM7b$Type=="C"], prM7b$uci[prM7b$Type=="C"], lty=2)
lines(prM7b$Veg_dense[prM7b$Type=="R"], prM7b$fit[prM7b$Type=="R"], lty=1, col="red")
lines(prM7b$Veg_dense[prM7b$Type=="R"], prM7b$lci[prM7b$Type=="R"], lty=2, col ="red")
lines(prM7b$Veg_dense[prM7b$Type=="R"], prM7b$uci[prM7b$Type=="R"], lty=2, col ="red")

AICc(Mnull.3); AICc(M7); AICc(M7a); AICc(M7b); AICc(M8) # M7b is best then M7

# Lower vegetation densities at culvert and road locations had a positive influence on the probability of observing an animal, whereas increasing vegetation densities had a negative influence on the probability of observing an animal. 
anova(Mnull.3, M7) # Significant

# Predict from the interactive model
ndM7 <- data.frame(Type = as.factor(c(rep("C", 50), rep("R", 50))), Veg_dense = seq(min(dat2$Veg_dense), max(dat2$Veg_dense), length.out = 50))
prM7 <- predictSE(mod = M7, newdata = ndM7, se.fit = T, type = "response")
prM7 <- data.frame(ndM7, fit = round(prM7$fit, 4), se = round(prM7$se.fit, 4))
prM7$lci <- prM7$fit - (prM7$se * 1.96)
prM7$uci <- prM7$fit + (prM7$se * 1.96)
head(prM7)

# Plot predictions from the interactive model
plot(prM7$Veg_dense[prM7$Type=="C"], prM7$fit[prM7$Type=="C"], pch=20, ylim=c(min(prM7$lci), max(prM7$uci)), xlab = "Veg. density", ylab = "Prob. of occurrence", type="l")
lines(prM7$Veg_dense[prM7$Type=="C"], prM7$lci[prM7$Type=="C"], lty=2)
lines(prM7$Veg_dense[prM7$Type=="C"], prM7$uci[prM7$Type=="C"], lty=2)
lines(prM7$Veg_dense[prM7$Type=="R"], prM7$fit[prM7$Type=="R"], lty=1, col="red")
lines(prM7$Veg_dense[prM7$Type=="R"], prM7$lci[prM7$Type=="R"], lty=2, col ="red")
lines(prM7$Veg_dense[prM7$Type=="R"], prM7$uci[prM7$Type=="R"], lty=2, col ="red")
axis(side=1, at=1:28, labels = unique(dat2$Veg_dense), cex.axis = 0.6, las = 2)
legend("bottomleft", legend = c("Culvert site", "Road site"), pch=c(20,1), cex = 0.6, bty="n", text.width =5)


M8 <- glmer(All_animals.pa ~ Type + (1|Site), family = "binomial", data = dat2)
summary(M8) # Produce the type only model, Not quite significant
# Culverts and road locations both positively influence the probability of observing an animal, although road cameras are more likely to observe an animal than culvert cameras. 
#What is the Log Likelihood of this model?
logLik(M8)

anova(Mnull.3, M8) # Not significant
AICc(Mnull.3); AICc(M8)  # Only just better than null but not within 2 of the interactive and additive model so will not be plotted



## Plots for thesis
dev.new(width=20, height=10, dpi=50, pointsize=35, noRStudioGD = T)
par(mfrow=c(1, 2), mgp=c(2.5,1,0), mar=c(4,4,2,2), oma=c(0,0,0,6), cex = 1, las = 1)

plot(prM7b$Veg_dense[prM7b$Type=="C"], prM7b$fit[prM7b$Type=="C"], pch=20, ylim=c(min(prM7b$lci), max(prM7b$uci)), xlab = "Vegetation density (m)", ylab = "Probability of occurrence", type="l", las = 1)
lines(prM7b$Veg_dense[prM7b$Type=="C"], prM7b$lci[prM7b$Type=="C"], lty=2)
lines(prM7b$Veg_dense[prM7b$Type=="C"], prM7b$uci[prM7b$Type=="C"], lty=2)
lines(prM7b$Veg_dense[prM7b$Type=="R"], prM7b$fit[prM7b$Type=="R"], lty=1, col="red")
lines(prM7b$Veg_dense[prM7b$Type=="R"], prM7b$lci[prM7b$Type=="R"], lty=2, col ="red")
lines(prM7b$Veg_dense[prM7b$Type=="R"], prM7b$uci[prM7b$Type=="R"], lty=2, col ="red")
title(main = "(a) All animals", outer = F, adj = 0, cex.main = 1, line = 0.3)

plot(prM3b$Veg_dense[prM3b$Type=="C"], prM3b$fit[prM3b$Type=="C"], pch=20, ylim=c(min(prM3b$lci), max(prM3b$uci)), xlab = "Vegetation density (m)", ylab = "Probability of occurrence", type="l", las = 1)
lines(prM3b$Veg_dense[prM3b$Type=="C"], prM3b$lci[prM3b$Type=="C"], lty=2)
lines(prM3b$Veg_dense[prM3b$Type=="C"], prM3b$uci[prM3b$Type=="C"], lty=2)
lines(prM3b$Veg_dense[prM3b$Type=="R"], prM3b$fit[prM3b$Type=="R"], lty=1, col="red")
lines(prM3b$Veg_dense[prM3b$Type=="R"], prM3b$lci[prM3b$Type=="R"], lty=2, col ="red")
lines(prM3b$Veg_dense[prM3b$Type=="R"], prM3b$uci[prM3b$Type=="R"], lty=2, col ="red")
title(main = "(b) Native species", outer = F, adj = 0, cex.main = 1, line = 0.3)

par(xpd=NA)
legend(x=1.5, y=1,legend =c("Culvert", "Road"), col = c("black", "red"), lty=1, lwd =1, cex = 1, bty ="n", text.width = 0.2)
par(xpd=F)





# Explore behavioural data

head(beh); dim(beh) # Look at the first few rows of the data, there are 4713 individual triggers
sum(table(beh$Behaviour)) # There are 4713 rows of data
range(beh$Anim_num) # There are 1159 individual animals
table(beh$Behaviour) # There are 73 culvert crossing occurrences, 2865 foraging occurrences, 505 road crossing occurences, and 1270 sitting/standing still occurrences in the behaviour data.
sum(2865+1270) # What is the sum of the foraging and sitting still behaviours, 4135 occurences of 4713 individual triggers. 


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
length(unique(beh$Anim_num)) # Although there are 4713 observations there are only 1158 unique animals. There are more observations than we want/need. 

# How do we deal with the repeated observations? Do we take the first observation for each day or hour? No, we would loose data by filtering the data by the hour or day that it was observed in as sometimes individuals displayed different behaviours depending on the hour of the day, this would mean we could loose data for culvert crossings which are already quite low. 
# Therefore, if there is a repeated observation within same hour we remove it, but we will keep in mind the behaviour. So we want 1 observation per hour for each behaviour per individual animal. 


beh$anim_hr<-paste(beh$Anim_num,beh$Behaviour, beh$Day, beh$Month, beh$Hour, sep="_") # Create a value within the data that contains all the information needed to simply distinguish between the day, hour and behaviour of each animal
head(beh); dim(beh) # Check how this looks
unique(beh$anim_hr) # Check how this looks, not in order


# We want to order the data so that when they are subset, we can take the first observation per hour
beh2 <- beh[order(beh$Anim_num,beh$Behaviour, beh$Day, beh$Month, beh$Hour, beh$Min, beh$Sec),]
rownames(beh2)<-1:nrow(beh2)
beh2[beh2$Anim_num==42,] # Lets have a look at one of the individual animals and ensure that it is ordered correctly. 


# Create a new data frame with only the first observation for each animal per hour per behaviour type:
beh3 <- beh2[-which(duplicated(beh2$anim_hr)),]
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
table(beh4.1$Behaviour) # There are now only 32 culvert crossings, 946 foraging, 365 road crossings, and 328 sitting still occurences 
range(beh4.1$Site) # There are still 30 sites




# HOW MANY INDEPENDENT OBSERVATIONS ARE THERE FOR ROAD CROSSING BEHAVIOURS?



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
length(manim) # There are 111 repeated observations. 
dim(beh2) # There are 578 observations of road crossing and culvert crossing behaviours, with 111 repeated observations. 
length(unique(beh2$Anim_num)) # Although there are 578 observations there are only 383 unique animals. There are more observations than we want/need. 

# How do we deal with the repeated observations? Do we take the first observation for each day or hour? No, we would loose data by filtering the data by the hour or day that it was observed in as sometimes individuals displayed different behaviours depending on the hour of the day, this would mean we could loose data for culvert crossings which are already quite low. 
# Therefore, if there is a repeated observation within same hour we remove it, but we will keep in mind the behaviour. So we want 1 observation per hour for each behaviour per individual animal. 


beh2$anim_hr<-paste(beh2$Anim_num,beh2$Behaviour, beh2$Day, beh2$Month, beh2$Hour, sep="_") # Create a value within the data that contains all the information needed to simply distinguish between the day, hour and behaviour of each animal
head(beh2); dim(beh2) # Check how this looks
unique(beh2$anim_hr) # Check how this looks, not in order


# We want to order the data so that when they are subset, we can take the first observation per hour
beh2 <- beh2[order(beh2$Anim_num,beh2$Behaviour, beh2$Day, beh2$Month, beh2$Hour, beh2$Min, beh2$Sec),]
rownames(beh2)<-1:nrow(beh2)
beh2[beh2$Anim_num==25,] # Lets have a look at one of the individual animals and ensure that it is ordered correctly. 


# Create a new data frame with only the first observation for each animal per hour per behaviour type:
beh3 <- beh2[-which(duplicated(beh2$anim_hr)),]
beh3 <- droplevels(beh3)
rownames(beh3) <- 1:nrow(beh3)
head(beh3); dim(beh3)
table(beh3$Behaviour) # We now only have 33 culvert crossing observations, and 370 road crossing observations. We have successfully removed duplicates of the same behaviour of animals within the same hour. Although we should also think about whether an individual animal was observed at 10:52 and then again at 11:02, both of these observations would be kept as they are in seperate hours but this data cannot be considered independent. Need to figure out how to deal with this. 
table(beh3$Behaviour, beh3$Species)

beh3$behav<-ifelse(beh3$Behaviour=="RX", 0, 1) # We want to assign numeric values to the behaviour for the means of further analysis, culvert crossing = 0, road crossing = 1.  

# We know that the occurrences are still not independent, a cow is seen at 11:21 and then again at 12:07 on the same day, we want there to be an interval of an hour between occurrences so we need to fix this. Need to install package to write data into excel file and will edit the data through excel
# install.packages("writexl")
library(writexl)
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



rc7 <- rc[,c("Site", "Length", "Clvt_wdt", "Clvt_ht", "Nat_ref", "Clvt_wdt")] # Create a new data frame with culvert variables and nat ref
beh5 <- merge(beh4, rc7, by = "Site", all.x = T, all.y = F) # Merge this with beh4 but create a new data set 

# Would like to look at whether the presence of a nature refuge area influences the data, will go back into excel to add this. There was a small area that was classified as a nature refuge on Rosewood Laidley Road that was not within the HV boundary, all those locations encompassed by the HV boundary will be assigned as a nature refuge. 


dir() # Get the directory so we can access functions that are saved in separate files
source("Felicity_Functions.R") # Load the functions to this script 

head(beh5); dim(beh5) # Check the head of the data
length(unique(beh2$Site)) # There are 14 sites in this dataset
beh6 <- beh5[-which(beh5$Behaviour == "RX" & beh5$Type == "C"),] # If we reduced this dataset down what would happen
beh6<- tidy.df(beh6) # Clean it up
head(beh6); dim(beh6) # Check how it looks
row.names(beh6) # Redo row names
str(beh6) # Check everything is ok
length(unique(beh6$Site)) # How many sites are included in the beh6 data? 12
length(unique(beh5$Site)) # How many sites are included in the beh5 data? 14

# Lets go back and look at the data again. 
table(beh6$Species, beh6$Behaviour) # How many animals were observed performing each road crossing behaviour? 

head(beh6); dim(beh6) # Compare beh6 and beh5
head(beh5, 3); dim(beh5) # When we reduced the data even further by removing road crossings from culvert cameras we only ended up with 12 sites, we want to go back to beh5 which still contains road crossing on culvert cameras as this has 2 more sites included. The original dataset had 28 sites, 14 of these sites there were no road crossings observed either over or under the road, animals were only foraging or sitting still at these locations, therefore we only have 14 sites where road crossings were observed.


# This isn't giving us the results we want, we want to take it down to site level and compare behaviour at the site specific level. Go through hypothesis and ensure the new structure will answer these questions. Want to analyse the data as all animals, kangaroos only, exotic only, and native only 


head(rc3, 3); dim(rc3) # What is the structure of the road characteristics data

rc10 <- data.frame(Site = rc3$Site, VegC = apply(rc3[,2:5], 1, mean), VegR = apply(rc3[,6:7], 1, mean)) # Produce the average vegetation density for culvert cameras and road cameras
head(rc10); dim(rc10) # Check that this has worked
rc10$Vegdif <- rc10$VegC - rc10$VegR # Calculate the difference of vegetation density between the culvert and road cameras
rc10$VegC <- sort(rc10$VegC) # Reorder
rc10$VegR <- sort(rc10$VegR) # Reorder
rc10Vegdif <- sort(rc10$Vegdif) # Reorder

# If vegdif is positive then there is more vegetation at the culvert, if it is negative then there is more vegetation at the road. 

head(rc10); dim(rc10) # If it is a negative value then there is higher vegetation density at the road than at the culvert, if it is a positive value than there is a higher vegetation density at the culvert than the road.

rc8 <- rc[,c("Site", "Rd_typ", "Cam_typ", "Clvt_sz", "Clvt_wdt", "Clvt_ht", "Length", "Construction", "Nat_ref")]
beh.site <- rc8[, c("Site", "Rd_typ", "Cam_typ", "Clvt_sz", "Clvt_wdt", "Clvt_ht", "Length", "Construction", "Nat_ref")] # Create a site specific data set containing only the information needed to answer the hypotheses
head(beh.site); dim(beh.site) # Check how this looks
beh.site$Rd_typ <- factor(rc$Rd_typ, levels = c("minor", "major")) # Needed to be reordered
str(beh.site$Rd_typ) # Make sure this looks right
str(beh.site$Clvt_sz) # Make sure this look right
beh.site$RC <- 0 # Create a new column for road crossings, and assign 0 to all
beh.site$RC[which(beh.site$Site %in% unique(beh5$Site))] <- 1 # If there was a road crossing either a culvert or road crossing at a site assign a 1, all other sites that had no crossings will remain 0
head(beh.site); dim(beh.site) # Check how this looks
beh.site <- merge(beh.site, rc10, by = "Site", all.x=T, all.y=F) # Merge the two data sets
head(beh.site); dim(beh.site) # Check how this looks
beh.site$Nat_ref <- factor(beh.site$Nat_ref, levels = c("0", "1")) # Make sure that this has been written correctly
str(beh.site$Nat_ref) # Check this has worked
table(beh.site$Site, beh.site$Clvt_sz) # Double check the right sites are nat refs

# Look at the openness ratio as a way to also look at the culvert size instead
head(beh.site) # look at the head of the data

Open <- data.frame(Openness = beh.site$Clvt_wdt*beh.site$Clvt_ht/beh.site$Length) # Create the openness ratio for each site
Open$Site <- 1:30 # Assign the sites to the dataframe
beh.site <- merge(beh.site, Open, by = "Site", all.x = T, all.y = F) # Merge this new column with the beh.site data set
head(beh.site) # Check this has worked


table(beh5$Site, beh5$Behaviour) # How many animals were observed performing each road crossing behaviour?
RC <- data.frame(Site = as.numeric(dimnames(table(beh5$Site, beh5$Behaviour))[[1]]), CC = table(beh5$Site, beh5$Behaviour)[,1], RX = table(beh5$Site, beh5$Behaviour)[,2]) # Create a new data frame containing the information for the number of each road crossing behaviour per site. Merge this with beh.site, and change NAs to 0 if there are any
# Then calculate the proportion of crossings that were under roads, CC/sum(CC+RX), then repeat same process for other groups, use same models below with this new dataset 
head(RC) # What does the Road crossing data look like

# We want to rename these columns to say All.CC or All.RX, so that we know this information is for all animals
names(RC)[names(RC) == "CC"] <- "All.CC"
names(RC)[names(RC) == "RX"] <- "All.RX"

beh.site <- merge(beh.site, RC, by = "Site", all.x = T, all.y = F) # Merge this with beh.site
beh.site[is.na(beh.site)] <- 0 # Replace NA with 0
head(beh.site); dim(beh.site) # Check how this looks
beh.site$Prop_RC_clvt <- beh.site[,"All.CC"]/rowSums(beh.site[,c("All.CC","All.RX")]) # Calculate the proportion of road crossings that were under the road - figure out shy this isn't working,, want proportion for all other models
head(beh.site,3); dim(beh.site) # Check that this has worked.

# We want to do a two part analysis, the first part to determine what if anything effects any sort of road crossings, the second to look at each site and determine what effected road crossings.  0 = all RC were over top of road. 
head(beh5) # Check the head of the data
beh.site$Prop_RC_clvt[beh.site$RC == 0] <- NA # For all animals when site had no RC at all it is now NA, it is 0 if it was a RX over road surface, and 1 if through a culvert. 
head(beh.site); dim(beh.site) # Check that this has worked


behKang <- beh5[which(beh5$Species %in% "Kangaroo"),] # Create the road crossings data for Kangaroos, first finding where they were observed
head(behKang) # See how this looks 
behKang <- tidy.df(data.set = behKang) # Clean up
RCKang <- data.frame(Site = as.numeric(dimnames(table(behKang$Site, behKang$Behaviour))[[1]]), CC = table(behKang$Site, behKang$Behaviour)[,1], RX = table(behKang$Site, behKang$Behaviour)[,2]) # Create road crossing information
head(RCKang); dim(RCKang) # Check how this looks
# Rename columns
names(RCKang)[names(RCKang) == "CC"] <- "Kangaroo.CC"
names(RCKang)[names(RCKang) == "RX"] <- "Kangaroo.RX"
beh.site <- merge(beh.site, RCKang, by = "Site", all.x = T, all.y = F)  # Merge with beh.site
head(beh.site) # Check this worked

beh.site$Prop_Kang_RC_clvt <- beh.site[,"Kangaroo.CC"]/rowSums(beh.site[,c("Kangaroo.CC","Kangaroo.RX")]) # Calculate the propensity for a road crossing 


behExotic <- beh5[which(beh5$Species %in% c("Cat", "Cow", "Dog", "Fox", "Hare")),] # Create the road crossings data for exotics, first finding where they were observed
head(behExotic) # See how this looks
behExotic <- tidy.df(data.set = behExotic) # Clean up
RCExotic <- data.frame(Site = as.numeric(dimnames(table(behExotic$Site, behExotic$Behaviour))[[1]]), CC = table(behExotic$Site, behExotic$Behaviour)[,1], RX = table(behExotic$Site, behExotic$Behaviour)[,2]) # Create road crossing information
head(RCExotic); dim(RCExotic) # Check how this looks
# Rename columns
names(RCExotic)[names(RCExotic) == "CC"] <- "Exotic.CC"
names(RCExotic)[names(RCExotic) == "RX"] <- "Exotic.RX"
beh.site <- merge(beh.site, RCExotic, by = "Site", all.x = T, all.y = F) # Merge with beh.site
head(beh.site) # Check this has worked

beh.site$Prop_Exotic_RC_clvt <- beh.site[,"Exotic.CC"]/rowSums(beh.site[,c("Exotic.CC","Exotic.RX")]) # Calculate the propensity for a road crossing


behNative <- beh5[which(beh5$Species %in% c("Kangaroo", "Macropod", "Monitor", "Possum", "Whiptail")),] # Create the road crossings data for natives, first finding where they were observed
head(behNative) # Check how this looks
behNative <- tidy.df(data.set = behNative) # Clean up
RCNative <- data.frame(Site = as.numeric(dimnames(table(behNative$Site, behNative$Behaviour))[[1]]), CC = table(behNative$Site, behNative$Behaviour)[,1], RX = table(behNative$Site, behNative$Behaviour)[,2]) # Create the road crossings data
head(RCNative); dim(RCNative) # Check this worked
# Rename columns
names(RCNative)[names(RCNative) == "CC"] <- "Native.CC"
names(RCNative)[names(RCNative) == "RX"] <- "Native.RX"
beh.site <- merge(beh.site, RCNative, by = "Site", all.x = T, all.y = F) # Merge with beh.site
head(beh.site) # Check this has worked

beh.site$Prop_Native_RC_clvt <- beh.site[,"Native.CC"]/rowSums(beh.site[,c("Native.CC","Native.RX")]) # Calculate the propensity for a road crossing

head(beh.site) # Check how this looks

beh.site$Clvt_sz <- relevel(beh.site$Clvt_sz, "small") # Make sure this has been organised correctly

# Check for correlations
cor.test(beh.site$VegC, beh.site$Clvt_ht)  # Correlation of culvert veg dense with culvert height
plot(beh.site$VegC, beh.site$Clvt_ht) # Plot this

cor.test(beh.site$VegR, beh.site$Clvt_ht) # correlation of road veg dense with culvert height
plot(beh.site$VegR, beh.site$Clvt_ht) # Plot this

cor.test(beh.site$Vegdif, beh.site$Clvt_ht) # Correlation of veg dense dif with culvert height
plot(beh.site$Vegdif, beh.site$Clvt_ht) # Plot this

cor.test(beh.site$Openness, beh.site$Clvt_ht) # Correlation of culvert openness with culvert height
plot(beh.site$Openness, beh.site$Clvt_ht) # Plot this

cor.test(beh.site$Clvt_wdt, beh.site$Clvt_ht) # Correlation of culvert width with culvert height 
plot(beh.site$Clvt_wdt, beh.site$Clvt_ht) # Plot this

cor.test(beh.site$Length, beh.site$Clvt_ht) # Correlation of culvert length with culvert height
plot(beh.site$Length, beh.site$Clvt_ht) # Plot this

cor.test(ifelse(beh.site$Clvt_sz == "large", 1,0), beh.site$Clvt_ht) 
plot(beh.site$Clvt_sz, beh.site$Clvt_ht)# Plot this
table(beh.site$Site, beh.site$Clvt_sz) # What sizes are the culverts depending on the sites, does this look right? Yes, no issues

cor.test(ifelse(beh.site$Nat_ref == "1", 1, 0), beh.site$Clvt_ht) # For plotting purposes in this case want to keep Nat_ref as a factor variable and just write the code so that we can test the correlation here
plot(beh.site$Nat_ref, beh.site$Clvt_ht) # Plot this




# Plots for appendix
dev.new(width=20, height=20, dpi=80, pointsize=30, noRStudioGD = T)
par(mfrow=c(3, 3), mgp=c(2.5,1,0), mar=c(4,4,3,3))

plot(beh.site$VegC, beh.site$Clvt_ht, xlab = "Vegetation density at culvert (m)", ylab = "Culvert height (m)", las = 1) 
title(main = "(a)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topright", legend = "r = 0.04, p = 0.82", cex = 1, bty ="n", text.width = 0.5)

plot(beh.site$VegR, beh.site$Culvert_ht, xlab = "Vegetation density at road (m)", ylab = "Culvert height (m)", las = 1)
title(main = "(b)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.08, p = 0.68", cex = 1, bty = "n", text.width = 0.4)

plot(beh.site$Vegdif, beh.site$Clvt_ht, xlab = "Vegetation density difference (m)", ylab = "Culvert height (m)", las = 1)
title(main = "(c)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.30, p = 0.09", cex = 1, bty = "n", text.width = 0.52)

plot(beh.site$Openness, beh.site$Clvt_ht, xlab = "Culvert openness ratio", ylab = "Culvert height (m)", las = 1)
title(main = "(d)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.85, p <0.001", cex = 1, bty = "n", text.width = 0.37)

plot(beh.site$Clvt_sz, beh.site$Clvt_ht, xlab = "Culvert size", ylab = "Culvert height (m)", las = 1)
title(main = "(e)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.82, p <0.001", cex = 1, bty = "n", text.width = 1)

plot(beh.site$Clvt_wdt, beh.site$Clvt_ht, xlab = "Culvert width (m)", ylab = "Culvert height (m)", las = 1)
title(main = "(f)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topright", legend = "r = 0.76, p <0.001", cex = 1, bty = "n", text.width = )

plot(beh.site$Length, beh.site$Clvt_ht, xlab = "Culvert Length (m)", ylab = "Culvert height (m)", las = 1)
title(main = "(g)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topright", legend = "r = 0.17, p = 0.36", cex = 1, bty = "n", text.width = 5)

plot(beh.site$Nat_ref, beh.site$Clvt_ht, xaxt = "n", xlab = "Nature refuge", ylab = "Culvert height (m)", las = 1)
axis(side = 1, at = 1:2, labels = c("No", "Yes"))
title(main = "(h)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topright", legend = "r = 0.08, p = 0.66", cex = 1, bty ="n", text.width = 1.5)




## Important models
# All animals
m_null.2 <- glm(Prop_RC_clvt ~ 1 + 1, family = "binomial", data = beh.site)
summary(m_null.2) # Null model for
#What is the Log Likelihood of this model?
logLik(m_null.2)

m11 <- glm(Prop_RC_clvt ~ Clvt_ht, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m11) # Produce culvert height model crossings, Not significant
# Culverts with low heights negatively influence the proportion of crossings observed being through culverts, whilst higher culverts positively influence the proportion of crossings observed being through culverts. Taller culvert = higher proportion of culvert crossings
#What is the Log Likelihood of this model?
logLik(m11)

AICc(m_null.2);AICc(m11) # Null is better

# Kangaroo
m_null.3 <- glm(Prop_Kang_RC_clvt ~ 1 + 1, family = "binomial", data = beh.site)
summary(m_null.3) # Null model 
#What is the Log Likelihood of this model?
logLik(m_null.3)


m19 <- glm(Prop_Kang_RC_clvt ~ Clvt_ht, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m19) # Produce culvert height model, Not significant
# Culvert height as expected has a negative influence on the proportion of kangaroo crossings observed being through culverts if it is low, whereas it is a positive influence as height increases.
#What is the Log Likelihood of this model?
logLik(m19)

AICc(m_null.3);AICc(m19) # Null is better

## Exotic 
m_null.4 <- glm(Prop_Exotic_RC_clvt ~ 1 + 1, family = "binomial", data = beh.site)
summary(m_null.4) # Null model
#What is the Log Likelihood of this model?
logLik(m_null.4)

m27 <- glm(Prop_Exotic_RC_clvt ~ Clvt_ht, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m27) # Produce culver height model, Not significant
# Culvert height has an overall negative influence on the proportion of exotic species crossings observed being through culverts, with shorter culverts having a stronger negative influence. 
#What is the Log Likelihood of this model?
logLik(m27)

AICc(m_null.4);AICc(m27) # Null is better

# Natives
m_null.5 <- glm(Prop_Native_RC_clvt ~ 1 + 1, family = "binomial", data = beh.site)
summary(m_null.5) # Null model 
#What is the Log Likelihood of this model?
logLik(m_null.5)


m35 <- glm(Prop_Native_RC_clvt ~ Clvt_ht, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m35) # Produce culvert height model, Not significant
# Lower culvert heights have a negative influence on the proportion of native species crossings observed being through culverts, whereas increasing culvert height has a positive influence on the proportion of native species crossings observed being through culverts. 
#What is the Log Likelihood of this model?
logLik(m35)

AICc(m_null.5);AICc(m35) # Null is better


# Plots for thesis
dev.new(width=10, height=10, dpi=80, pointsize=20, noRStudioGD = T)
par(mfrow=c(2, 2), mgp=c(2.5,1,0), mar=c(4,4,3,3))

plot(beh.site$Clvt_ht, beh.site$Prop_RC_clvt, pch=20, cex=1.5, las=1, xlab="Culvert height (m)", ylab="Prop. crossing through culvert")
title(main = "(a) All animals", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main =1)

plot(beh.site$Clvt_ht, beh.site$Prop_Exotic_RC_clvt, pch=20, cex=1.5, las=1, xlab="Culvert height (m)", ylab="Prop. crossing through culvert")
title(main = "(b) Exotic Species", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main =1)

plot(beh.site$Clvt_ht, beh.site$Prop_Native_RC_clvt, pch=20, cex=1.5, las=1, xlab="Culvert height (m)", ylab="Prop. crossing through culvert")
title(main = "(c) Native Species", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main =1)

plot(beh.site$Clvt_ht, beh.site$Prop_Kang_RC_clvt, pch=20, cex=1.5, las=1, xlab="Culvert height (m)", ylab="Prop. crossing through culvert")
title(main = expression("(d)" *italic(" Macropus giganteus")), outer = F, adj = 0, cex.main = 1, line = 0.6, font.main =4)







## Although this won't all be used in my thesis, lets take a look at what is actually going on.

beh.site$Prop_RC_clvt # What affects this, start with RC variable


m_null <- glm(RC ~ 1 + 1, family = "binomial", data = beh.site)
summary(m_null) # Null model

m1 <- glm(RC ~ Rd_typ, family = "binomial", data = beh.site)
summary(m1) # Influence of road type on road crossings
# Minor roads strongly negatively influence the probability of observing a road crossings, whereas major roads have a slightly less negative influence on the probability of observing a road crossing - I don't like this measure. 

anova(m_null, m1) # Not significant
AICc(m_null); AICc(m1) # Null model is better

m2 <- glm(RC ~ Length, family = "binomial", data = beh.site)
summary(m2) # Influence of road with and culvert length on road crossings
# Narrower roads have a positive influence on the probability of observing a road crossing, whereas the increasing width of roads has a negative influence on the probability of observing a road crossings
anova(m_null, m2) # Not significant 
AICc(m_null); AICc(m2) # Null model is slightly better

plot(beh.site$Rd_typ, beh.site$Length) # Minor roads are often wider, whereas major roads are often more narrow, road type isn't a great measure 


m3 <- glm(RC ~ VegC, family = "binomial", data = beh.site)
summary(m3) # influence of culvert veg dense on road crossings
# Low vegetation cover at culverts has a negative influence on the probability of observing a road crossing, whereas increasing vegetation cover at culverts has a positive influence on the probability of observing a road crossing. 
anova(m_null, m3) # Not significant
AICc(m_null); AICc(m3) # Null model is better

m4 <- glm(RC ~ VegR, family = "binomial", data = beh.site)
summary(m4) # Influence of road veg dense on road crossings, Not significant
# Lower vegetation cover at road locations has a positive influence on the probability of observing a road crossings, whereas increasing vegetation cover at road locations has a negative influence on the probability of observing a road crossing.
anova(m_null, m4) # Not significant
AICc(m_null); AICc(m4) # Null model is slightly better
range(beh.site$VegR) # What are the different heights, min and max.

m5 <- glm(RC ~ Nat_ref, family = "binomial", data = beh.site)
summary(m5) # Influence of nat ref on road crossings, Not significant 
# Non-nature refuge areas negatively influence the probability of observing a road crossing, whereas nature refuge areas positively influence the probability of observing a road crossing.
anova(m_null, m5) # Not significant
AICc(m_null); AICc(m5) # Better than null model


AICc(m_null); AICc(m1); AICc(m2); AICc(m3); AICc(m4); AICc(m5) # Model 5 (Road crossings ~ Nature Refuge) is a slightly better model than the null model, although all are quite similar


# Animals cross roads with similar frequencies if it is a major or minor road. There was also no significant effect of vegetation height, road width, or the site occurring within a Nature Refuge area. Therefore, when they do cross roads, do any factors influence th propensity of animals to cross roads using a culvert? 



# So Where they do cross, is anything affecting the propensity of culvert crossings
# All animals
head(beh.site) # look at head of the data
m_null.2 <- glm(Prop_RC_clvt ~ 1 + 1, family = "binomial", data = beh.site)
summary(m_null.2) # null model

m6 <- glm(Prop_RC_clvt ~ Rd_typ, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m6) # influence of rd typ
# Minor and major roads negatively influence the proportion of a road crossing through a culvert, lower proportion of crossings through culverts on major roads. 

m7 <- glm(Prop_RC_clvt ~ Vegdif, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m7) # influence of veg dense dif
# The difference in vegetation density has an overall negative influence on the proportion of road crossings through a culvert, with increased vegetation density at roads having a weaker negative influence, although not quite making statistical significance. Whereas increasing vegetation density at the culvert has a stronger negative influence on the proportion of road crossings through a culvert. 

m8 <- glm(Prop_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m8)# influence of culvert size
# Small culverts negatively influences the proportion of crossings being through culverts, whilst larger culverts positively influences the proportion of crossings being through culverts. Therefore larger culverts are more likely to be used to cross roads than small culverts by all species. 


m9 <- glm(Prop_RC_clvt ~ Openness, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m9) # influence of culvert openness
# Not significant 
# The openness of the culvert does have an overall negative influence on the proportion of crossings observed being through a culvert, although increasing openness of the culvert reduces the negative influence. More open culverts are more likely to be used to cross roads. 

m10 <- glm(Prop_RC_clvt ~ Clvt_wdt, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m10) # influence of culvert width
# Not significant
# Culvert width has a negative influence on the proportion of crossings observed being through culverts, although increasing culvert width reduces this negative influence. Therefore wider culverts = higher proportion of crossings through culverts. 

m11 <- glm(Prop_RC_clvt ~ Clvt_ht, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m11) # influence of culvert height
# Not significant
# Culverts with low heights negatively influence the proportion of crossings observed being through culverts, whilst higher culverts positively influence the proportion of crossings observed being through culverts. Taller culvert = higher proportion of culvert crossings

m12 <- glm(Prop_RC_clvt ~ Length, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m12) # influence of culvert length
# Not significant
# Culvert length has a negative influence on the proportion of crossings observed being through culverts, although as culvert length increases, the negative influence of length decreases. Longer culverts = higher proportion of culvert crossings due to wider road animals more likely to cross through culvert 

m13 <- glm(Prop_RC_clvt ~ Nat_ref, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m13) # influence of nat ref
# Not significant 
# Non-nature refuge areas have a negative influence on the proportion of crossings observed being through culverts, therefore in non-nature refuge areas it is unlikely to observe a culvert crossing. Whereas animals in nature refuges are positively influenced by the presence of culverts, proportion of culvert crossings is positively influenced by nature refuge areas. 


AICc(m_null.2); AICc(m6); AICc(m7); AICc(m8); AICc(m9); AICc(m10); AICc(m11); AICc(m12); AICc(m13)
# The null model is the worst, all models are quite similar, with model 9, openness ratio the best model to explain the proportion of CC.




## Kangaroo

m_null.3 <- glm(Prop_Kang_RC_clvt ~ 1 + 1, family = "binomial", data = beh.site)
summary(m_null.3) # Null model

m14 <- glm(Prop_Kang_RC_clvt ~ Rd_typ, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m22) # influence of road type
# Not significant

m15 <- glm(Prop_Kang_RC_clvt ~ Vegdif, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m15) # influence of veg dense dif
# Not significant
# The difference in vegetation height between culvert and road locations has a negative influence on the proportion of kangaroo crossings through culverts, with increasing vegetation density at the culvert having a stronger negative influence. 

m16 <- glm(Prop_Kang_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m16) # influence of culvert size
# Not significant
# Small culverts have a negative influence on the proportion of kangaroo crossings observed being through culverts, due to the size of the culvert not accommodating the body size of the kangaroo, therefore larger culverts have a positive influence on the proportion of kangaroo crossings observed being through culverts.


m17 <- glm(Prop_Kang_RC_clvt ~ Openness, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m17)  # influence of culvert openness
# Not significant
# Culvert openness has a negative influence on the proportion of kangaroo crossings observed being through culverts, with increasing openness having a stronger negative influence. As discussed before this may be due to Kangaroos often being observed using circular culverts rather than rectangular culverts which may have been shorter more often therefore not accommodating the kangaroos size. 

m18 <- glm(Prop_Kang_RC_clvt ~ Clvt_wdt, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m18) # influence of culvert width
# Not significant
# As above increasing culvert width has a negative influence on the proportion of kangaroo crossings observed being through culverts, whereas if the culvert is not very wide there was a positive influence, likely due to Kangaroos often crossing through circular culverts. 

m19 <- glm(Prop_Kang_RC_clvt ~ Clvt_ht, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m19) # influence of culvert height
# Not significant
# Culvert height as expected has a negative influence on the proportion of kangaroo crossings observed being through culverts if it is low, whereas it is a positive influence as height increases.

m20 <- glm(Prop_Kang_RC_clvt ~ Length, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m20) # influence of culvert length/road width
# Not significant
# Shorter culverts/narrower roads had a negative influence on the proportion of kangaroo crossings observed being through culverts, on narrower roads with shorter culverts kangaroos are more often observed crossing roads directly. Whereas as road width/culvert length increases there is a positive influence on the proportion of kangaroo crossings observed being through culverts, therefore more likely to use culverts on wider roads. 


m21 <- glm(Prop_Kang_RC_clvt ~ Nat_ref, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m21) # influence of nat ref
# Not significant
# Non-nature refuge areas has a negative influence on the proportion of kangaroo crossings observed being through culverts, so if observed in these areas they are likely to cross roads directly whereas in nature refuge areas there is a positive influence, therefore they are more likely to use culverts in nature refuge areas. 



AICc(m_null.3); AICc(m14); AICc(m15); AICc(m16); AICc(m17); AICc(m18); AICc(m19); AICc(m20); AICc(m21)
# All AICs are similar



## Exotic 
m_null.4 <- glm(Prop_Exotic_RC_clvt ~ 1 + 1, family = "binomial", data = beh.site)
summary(m_null.4) # Null model

m22 <- glm(Prop_Exotic_RC_clvt ~ Rd_typ, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m22) # influence of road type
# Minor roads have a negative influence on the proportion of exotic species crossings observed being through culverts, whereas major roads have a positive influence on the proportion of exotic species crossings observed being through culverts. Therefore exotic species use culverts to cross roads often only when on a major road. 

m23 <- glm(Prop_Exotic_RC_clvt ~ Vegdif, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m23)  # influence of veg dense difference
# Intercept is significant, not the point we want to be significant
# Increasing vegetation at road locations has a negative influence on the proportion of exotic species crossings observed being through culverts, although not quite statistically significant. Whereas increasing vegetation density at the culvert has a positive influence on the proportion of exotic species crossing observed being through culverts. 

m24 <- glm(Prop_Exotic_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m24) # influence of culvert size
# Not significant
# Culvert size has an overall negative influence on the proportion of exotic species crossings observed being through culverts, although smaller culverts has a stronger influence. Therefore exotic species are more likely to use larger culverts. 


m25 <- glm(Prop_Exotic_RC_clvt ~ Openness, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m25) # influence of culvert openness
# Intercept is significant, not the point we want to be significant
# Lower openness of the culverts has a negative influence on the proportion of exotic species crossings observed being through culverts, although not quite statistically significant. Whereas increasing culvert openness has a positive influence on the proportion of crossings observed being through culverts. Therefore exotic species are more likely to be observed using more open/spacious culverts

m26 <- glm(Prop_Exotic_RC_clvt ~ Clvt_wdt, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m26) # influence of culvert width
# Not significant
# Lower culvert width has a negative influence on the proportion of exotic species crossings observed being through culverts, whereas increasing culvert width has a positive influence on the proportion of exotic species crossings being through culverts. 

m27 <- glm(Prop_Exotic_RC_clvt ~ Clvt_ht, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m27) # influence of culvert width
# Not significant
# Culvert height has an overall negative influence on the proportion of exotic species crossings observed being through culverts, with shorter culverts having a stronger negative influence. 

m28 <- glm(Prop_Exotic_RC_clvt ~ Length, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m28) # influence of culvert length/road width
# Not significant
# Shorter culverts have a positive influence on the proportion of exotic species crossings observed being through culverts, whereas increasing culvert length has a negative influence on the proportion of exotic species crossings observed being through culverts. Prefer to only use shorter culverts 

m29 <- glm(Prop_Exotic_RC_clvt ~ Nat_ref, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m29)# influence of nat ref
# Intercept is significant, not the point we want to be significant
#Non-nature refuge areas have a negative influence on the proportion of exotic species crossings observed being through culverts, although not quite statistically significant. Whereas culverts in nature refuge areas have a positive influence on the proportion of exotic species crossings observed being through culverts.



AICc(m_null.4); AICc(m22); AICc(m23); AICc(m24); AICc(m25); AICc(m26); AICc(m27); AICc(m28); AICc(m29)
# All AICs are similar



## Native 
m_null.5 <- glm(Prop_Native_RC_clvt ~ 1 + 1, family = "binomial", data = beh.site)
summary(m_null.5) # Null model

m30 <- glm(Prop_Native_RC_clvt ~ Rd_typ, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m30) # influence of road type
# Not significant

m31 <- glm(Prop_Native_RC_clvt ~ Vegdif, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m31) # influence of veg dense difference
# Not significant
# The difference in vegetation height between culverts and road locations has an overall negative influence on the proportion of native species crossings observed being through culverts, with increasing vegetation densities at culverts having a stronger negative influence. 

m32 <- glm(Prop_Native_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m32) # influence of culvet size
# Not significant
# Small culverts have a negative influence on the proportion of native species crossings observed being through culverts, whereas large culverts have a positive influence on the proportion of native species crossings observed being through culverts.


m33 <- glm(Prop_Native_RC_clvt ~ Openness, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m33) # influence of culvert openness
# Not significant
# Low culvert openness has a negative influence on the proportion of native species crossings observed being through culverts, whereas higher openness has a positive influence on the proportion of native species crossing observed being through culverts. 

m34 <- glm(Prop_Native_RC_clvt ~ Clvt_wdt, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m34) # influence of culvert width
# Not significant
# Culvert width has an overall negative influence on the proportion of native species crossings observed being through culverts, with increasing culvert width having a weaker negative influence. 

m35 <- glm(Prop_Native_RC_clvt ~ Clvt_ht, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m35) # influence of culvert height
# Not significant
# Lower culvert heights have a negative influence on the proportion of native species crossings observed being through culverts, whereas increasing culvert height has a positive influence on the proportion of native species crossings observed being through culverts. 

m36 <- glm(Prop_Native_RC_clvt ~ Length, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m36) # influence of culvert length/road width
# Not significant
# Shorter culverts have a negative influence on the proportion of native species crossings observed being through culverts, whereas increasing culvert length has a positive influence on the proportion of native species crossings observed being through culverts. Native species tend to use culverts when roads are wider. 

m37 <- glm(Prop_Native_RC_clvt ~ Nat_ref, data = beh.site[which(beh.site$RC == 1),], family = "binomial")
summary(m37) # influence of nat ref
# Not significant
# Non-nature refuge areas have a positive influence on the proportion of native species crossings observed being through culverts, whereas, interestingly, nature refuge areas have a negative influence on the proportion of native species crossings observed being through culverts. So when looking at all native species they are more likely to use culverts outside of nature refuges but within them they tend to cross roads directly. 


AICc(m_null.5); AICc(m30); AICc(m31); AICc(m32); AICc(m33); AICc(m34); AICc(m35); AICc(m36); AICc(m37)
# All AICs are similar



## Produce simple plots that won't be included because they are correlated variables but shows what was happening

# All animals
dev.new(width=20, height=20, dpi=80, pointsize=20, noRStudioGD = T)
par(mfrow=c(4, 2), mgp=c(2.5,1,0), mar=c(4,4,3,3))

plot(beh.site$Vegdif, beh.site$Prop_RC_clvt, pch=20, cex=1.5, las=1, xlab="Difference of vegetation height", ylab="Prop. crossing through culvert")
title(main = "(a)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Clvt_sz, beh.site$Prop_RC_clvt, pch=20, cex=1.5, las=1, xlab="Culvert size", ylab="Prop. crossing through culvert")
title(main = "(b)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Openness, beh.site$Prop_RC_clvt, pch=20, cex=1.5, las=1, xlab="Culvert openness", ylab="Prop. crossing through culvert")
title(main = "(c)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Clvt_wdt, beh.site$Prop_RC_clvt, pch=20, cex=1.5, las=1, xlab="Width of culvert", ylab="Prop. crossing through culvert")
title(main = "(d)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Clvt_ht, beh.site$Prop_RC_clvt, pch=20, cex=1.5, las=1, xlab="Height of culvert", ylab="Prop. crossing through culvert")
title(main = "(e)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Length, beh.site$Prop_RC_clvt, pch=20, cex=1.5, las=1, xlab="Length of culvert", ylab="Prop. crossing through culvert")
title(main = "(f)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(as.factor(beh.site$Nat_ref), beh.site$Prop_RC_clvt, pch=20, cex=1.5, las=1, xaxt = "n", xlab="Nature refuge area", ylab="Prop. crossing through culvert") 
axis(side = 1, at = 1:2, labels = c("Yes", "No"))
title(main = "(g)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)

mtext("Prop. All animal crossings through culvert", outer=TRUE, cex=1, line=-1.1)


# Kangaroos
dev.new(width=20, height=20, dpi=80, pointsize=20, noRStudioGD = T)
par(mfrow=c(4, 2), mgp=c(2.5,1,0), mar=c(4,4,3,3))

plot(beh.site$Vegdif, beh.site$Prop_Kang_RC_clvt, pch=20, cex=1.5, las=1, xlab="Difference of vegetation height", ylab="Prop. crossing through culvert")
title(main = "(a)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Clvt_sz, beh.site$Prop_Kang_RC_clvt, pch=20, cex=1.5, las=1, xlab="Culvert size", ylab="Prop. crossing through culvert")
title(main = "(b)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Openness, beh.site$Prop_Kang_RC_clvt, pch=20, cex=1.5, las=1, xlab="Culvert openness", ylab="Prop. crossing through culvert")
title(main = "(c)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Clvt_wdt, beh.site$Prop_Kang_RC_clvt, pch=20, cex=1.5, las=1, xlab="Width of culvert", ylab="Prop. crossing through culvert")
title(main = "(d)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Clvt_ht, beh.site$Prop_Kang_RC_clvt, pch=20, cex=1.5, las=1, xlab="Height of culvert", ylab="Prop. crossing through culvert")
title(main = "(e)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Length, beh.site$Prop_Kang_RC_clvt, pch=20, cex=1.5, las=1, xlab="Length of culvert", ylab="Prop. crossing through culvert")
title(main = "(f)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(as.factor(beh.site$Nat_ref), beh.site$Prop_Kang_RC_clvt, pch=20, cex=1.5, las=1, xaxt = "n", xlab="Nature refuge area", ylab="Prop. crossing through culvert") 
axis(side = 1, at = 1:2, labels = c("Yes", "No"))
title(main = "(g)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)

mtext("Prop. Kangaroos crossing through culvert", outer=TRUE, cex=1, line=-1.1)


# Exotics
dev.new(width=20, height=20, dpi=80, pointsize=20, noRStudioGD = T)
par(mfrow=c(4, 2), mgp=c(2.5,1,0), mar=c(4,4,3,3))

plot(beh.site$Vegdif, beh.site$Prop_Exotic_RC_clvt, pch=20, cex=1.5, las=1, xlab="Difference of vegetation height", ylab="Prop. crossing through culvert")
title(main = "(a)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Clvt_sz, beh.site$Prop_Exotic_RC_clvt, pch=20, cex=1.5, las=1, xlab="Culvert size", ylab="Prop. crossing through culvert")
title(main = "(b)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Openness, beh.site$Prop_Exotic_RC_clvt, pch=20, cex=1.5, las=1, xlab="Culvert openness", ylab="Prop. crossing through culvert")
title(main = "(c)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Clvt_wdt, beh.site$Prop_Exotic_RC_clvt, pch=20, cex=1.5, las=1, xlab="Width of culvert", ylab="Prop. crossing through culvert")
title(main = "(d)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Clvt_ht, beh.site$Prop_Exotic_RC_clvt, pch=20, cex=1.5, las=1, xlab="Height of culvert", ylab="Prop. crossing through culvert")
title(main = "(e)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Length, beh.site$Prop_Exotic_RC_clvt, pch=20, cex=1.5, las=1, xlab="Length of culvert", ylab="Prop. crossing through culvert")
title(main = "(f)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(as.factor(beh.site$Nat_ref), beh.site$Prop_Exotic_RC_clvt, pch=20, cex=1.5, las=1, xaxt = "n", xlab="Nature refuge area", ylab="Prop. crossing through culvert") 
axis(side = 1, at = 1:2, labels = c("Yes", "No"))
title(main = "(g)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)

mtext("Prop. Exotic species crossing through culvert", outer=TRUE, cex=1, line=-1.1)


# Natives
dev.new(width=20, height=20, dpi=80, pointsize=20, noRStudioGD = T)
par(mfrow=c(4, 2), mgp=c(2.5,1,0), mar=c(4,4,3,3))

plot(beh.site$Vegdif, beh.site$Prop_Native_RC_clvt, pch=20, cex=1.5, las=1, xlab="Difference of vegetation height", ylab="Prop. crossing through culvert")
title(main = "(a)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Clvt_sz, beh.site$Prop_Native_RC_clvt, pch=20, cex=1.5, las=1, xlab="Culvert size", ylab="Prop. crossing through culvert")
title(main = "(b)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Openness, beh.site$Prop_Native_RC_clvt, pch=20, cex=1.5, las=1, xlab="Culvert openness", ylab="Prop. crossing through culvert")
title(main = "(c)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Clvt_wdt, beh.site$Prop_Native_RC_clvt, pch=20, cex=1.5, las=1, xlab="Width of culvert", ylab="Prop. crossing through culvert")
title(main = "(d)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Clvt_ht, beh.site$Prop_Native_RC_clvt, pch=20, cex=1.5, las=1, xlab="Height of culvert", ylab="Prop. crossing through culvert")
title(main = "(e)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(beh.site$Length, beh.site$Prop_Native_RC_clvt, pch=20, cex=1.5, las=1, xlab="Length of culvert", ylab="Prop. crossing through culvert")
title(main = "(f)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)
plot(as.factor(beh.site$Nat_ref), beh.site$Prop_Native_RC_clvt, pch=20, cex=1.5, las=1, xaxt = "n", xlab="Nature refuge area", ylab="Prop. crossing through culvert") 
axis(side = 1, at = 1:2, labels = c("Yes", "No"))
title(main = "(g)", outer = F, adj = 0, cex.main = 0.8, line = 0.3)


mtext("Prop. Native species crossing through culvert", outer=TRUE, cex=1, line=-1.1)




### Discovery - It seems that there are no factors which influence the likelihood or propensity for a road surface crossing, whereas for culvert crossings.
