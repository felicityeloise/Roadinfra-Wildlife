# Written by Felicity Charles and Annabel Smith 5/2/2021
# Updated post peer review 16/05/2023 
# Caveat emptor

# This script deals with the analysis of the occurrence data from the study of the use of road infrastructure by terrestrial vertebrates. 
# 1. Read in data ----
dat <- read.table("./00_Data/Occurrence.txt", header = T, stringsAsFactors = T) # Read the occurrence data into R, ensuring that factor variables are assigned
str(dat) # Check the structure of dat
head(dat) # Check the first few rows of dat

rc <- read.table("./00_Data/CharacteristicsR.txt", header = T, stringsAsFactors = T) # Read the site characteristics data into R, ensuring that factor variables are assigned  



# 2.1 Check the levels of each column of the occurrence data ----
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


# 2.2 Check the levels of each column of the site characteristics data
levels(rc$Suburb)
levels(rc$Road)
levels(rc$Rd_typ)
rc$Rd_typ <- factor(rc$Rd_typ, levels = c("minor", "major"))# Reorder rd_typ
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


# 3. Load required packages ----
library(mgcv)
library(dplyr)
library(tidyr)
library(lmerTest) 
library(lme4)
library(AICcmodavg)
library(tidyverse)

## 4. Exploring occurrence data ----
colSums(dat[, which(colnames(dat) == "Kangaroo"): (ncol(dat)-1)]) # What are the total number of animals observed for each species, change "Kangaroo" to other species


# 4.1 Create functional group categories ----
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


## 5.1 Stage 1 - analysis of occurrence data ----
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


# 5.2 Turn count data into presence/absence data -----
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




# 6. How do environmental variables influence occurrence data? -----
rc[is.na(rc)] <- 0 # Replace NA with 0
head(rc) # Check that this worked

# Calculate averages of vegetation height (vegetation density from now on) per camera point
rc$C1 <- rowMeans(rc[ , c(14, 15, 16)], na.rm = TRUE )
rc$C2 <- rowMeans(rc[ , c(18, 19, 20)], na.rm = TRUE)
rc$C3 <- rowMeans(rc[ , c(22, 23, 24)], na.rm = TRUE)
rc$C4 <- rowMeans(rc[ , c(26, 27, 28)], na.rm = TRUE)
rc$R1 <- rowMeans(rc[ , c(30, 31, 32)], na.rm = TRUE)
rc$R2 <- rowMeans(rc[ , c(34, 35, 36)], na.rm = TRUE)
rc <- rc %>% 
  mutate_if(is.numeric, round, digits = 2)
head(rc) # Ensure that this has worked


# 6.1 Let's get the characteristics data into a workable format ----
# 6.1.2 Extract the vegetation density information into a format that can be combined with dat ----
rc2 <- rc[,c("Site", "C1", "C2", "C3", "C4", "R1", "R2")]
rc2 <- gather(rc2, key = "Cam_pt", value = "Veg_dense", "C1", "C2", "C3", "C4", "R1", "R2")
rc2$Veg_dense <- sort(rc2$Veg_dense) # Order from lowest to highest


# 6.1.3 Extract the vegetation cover information into a format that can be combined with dat ----
rc3 <- rc[,c("Site", "Veg_cov_C1","Veg_cov_C2","Veg_cov_C3", "Veg_cov_C4", "Veg_cov_R1", "Veg_cov_R2")] # Create a new dataset that only contains the information for site and vegetation 
rc3 <- gather(rc3, key = "Cam_pt", value = "Veg_cov", "Veg_cov_C1", "Veg_cov_C2", "Veg_cov_C3", "Veg_cov_C4", "Veg_cov_R1", "Veg_cov_R2")
rc3$Cam_pt[1:30] <- "C1"
rc3$Cam_pt[31:60] <- "C2"
rc3$Cam_pt[61:90] <- "C3"
rc3$Cam_pt[91:120] <- "C4"
rc3$Cam_pt[121:150] <- "R1"
rc3$Cam_pt[151:180] <- "R2"
rc3 # How does this look?
rc3$Veg_cov <- factor(rc3$Veg_cov, levels = c("none", "low", "medium", "high")) # Reorder veg_cov


# 6.2 Now that all the data is formatted correctly, add information to dat ----
# 6.2.1 Firstly lets get information that is the same for a whole site and add it to dat -----
rc4 <- rc[,c("Site", "Rd_typ", "Length", "Clvt_ht")] # Create a new dataset that only contains the information for site and road type
head(rc4); dim(rc4) # Check this worked, there are only 30 sites
which(duplicated(rc4$Site)) # Although this is correct, double check that there are no duplications just in case
dat2 <- merge(dat, rc4, by = "Site", all.x = T, all.y = F) # Merge the data in the new site characteristics dataset with dat2 according to Site. 


# 6.2.2 Add the vegetation density data to dat ----
dat2 <- merge(dat2, rc2, by = c("Site", "Cam_pt"), all.x = T, all.y = F) # Merge the data in the new site characteristics dataset with dat2 according to Site and Camera point. 
head(dat2) # Check that this has worked 

# 6.2.3 Add the vegetation cover data to dat ----
dat2 <- merge(dat2, rc3, by = c("Site", "Cam_pt"), all.x = T , all.y = F) # Merge the data in the new site characteristics dataset with dat2 according to Site and Camera point.
head(dat2); dim(dat2)



# How does road type influence road length? For future reference only
table(dat2$Length, dat2$Site) 
Site <- 1:30
Road <- data.frame(Site)
Road$Rd_typ <- c(rep("major", 2), rep("minor", 1), rep("major", 3), rep("minor", 2), rep("major", 16), rep("minor", 3), rep("major", 3))
Road$Length <- c(rep("8.15", 1), rep("8.4",1), rep("7.1",1), rep("9.65", 1), rep("7.95", 1), rep("10",1), rep("11.05",1), rep("11.1",1), rep("7.3",1), rep("9.25",1), rep("9.55",1), rep("9.85",1), rep("9.35",1), rep("9.75",1), rep("7.15",1), rep("12.1",1), rep("9.9",1), rep("9.35",1), rep("11.2",1), rep("7,4",1), rep("6.65",1), rep("14.8",1), rep("9.8",1), rep("9.35",1), rep("12.6",1), rep("8.9",1), rep("9.3",1), rep("10.6",1), rep("12.9",1), rep("11.25",1)) # Need to do this otherwise it wasn't ordering correctly
table(Road$Length, Road$Rd_typ) # Minor roads are not necessarily narrower roads or roads with shorter culverts, some major roads have shorter culverts on narrower roads. Road type isn't really a great indicator for road size in this instance.




# 7. Test for correlated variables to simplify model set ----
head(dat)

cor.test(dat2$Length, dat2$Veg_dense) # Correlated, more veg dense on wider road
# road length and vegetation density were correlated (r = 0.23, p = 0.001)
rl <- lmer(Veg_dense ~ Length + (1 |Site), data = dat2)
summary(rl) # Summarise the relationship between veg dense and length
plot(dat2$Length, dat2$Veg_dense) # How does this look as a plot

# NOTE - Vegetation cover was not included as we preferred to use the numerical version of vegetation measurements - vegetation density

cor.test(ifelse(dat2$Rd_typ== "major",1,0), dat2$Veg_dense) # Check correlation of veg dense with rd type
summary(lm(Veg_dense ~ Rd_typ, data = dat2)) # Relationship between veg dense and road type - road type isn't really giving us the information that we are wanting, we will just stick with road width/culvert length


summary(lm(Veg_dense ~ Length, data = dat2)) # Relationship between veg dense and road width 


Cs <- lmer(Veg_dense ~ Clvt_sz + (1 | Site), data = dat2) # Relationship of veg dense with culvert size
summary(Cs) # Not sig.
cor.test(ifelse(dat2$Clvt_sz == "large", 1,0), dat2$Veg_dense) # 0.04, p=0.62



# Vegetation density is correlated with the other variables so will be used in their place. However, it is not correlated with culvert size, which we are also interested in determining the influence of culvert size on observing wildlife so models for this variable will also be produced. 

# Need to test the correlation of culvert size with other variables

cor.test(ifelse(dat2$Clvt_sz == "large", 1,0), dat2$Veg_dense) # 0.04, p=0.622

cor.test(ifelse(dat2$Clvt_sz == "large", 1,0), dat2$Length) # 0.024, p=0.747

cor.test(ifelse(dat2$Clvt_sz == "large",1,0), ifelse(dat2$Rd_typ == "major", 1,0)) #-0.09, p=0.199


# Make the Helvetica fonts are available to R
quartzFonts(helvetica = c("Helvetica Neue Light", 
                          "Helvetica Neue Bold", "Helvetica Neue Light Italic", 
                          "Helvetica Neue Bold Italic"))





## Create the plots for appendix for correlated variables in the occurrence data ----
dev.new(width=20, height=20, dpi=80, pointsize=28, noRStudioGD= F) # Save as image 900x900
par(mfrow=c(2, 2), mgp=c(2.5,1,0), mar=c(4,4,3,3), cex = 1, las = 1, family = 'helvetica')


plot(dat2$Length, dat2$Veg_dense, xlab = "Culvert length (m)", ylab = "Vegetation density (m)", las = 1) # As vegetation density increases so does length
title(main = "(a)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.23, p = 0.001", cex = 1, bty ="n", text.width = 1)


plot(dat2$Clvt_sz, dat2$Veg_dense, xlab = "Culvert size", ylab = "Vegetation density (m)", las = 1)
title(main = "(b)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("top", legend = "r = 0.03, 
p = 0.62", cex = 1, bty ="n", text.width =1)

plot(dat2$Clvt_sz, dat2$Length, xlab = "Culvert size", ylab = "Road width/Culvert length (m)", las = 1)
title(main = "(c)", outer = F, adj = 0, cex.main = 1, line = 0.3)
legend("topleft", legend = "r = 0.02, p = 0.74", cex = 1, bty = "n", text.width = 1)



# 8. How does vegetation density influence the occurrence data? ----

# 8.1 Exotic species ----
# Model the probability of observing an exotic animal in relation to vegetation density
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



AICc(Mnull); AICc(M3); AICc(M3a); AICc(M3b) # Null model is better, but M3a the size only model is second best

AICc(Mnull); AICc(M1); AICc(M1a); AICc(M1b); AICc(M2); AICc(M3); AICc(M3a); AICc(M3b) # Null model is best, M1a is best model for veg dense which is quite similar to the type only model

cand.set <- list(Mnull,M1,M1a, M1b, M2, M3, M3a, M3b)

aictab(cand.set)




# 8.2 Native species  -----

Mnull.1 <- glmer(Native.pa ~ 1 + (1 | Site), family = "binomial", data = dat2)
summary(Mnull.1) # What is happening if nothing was occurring
#What is the Log Likelihood of this model?
logLik(Mnull.1)


M4 <- glmer(Native.pa ~ Type * Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M4) # Produce interactive model, Significant
#What is the Log Likelihood of this model?
logLik(M4)


M4a <- glmer(Native.pa ~ Veg_dense + (1 | Site), family = "binomial", data = dat2)
summary(M4a) # Model veg dense only

#What is the Log Likelihood of this model?
logLik(M4a)


M4b <- glmer(Native.pa ~ Type + Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M4b) # Produce additive model, Significant 
#What is the Log Likelihood of this model?
logLik(M4b)


M5 <- glmer(Native.pa ~ Type + (1|Site), family = "binomial", data = dat2)
summary(M5) # Model type only, it is significant although the AIC is not within 2 of the other two models
#What is the Log Likelihood of this model?
logLik(M5) 
AICc(Mnull.1); AICc(M5) # Better than null


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


# Best model overall?
AICc(Mnull.1); AICc(M4); AICc(M4a); AICc(M4b); AICc(M5); AICc(M6); AICc(M6a); AICc(M6b) # M4b is the best model, Vegetation density additive model

cand.set <- list(Mnull.1,M4,M4a, M4b, M5, M6, M6a, M6b)

aictab(cand.set)



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



# Predict from Model 4b
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




# 8.3 Kangaroos ---- 
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

AICc(Mnull.2); AICc(M7); AICc(M7a); AICc(M7b); AICc(M8); AICc(M9); AICc(M9a); AICc(M9b) # Null model is best.

cand.set <- list(Mnull.2,M7,M7a, M7b, M8, M9, M9a, M9b)

aictab(cand.set)




## 8.4 All animals -----
Mnull.3 <- glmer(All_animals.pa ~ 1 + (1|Site), family = "binomial", data = dat2)
summary(Mnull.3) # Produce null model
#What is the Log Likelihood of this model?
logLik(Mnull.3)

M10 <- glmer(All_animals.pa ~ Type * Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M10) # Produce interactive model, Significant
#What is the Log Likelihood of this model?
logLik(M10)


M10a <- glmer(All_animals.pa ~ Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M10a) # Produce veg dense only model
#What is the Log Likelihood of this model?
logLik(M10a)

M10b <- glmer(All_animals.pa ~ Type + Veg_dense + (1|Site), family = "binomial", data = dat2)
summary(M10b) # Produce additive model, Significant
#What is the Log Likelihood of this model?
logLik(M10b)

# Lower vegetation densities at culvert and road locations had a positive influence on the probability of observing an animal, whereas increasing vegetation densities had a negative influence on the probability of observing an animal. 
anova(Mnull.3, M10) # Significant



M11 <- glmer(All_animals.pa ~ Type + (1|Site), family = "binomial", data = dat2)
summary(M11) # Produce the type only model, Not quite significant
# Culverts and road locations both positively influence the probability of observing an animal, although road cameras are more likely to observe an animal than culvert cameras. 
#What is the Log Likelihood of this model?
logLik(M11)

anova(Mnull.3, M11) # Not significant



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



AICc(Mnull.3); AICc(M10); AICc(M10a); AICc(M10b); AICc(M11); AICc(M12); AICc(M12a); AICc(M12b) # The additive vegetation model is best M10b

cand.set <- list(Mnull.3,M10,M10a, M10b, M11, M12, M12a, M12b)

aictab(cand.set)



# Predict from the interactive model
ndM10 <- data.frame(Type = as.factor(c(rep("C", 50), rep("R", 50))), Veg_dense = seq(min(dat2$Veg_dense), max(dat2$Veg_dense), length.out = 50))
prM10 <- predictSE(mod = M10, newdata = ndM10, se.fit = T, type = "response")
prM10 <- data.frame(ndM10, fit = round(prM10$fit, 4), se = round(prM10$se.fit, 4))
prM10$lci <- prM10$fit - (prM10$se * 1.96)
prM10$uci <- prM10$fit + (prM10$se * 1.96)

# Plot predictions from the interactive model
plot(prM10$Veg_dense[prM10$Type=="C"], prM10$fit[prM10$Type=="C"], pch=20, ylim=c(min(prM10$lci), max(prM10$uci)), xlab = "Veg. density", ylab = "Prob. of occurrence", type="l")
lines(prM10$Veg_dense[prM10$Type=="C"], prM10$lci[prM10$Type=="C"], lty=2)
lines(prM10$Veg_dense[prM10$Type=="C"], prM10$uci[prM10$Type=="C"], lty=2)
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$fit[prM10$Type=="R"], lty=1, col="red")
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$lci[prM10$Type=="R"], lty=2, col ="red")
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$uci[prM10$Type=="R"], lty=2, col ="red")


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




## 8.5 Large species ----
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


M15b <- glmer(Large.pa ~ Type + Clvt_sz + (1 | Site), family = "binomial", data = dat2)# Produce additive model
summary(M15b) # Significant negative influence of small culverts on large species occurrences


AICc(Mnull.4); AICc(M13); AICc(M13a); AICc(M13b); AICc(M14); AICc(M15); AICc(M15a); AICc(M15b) # The size only model is the best model M15a
cand.set <- list(Mnull.4,M13,M13a, M13b, M14, M15, M15a, M15b)

aictab(cand.set)

# Predict from the size only model
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



## 8.6 Small species ----
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

anova(Mnull.5, M17) # Significant


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


AICc(Mnull.5); AICc(M16); AICc(M16a); AICc(M16b); AICc(M17); AICc(M18); AICc(M18a); AICc(M18b) # M17, the type only model is best.


cand.set <- list(Mnull.5,M16,M16a, M16b, M17, M18, M18a, M18b)

aictab(cand.set)

# Predict from the type only model
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






## 9. Plots for probability of occurrence ----

dev.new(width=12, height=10, res = 300, dpi=80, pointsize=18, noRStudioGD = F) # Save as image as 850 X 700 - fig2
par(mfrow=c(2,2), mgp=c(2.5,1,0), mar=c(4,4,2,2), oma=c(0,0,0,6), cex = 1.5, las =1, family = 'helvetica')

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


plot(c(1,2), prM17$fit[c(1,3)], pch = 20, ylim=c(0,1), xlim=c(0.5,2.5), xlab = "Camera position", ylab = "Probability of occurrence", type = "p", xaxt="n", col = c("black", "red"))
axis(side=1, at=c(1,2), labels=c("culvert","road"))
arrows(c(1,2), prM17$lci[c(1,3)], c(1,2), prM17$uci[c(1,3)], length = 0.05, code=3, angle=90, col = c("black", "red"))
title(main = "(c) Small species", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main = 1)

plot(c(1,2), prM15a$fit[c(1,3)], pch = 20, ylim=c(0,1), xlim=c(0.5,2.5), xlab = "Culvert size", ylab = "Probability of occurrence", type = "p", xaxt="n")
axis(side=1, at=c(1,2), labels=c("small","large"))
arrows(c(1,2), prM15a$lci[c(1,3)], c(1,2), prM15a$uci[c(1,3)], length = 0.05, code=3, angle=90)
title(main = "(d) Large species", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main = 1)


par(xpd=NA)
legend(x=2.5, y=2.8, legend=c("Culvert", "Road"), col=c("black", "red"), lty=1, lwd=0.5, cex=1, bty="n", text.width=0.2, title = "Camera position")
par(xpd=F)







# 10. Plots for appendix ----

# These are interactive models that were better than the null model and within AICc2 of the additive model 
dev.new(width=12, height=10, dpi=80, pointsize=18, noRStudioGD = T) # Save as eps 850 X 700 - fig a5
par(mfrow=c(2,2), mgp=c(2.5,1,0), mar=c(4,4,2,2), oma=c(0,0,0,6), cex = 1, las =1, family = 'helvetica')


plot(prM10$Veg_dense[prM10$Type=="C"], prM10$fit[prM10$Type=="C"], pch=20, ylim=c(0, 1), xlab = "Vegetation density", ylab = "Probability of occurrence", type="l")
lines(prM10$Veg_dense[prM10$Type=="C"], prM10$lci[prM10$Type=="C"], lty=2)
lines(prM10$Veg_dense[prM10$Type=="C"], prM10$uci[prM10$Type=="C"], lty=2)
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$fit[prM10$Type=="R"], lty=1, col="red")
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$lci[prM10$Type=="R"], lty=2, col ="red")
lines(prM10$Veg_dense[prM10$Type=="R"], prM10$uci[prM10$Type=="R"], lty=2, col ="red")
title(main = "(a) All animals", outer = F, adj = 0, cex.main = 1, line = 0.3, font.main = 1)

par(xpd=NA)
legend(x=3.4, y=1, legend=c("Culvert", "Road"), col=c("black", "red"), lty=1, lwd=0.5, cex=1, bty="n", text.width=0.2, title = "Camera position")
par(xpd=F)

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



plot(prM16b$Veg_dense[prM16b$Type=="C"], prM16b$fit[prM16b$Type=="C"], pch = 20, ylim=c(0, 1), xlab = "Vegetation density (m)", ylab = "Probability of occurrence", type = "l", las = 1)
lines(prM16b$Veg_dense[prM16b$Type=="C"], prM16b$lci[prM16b$Type=="C"], lty = 2)
lines(prM16b$Veg_dense[prM16b$Type=="C"], prM16b$uci[prM16b$Type=="C"], lty = 2)
lines(prM16b$Veg_dense[prM16b$Type=="R"], prM16b$fit[prM16b$Type=="R"], lty = 1, col = "red")
lines(prM16b$Veg_dense[prM16b$Type=="R"], prM16b$lci[prM16b$Type=="R"], lty = 2, col = "red")
lines(prM16b$Veg_dense[prM16b$Type=="R"], prM16b$uci[prM16b$Type=="R"], lty = 2, col = "red")
title(main = "(d) Small species", outer = F, adj =0, cex.main = 1, line = 0.3, font.main = 1)

