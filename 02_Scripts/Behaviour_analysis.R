# Written by Felicity E. Charles and Annabel L. Smith 5/2/2021
# Updated post peer review 16/05/2023 
# Caveat emptor

# This script deals with the analysis of the behaviour data from the study of the use of road infrastructure by terrestrial vertebrates. 

# 1. Read in the data
beh <- read.table("00_Data//Behaviour.txt", header = T, stringsAsFactors = T) # Read the behaviour data into R, ensuring that factor variables are assigned
str(beh) # Check the structure of beh
head(beh); dim(beh) # Check the first few rows of beh

rc <- read.table("./00_Data/CharacteristicsR.txt", header = T, stringsAsFactors = T) # Read the site characteristics data into R, ensuring that factor variables are assigned  


# 2.1 Check the levels of each column of the behaviour data
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
library(lubridate)
library(tidyverse)


# 4.Explore behavioural data ----

head(beh); dim(beh) # Look at the first few rows of the data, there are 4714 individual triggers
sum(table(beh$Behaviour)) # There are 4713 rows of data
range(beh$Anim_num) # There are 11645 individual animals
table(beh$Behaviour) # There are 73 culvert crossing occurrences, 2864 foraging occurrences, 506 road crossing occurrences, and 1270 sitting/standing still occurrences in the behaviour data.
sum(2864+1270) # What is the sum of the foraging and sitting still behaviours, 4134 occurrences of 4713 individual triggers. 



# 4.1 HOW MANY INDEPENDENT OBSERVATIONS ARE THERE FOR BEHAVIOUR?
# 4.1.1 Firstly let's look at for all behaviours

hist(table(beh$Anim_num)) # How often was the same individual observed multiple times? Mostly animals were only observed once but there were occasions when animals were observed multiple times. 
beh[which(unique(beh$Anim_num) >1), ] # We can see that some of the repeated observations are occurring on different days/hours/display different behaviours

table(beh$Anim_num) # Are there repeated observations? For animals that were observed either crossing the road or using the culvert which are contained within beh, how many have repeat observations?

str(beh) # Check the structure of this dataframe to refresh the memory
manim <- as.numeric(names(table(beh$Anim_num)[which(table(beh$Anim_num)>1)])) # Create a value containing the data with multiple animal occurrences
mdat <- beh[which(beh$Anim_num %in% manim),] # Create a data frame containing the data with multiple animal occurrences 
mdat[which(mdat$Anim_num == manim[1]),] # Look at the first individual of the multiple data occurrences data frame to see what is occurring, individual observed over multiple days and hours or with different behaviours. Data needs to be cleaned up further. 


length(manim) # There are 696 repeated observations. 
length(unique(beh$Anim_num)) # Although there are 4714 observations there are only 1164 unique animals. There are more observations than we want/need. 

# How do we deal with the repeated observations? Do we take the first observation for each day or hour? No, we would loose data by filtering the data by the hour or day that it was observed in, as sometimes individuals displayed different behaviours depending on the hour of the day. We run the risk of loosing data for culvert crossings which are already quite low if we did this.

# Therefore, if there is a repeated observation within same hour we remove it, but we will keep in mind the behaviour. So we want 1 observation per hour per behaviour per individual animal. 


# 4.1.2 Data filtering -----
# Create a value within the data that contains all the information needed to simply distinguish between the day, hour and behaviour of each animal
beh$anim_hr<-paste(beh$Anim_num,beh$Behaviour, beh$Day, beh$Month, beh$Hour, sep="_") # Create a value within the data that contains all the information needed to simply distinguish between the day, hour and behaviour of each animal
head(beh); dim(beh) # Check how this looks
unique(beh$anim_hr) # Check how this looks, not in order


# We want to order the data so that when they are subset, we can take the first observation per hour
beh2 <- beh[order(beh$Anim_num,beh$Behaviour, beh$Day, beh$Month, beh$Hour, beh$Min, beh$Sec),]
rownames(beh2)<-1:nrow(beh2)
beh2[beh2$Anim_num==42,] # Lets have a look at one of the individual animals and ensure that it is ordered correctly. 

# 4.1.3 Remove multiple observations of same animal -----
# Create a new data frame with only the first observation for each animal per hour per behaviour type:
beh3 <- beh2[-which(duplicated(beh2$anim_hr)),]
beh3 <- droplevels(beh3)
rownames(beh3) <- 1:nrow(beh3)
head(beh3); dim(beh3)
table(beh3$Behaviour) # We now only have 33 culvert crossing observations, and 370 road crossing observations. We have successfully removed duplicates of the same behaviour of animals within the same hour. 

# We do also need to consider whether an individual animal was observed at 10:52 and then again at 11:02, both of these observations would be kept as they are in separate hours but this data cannot be considered independent. For instance, a cow is seen at 11:21 and then again at 12:07 on the same day, we want there to be an interval of an hour between occurrences so we need to fix this.



# 4.2.5 Remove duplicate records of the same animal performing the same behaviour within a 60 minute period ----
beh3$hr <- paste(beh3$Hour, beh3$Min, beh3$Sec, sep=":") # Add a column with the time variables concatenated. 

# Find duplicates
# Filter the data to find duplicates of same animal, same behaviour and same day. Determine the difference of time between the duplicates on the same day.
beh3.1 <- beh3 %>% 
  group_by(Anim_num, Behaviour, Day) %>% 
  mutate(dupe = n()>1) %>% 
  mutate(diff = strptime(hr, "%H:%M:%S") - lag(strptime(hr, "%H:%M:%S")), diff_mins = as.numeric(diff, units = "mins")) # Now we can see that there are records where less than 60 minutes have elapsed on the same day for the same behaviour.
View(beh3.1) # Double check what is flagged as a duplicate and whether there are more than 1 duplicate for 1 animal.


# If we look at the data, we know all but animal number 2, the cow, were only observed twice on the same day performing the same behaviour. To make sure we don't loose the 3rd observation that is more than 60 minutes from the first observation of animal number 2 performing a culvert crossing, let us first manually remove this from the data. 

beh3.2 <- beh3.1[-4,]
beh3.2 # Make sure this has worked. 

# Now we need to rerun the above filtering to ensure that there is no longer a duplicate record within 60 minutes for animal number 2.
beh3.3 <- beh3.2 %>% 
  group_by(Anim_num, Behaviour, Day) %>% 
  mutate(dupe = n()>1) %>% 
  mutate(diff = strptime(hr, "%H:%M:%S") - lag(strptime(hr, "%H:%M:%S")), diff_mins = as.numeric(diff, units = "mins"))
# If we look at animal number 2, we can see that 92 minutes has elapsed between records. 

# Remove duplicates from dataset
beh4 <- beh3.3[-which(beh3.3$dupe == T & beh3.3$diff_mins <= 60),]
head(beh4, 10); dim(beh4) # Check this has worked, there are now 1676 individual independent triggers in the data
str(beh4) # Double check everything has been coded and imported correctly. 
table(beh4$Behaviour) # There are 32 culvert crossings and 366 road crossings, 948 foraging, and 330 sitting still
range(beh4$Site) # There are still 30 sites, no data lost.




# 4.2 HOW MANY INDEPENDENT OBSERVATIONS ARE THERE FOR ROAD CROSSING BEHAVIOURS? ----
# 4.2.1 Create a dataframe with only culvert crossing and road crossing behaviours ----
beh2 <- beh[c(which(beh$Behaviour == "CC"), which(beh$Behaviour == "RX")), ] 

str(beh) # Look at the structure of the original behavioural data
str(beh2) # Look at the structure of the new data just for CC and RX, the Behaviour variable still has 4 levels, check what is going on
unique(beh2$Behaviour) # Although the data only contains the data for culvert crossings or road crossings, all the levels from the original data are still present
beh2 <- droplevels(beh2) # Drop the levels for the behaviour variable that are not present within the dataframe
head(beh2) # Check how the data looks, the row numbers are not consistent 1-10 etc. due to the removal of data that was not categorised as CC or RX

rownames(beh2) <- 1: nrow(beh2) # Change the row names so that they are consistently numbered
table(beh2$Behaviour) # How many animals are observed for each behaviour in this dataframe?

# There are more animals crossing over the surface of the road. Although, we must consider that some animals captured on the culvert cameras were observed crossing the road and that there was double the trapping effort on culverts versus roads, as culverts had 4 cameras, and roads only 2 cameras. 


hist(table(beh2$Anim_num)) # How often was the same individual observed multiple times? Mostly animals were only observed once but there were occasions when animals were observed multiple times. 
beh2[which(unique(beh2$Anim_num) >1), ] # We can see that some of the repeated observations are occurring on different days/hours/display different behaviours

table(beh2$Anim_num) # Are there repeated observations? For animals that were observed either crossing the road or using the culvert which are contained within beh2, and how many have repeat observations? 

manim <- as.numeric(names(table(beh2$Anim_num)[which(table(beh2$Anim_num)>1)])) # Create a value containing the data with multiple animal occurrences
mdat <- beh2[which(beh2$Anim_num %in% manim),] # Create a data frame containing the data with multiple animal occurrences 
mdat[which(mdat$Anim_num == manim[1]),] # Look at the first individual of the multiple data occurrences data frame to see what is occurring, individual observed over multiple days and hours or with different behaviours. Data needs to be cleaned up further. 


## Explore the multiple animal occurrences data
length(manim) # There are 110 repeated observations. 
dim(beh2) # There are 579 observations of road crossing and culvert crossing behaviours, with 110 repeated observations. 
length(unique(beh2$Anim_num)) # Although there are 579 observations there are only 384 unique animals. There are more observations than we want/need. 



# 4.2.2 Data filtering -----
# Create a value within the data that contains all the information needed to simply distinguish between the day, hour and behaviour of each animal
beh2$anim_hr<-paste(beh2$Anim_num,beh2$Behaviour, beh2$Day, beh2$Month, beh2$Hour, sep="_")
head(beh2); dim(beh2) # Check how this looks
unique(beh2$anim_hr) # Check how this looks, not in order

# We want to order the data so that when they are subset, we can take the first observation per hour
beh2 <- beh2[order(beh2$Anim_num,beh2$Behaviour, beh2$Day, beh2$Month, beh2$Hour, beh2$Min, beh2$Sec),]
rownames(beh2)<-1:nrow(beh2)
beh2[beh2$Anim_num==25,] # Lets have a look at one of the individual animals and ensure that it is ordered correctly. 

# 4.2.3 Standardise trapping effort ----
# In the analysis steps we can take into account the difference in trapping effort on culvert cameras vs road cameras but to begin with, we are going to remove RX from culvert cameras and CC from road cameras before we remove duplicated observations of animals. 
beh2.1 <- beh2[-which(beh2$Type == "C" & beh2$Behaviour == "RX" | beh2$Type == "R" & beh2$Behaviour == "CC"),]
source("./01_Functions/Felicity_Functions.R") # Load the functions to this script 
tidy.df # Check it is working
beh2.1 <- tidy.df(beh2.1)
row.names(beh2.1) # Redo row names
str(beh2.1)


# 4.2.4 Remove multiple observations of same animal -----
# Create a new data frame with only the first observation for each animal per hour per behaviour type:
beh3 <- beh2.1[-which(duplicated(beh2.1$anim_hr)),]
beh3 <- droplevels(beh3)
rownames(beh3) <- 1:nrow(beh3)
head(beh3); dim(beh3)
table(beh3$Behaviour) # We now only have 33 culvert crossing observations, and 248 road crossing observations. We have successfully removed duplicates of the same behaviour of animals within the same hour. 

# We do also need to consider whether an individual animal was observed at 10:52 and then again at 11:02, both of these observations would be kept as they are in separate hours but this data cannot be considered independent. For instance, a cow is seen at 11:21 and then again at 12:07 on the same day, we want there to be an interval of an hour between occurrences so we need to fix this.

# 4.2.5 Remove duplicate records of the same animal performing the same behaviour within a 60 minute period ----
beh3$behav<-ifelse(beh3$Behaviour=="RX", 0, 1) # We want to assign numeric values to the behaviour for the means of further analysis, culvert crossing = 0, road crossing = 1.
beh3$hr <- paste(beh3$Hour, beh3$Min, beh3$Sec, sep=":") # Add a column with the time variables concatenated. 


# Find duplicates 
# Filter the data to find duplicates of same animal, same behaviour and same day. Determine the difference of time between the duplicates on the same day.
beh3.1 <- beh3 %>% 
  group_by(Anim_num, behav, Day) %>% 
  mutate(dupe = n()>1) %>% 
  mutate(diff = strptime(hr, "%H:%M:%S") - lag(strptime(hr, "%H:%M:%S")), diff_mins = as.numeric(diff, units = "mins")) # Now we can see that there are records where less than 60 minutes have elapsed on the same day for the same behaviour.
View(beh3.1) # Double check what is flagged as a duplicate and whether there are more than 1 duplicate for 1 animal.

# As before, remove the first repeated observation of cow 2 manually.
beh3.2 <- beh3.1[-4,]
beh3.2 # Make sure this has worked. 

# Rerun filtering
beh3.3 <- beh3.2 %>% 
  group_by(Anim_num, behav, Day) %>% 
  mutate(dupe = n()>1) %>% 
  mutate(diff = strptime(hr, "%H:%M:%S") - lag(strptime(hr, "%H:%M:%S")), diff_mins = as.numeric(diff, units = "mins"))
# If we look at animal number 2, we can see that 92 minutes has elapsed between records. 

# Remove duplicates from dataset
beh4 <- beh3.3[-which(beh3.3$dupe == T & beh3.3$diff_mins <= 60),]
head(beh4, 10); dim(beh4) # Check this has worked, there are now 278 individual independent triggers in the data
str(beh4) # Double check everything has been coded and imported correctly. 
table(beh4$Behaviour) # There are 32 culvert crossings and 246 road crossings
range(beh4$Site) # There are now only 26 sites



# 5. Environmental variable wrangling ---- 
# To work with site characteristics data better, need to create summaries of the vegetation data (average the vegetation height for each camera point). Want to determine if the probability of spotting individuals was influenced by vegetation height or density. Want to add extra columns to beh3 for road type (character), veg height (numeric), veg dense (character). Also want to determine the influence of road type on crossing behaviour. 

# 5.1 Let's get the characteristics data into a workable format ----
unique(rc$Site) # What are the variables of the column Site in the site characteristics dataset?
unique(beh4$Site) # What are the variables of the column Site in the beh4 dataset? Some of the sites are not included in this data as there were no occurrences of road crossings at these sites. 

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


# 5.2 Calculate culvert openness ratio ----
Open <- data.frame(Openness = rc$Clvt_wdt*rc$Clvt_ht/rc$Length) # Create the openness ratio for each site
Open$Site <- 1:30 # Assign the sites to the dataframe


# 5.3 Extract the vegetation density information ----
rc2 <- rc[,c("Site", "C1", "C2", "C3", "C4", "R1", "R2")]
rc3 <- data.frame(Site = rc2$Site, VegC = apply(rc2[,2:5], 1, mean), VegR = apply(rc2[,6:7], 1, mean)) # Produce the average vegetation density for culvert cameras and road cameras
rc3$Vegdif <- rc3$VegC - rc3$VegR # Calculate the difference of vegetation density between the culvert and road cameras
rc3$VegC <- sort(rc3$VegC) # Reorder
rc3$VegR <- sort(rc3$VegR) # Reorder
rc3Vegdif <- sort(rc3$Vegdif) # Reorder
# If vegdif is positive then there is more vegetation at the culvert, if it is negative then there is more vegetation at the road. 



# 6. Crossing behaviour at the site level ----
# As we only have a small sample size, especially for culvert crossings, we need to take this data down to the site level for analysis. 

# 6.1 Calculate average vegetation density at the site level ----
rc3$Vegmean <- (rc3$VegC + rc3$VegR)/2 # Produce the average vegetation density at the site level

# 6.2 Get information that can be extracted from the original road characteristics dataset and add it to beh.site -----
beh.site <- rc[,c("Site", "Rd_typ", "Cam_typ", "Clvt_sz", "Clvt_wdt", "Clvt_ht", "Length", "Construction")] # Create a new dataset containing these variables

# 6.2.1 Add the openness data to beh.site
beh.site <- merge(beh.site, Open, by = "Site", all.x = T, all.y = F) # Merge this new column with the beh.site data set
head(beh.site); dim(beh.site) # Check how this looks
str(beh.site)

# 6.2.2 Add the vegetation density data to beh.site ----
beh.site <- merge(beh.site, rc3, by = c("Site"), all.x = T, all.y = F)
head(beh.site) # Check that this has worked 

# We now have all the road and environmental characteristics data we wanted added to the behavioural data for site level analysis.


# 7. Road crossing information for beh.site ----
beh.site$RC <- 0 # Create a new column for road crossings, and assign 0 to all
beh.site$RC[which(beh.site$Site %in% unique(beh4$Site))] <- 1 # If there was a road crossing either a culvert or road crossing at a site assign a 1, all other sites that had no crossings will remain 0
head(beh.site); dim(beh.site) # Check how this looks


# 7.1 Extract road crossing information from beh4 ----
table(beh4$Site, beh4$Behaviour) # How many animals were observed performing each road crossing behaviour?
RC <- data.frame(Site = as.numeric(dimnames(table(beh4$Site, beh4$Behaviour))[[1]]), CC = table(beh4$Site, beh4$Behaviour)[,1], RX = table(beh4$Site, beh4$Behaviour)[,2]) # Create a new data frame containing the information for the number of each road crossing behaviour per site.
head(RC) # What does the road crossing data look like
# We want to rename these columns to say All.CC or All.RX, so that we know this information is for all animals
names(RC)[names(RC) == "CC"] <- "All.CC"
names(RC)[names(RC) == "RX"] <- "All.RX"

# 7.1.2 Standardise for trapping effort ----
# Double the number of All.RX to standardise for trapping effort
RC$All.RX <- round(RC$All.RX*2,)
RC$All.RX # Has this worked, yes.
beh.site <- merge(beh.site, RC, by = "Site", all.x = T, all.y = F) # Merge this with beh.site
beh.site[is.na(beh.site)] <- 0 # Replace NA with 0
head(beh.site); dim(beh.site) # Check how this looks
beh.site$Prop_RC_clvt <- beh.site[,"All.CC"]/rowSums(beh.site[,c("All.CC","All.RX")]) # Calculate the proportion of road crossings that were under the road.
head(beh.site,3); dim(beh.site) # Check that this has worked.
beh.site$Prop_RC_clvt[beh.site$RC == 0] <- NA # For all animals when site had no RC at all it is now NA, it is 0 if it was a RX over road surface, and 1 if through a culvert. 
head(beh.site); dim(beh.site) # Check that this has worked



# 8. Extract functional group categories crossing behaviour data ----
# 8.1 Crossing behaviour of Kangaroos ----
behKang <-beh4[which(beh4$Species %in% "Kangaroo"),] # Create the road crossings data for Kangaroos, first finding where they were observed
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



# 8.2 Crossing behaviour of Exotic species ----
behExotic <- beh4[which(beh4$Species %in% c("Cat", "Cow", "Dog", "Fox", "Hare")),] # Create the road crossings data for exotics, first finding where they were observed
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



# 8.3 Crossing behaviour of Native species ----

behNative <- beh4[which(beh4$Species %in% c("Kangaroo", "Macropod", "Monitor", "Possum", "Whiptail")),] # Create the road crossings data for natives, first finding where they were observed
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



# 8.4 Crossing behaviour of Small species ----
behSmall <- beh4[which(beh4$Species %in% c("Monitor", "Possum", "Hare", "Rabbit", "Amphibian", "Snake", "Rodent", "Quail", "Dragon", "Cat")),] # Create the road crossings data for small species, first finding where they were observed
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



# 8.5 Crossing behaviour of Large species ----
behLarge <- beh4[which(beh4$Species %in% c("Kangaroo", "Macropod", "Whiptail", "Cow","Fox", "Dog","Deer")),] # Create the road crossings data for large species, first finding where they were observed.
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




# 9. Test for correlated variables ----
# We are interested in the influence of vegetation and culvert size on crossing behaviour, to simplify the model set, determine whether any other variables are correlated with these factors. 

# 9.1 Firstly focus on culvert size, are there any correlated variables with culvert size 
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




# 9.2 Plots for appendix ----
dev.new(width=20, height=20, dpi=80, pointsize=30, noRStudioGD = T) 
par(mfrow=c(3, 3), mgp=c(2.5,1,0), mar=c(4,4,3,3), family = 'helvetica') # Fig A3 850x800


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



# 10. Analyse crossing behaviour in relation to culvert size ----
# How does culvert size influence the proportion of crossings under the culvert?
# To analyse this, as these are proportions we will be undertaking beta regressions instead of the generalized linear models used above. 

?mgcv
gam(Prop_RC_clvt ~ 1 , family=betar(link="logit"), data = beh.site) 

gam(Prop_RC_clvt ~ Clvt_sz , family=betar(link="logit"), data = beh.site) 

head(beh.site)


# 10.1 All animals ----
m_null.2 <- gam(Prop_RC_clvt ~ 1, family=betar(link="logit"), data = beh.site) 
summary(m_null.2) # Null model


m5 <- gam(Prop_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m5) # Not significant
AICc(m_null.2); AICc(m5)


AICc(m_null.2); AICc(m5) # Null model is better, the lower the value the better

# 10.2 Kangaroos ----
m_null.3 <- gam(Prop_Kang_RC_clvt ~ 1, family = betar(link="logit"), data = beh.site)
summary(m_null.3) # Null model

m6 <- gam(Prop_Kang_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m6) # Not sig.
AICc(m_null.3); AICc(m6) #Null model is better


# 10.3 Exotics ----
m_null.4 <- gam(Prop_Exotic_RC_clvt ~ 1, family = betar(link="logit"), data = beh.site)
summary(m_null.4) # Null model


m7 <- gam(Prop_Exotic_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m7)# Not sig.
AICc(m_null.4); AICc(m7) # null model is better


# 10.4 Natives ----
m_null.5 <- gam(Prop_Native_RC_clvt ~ 1, family = betar(link="logit"), data = beh.site)
summary(m_null.5) # Null model

m8 <- gam(Prop_Native_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m8)# Not sig.
AICc(m_null.5); AICc(m8) # Null model is better


# 10.5 Small species ----

m_null.6 <- gam(Prop_Small_RC_clvt ~ 1, family = betar(link="logit"), data = beh.site)
summary(m_null.6) # Null model

m9 <- gam(Prop_Small_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m9)# Not sig.
AICc(m_null.6); AICc(m9) # Null model is better


# 10.6 Large species ----

m_null.7 <- gam(Prop_Large_RC_clvt ~ 1, family = betar(link="logit"), data = beh.site)
summary(m_null.7) # Null model

m10 <- gam(Prop_Large_RC_clvt ~ Clvt_sz, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m10)# Not significant
AICc(m_null.7); AICc(m10) # Null model is better



# 11. Now to investigate vegetation density's influence on crossing behaviour ----
# 11.1 Test for correlated variables ----

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


# 11.2 Plots for appendix ----
dev.new(width=20, height=20, dpi=80, pointsize=30, noRStudioGD = T) # Figure A4 850 x 800
par(mfrow=c(3, 3), mgp=c(2.5,1,0), mar=c(4,4,3,3), family = 'helvetica')


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


# 12. Analyse crossing behaviour in relation to the average vegetation density ----
# How does the average vegetation density influence the proportion of crossings under the culvert?

# 12.1 All animals ----
m11 <- gam(Prop_RC_clvt ~ Vegmean, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m11) # Not sig.
AICc(m_null.2); AICc(m11) # null model is better


# 12.2 Kangaroos ----
m12 <- gam(Prop_Kang_RC_clvt ~ Vegmean, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m12) # Not sig.
AICc(m_null.3); AICc(m12) # Null model is better


# 12.3 Exotics ----
m13 <- gam(Prop_Exotic_RC_clvt ~ Vegmean, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m13)# Not sig.
AICc(m_null.4); AICc(m13) # Null model is better


# 12.4 Natives ----
m14 <- gam(Prop_Native_RC_clvt ~ Vegmean, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m14)# Not sig.
AICc(m_null.5); AICc(m14) # null model is better


# 12.5 Small species ----
m15 <- gam(Prop_Small_RC_clvt ~ Vegmean, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m15)# Significant influence of higher average vegetation density
AICc(m_null.6); AICc(m15) # Null model is still better


# 12.6 Large species ----
m16 <- gam(Prop_Large_RC_clvt ~ Vegmean, data = beh.site[which(beh.site$RC == 1),], family = betar(link="logit"))
summary(m16)# Not sig.
AICc(m_null.7); AICc(m16) # Null model is better



# 13. Compare culvert size and vegetation density models ----
# Model selection framework information - smaller AICc the better it is (more negative).
# 13.1 All animals ----
AICc(m_null.2); AICc(m5); AICc(m11) # Null model is best
logLik.gam(m_null.2)
logLik.gam(m11)
logLik.gam(m5)

# 13.2 Exotic species ----
AICc(m_null.4); AICc(m7); AICc(m13) #Null model is best
logLik.gam(m_null.4)
logLik.gam(m13)
logLik.gam(m7)


# 13.3 Native species ----
AICc(m_null.5); AICc(m8); AICc(m14) # Null model is best
logLik.gam(m_null.5)
logLik.gam(m14)
logLik.gam(m8)



# 13.4 Kangaroos ----
AICc(m_null.3); AICc(m6); AICc(m12) #Null model is best
logLik.gam(m_null.3)
logLik.gam(m6)
logLik.gam(m12)


# 13.5 Small species ----
AICc(m_null.6); AICc(m9); AICc(m15) #Null model is best
logLik.gam(m_null.6)
logLik.gam(m15)
logLik.gam(m9)


# 13.6 Large species ----
AICc(m_null.7); AICc(m10); AICc(m16) # Null model is best
logLik.gam(m_null.7)
logLik.gam(m10)
logLik.gam(m16)

