#### Running the data ####
proficiency <- read.csv("~/QR/SDGdatasets/Indicator_4.a.1__Schools_with_access_to_basic_drinking_water_by_education_level_percent.csv", stringsAsFactors=FALSE)
skills <- read.csv("~/QR/SDGdatasets/Indicator_4.6.1__Proportion_of_population_achieving_at_least_a_fixed_level_of_proficiency_in_functional_skills_by_sex_age_and_type_of_skill_percent.csv", stringsAsFactors=FALSE)
childecono <- read.csv("~/QR/SDGdatasets/Indicator_8.7.1__Proportion_of_children_engaged_in_economic_activity_by_sex_and_age__percent.csv", stringsAsFactors=FALSE)
bank <- read.csv("~/QR/SDGdatasets/Indicator_8.10.2__Proportion_of_adults_15_years_and_older_with_an_account_at_a_financial_institution_or_mobilemoneyservice_provider_by_sex_percent_of_adults_aged_15_years_and_older.csv", stringsAsFactors=FALSE)

#### Cleaning the useless columns ####
proficiencycc <- proficiency[c(8,10,16,17, 29:48)]
skillscc <- skills[c(8,10,18:21,33:45)]
childeconocc <- childecono[c(8,10,16:19,31:39)]
bankcc <- bank[c(8,10,18,19,31:34)]

#### LATEST YEAR ONLY ####
# upper secondary level - latest year
proflatestup <- proficiencyupsec[c(1,24)]
proflatestup <- na.omit(proflatestup)

# lower secondary level - latest year 
proflatestlow <- proficiencylowsec[c(1,24)]
proflatestlow <- na.omit(proflatestlow)

# primary level - latest year
proflatestprim <- proficiencyprim[c(1,24)]
proflatestprim <- na.omit(proflatestprim)

# literature - last year
skillatestlit <- skillsccboth[skillsccboth$typeOfSkillCode == "LITE",]
skillatestlit <- skillatestlit[c(1,19)]        
skillatestlit <- na.omit(skillatestlit)     

# numerical - last year
skillatestnum <- skillsccboth[skillsccboth$typeOfSkillCode == "NUME",]
skillatestnum <- skillatestnum[c(1,19)]
skillatestnum <- na.omit(skillatestnum)

#### 2015 ####
# upper secondary level - latest year
profup5 <- proficiencyupsec[c(1,20)]
profup5 <- na.omit(profup5)

# lower secondary level - latest year 
proflow5 <- proficiencylowsec[c(1,20)]
proflow5 <- na.omit(proflow5)

# primary level - latest year
profprim5 <- proficiencyprim[c(1,20)]
profprim5 <- na.omit(profprim5)

# literature - last year
skillit5 <- skillsccboth[skillsccboth$typeOfSkillCode == "LITE",]
skillit5 <- skillit5[c(1,16)]        
skillit5 <- na.omit(skillit5)     

# numerical - last year
skillnum5 <- skillsccboth[skillsccboth$typeOfSkillCode == "NUME",]
skillnum5 <- skillnum5[c(1,16)]
skillnum5 <- na.omit(skillnum5)

#### Extreme cleaning - cleaning the rows that have ALL NAs ####
proficiencycc1 <- proficiencycc[complete.cases(proficiencycc[ , 24]),]
proficiencycc1 <- proficiencycc1[c(1,2,3,4,21,22,24)]

skillscc1 <- skillscc[complete.cases(skillscc[,19]),]
skillscc1 <- skillscc1[c(1,2,3,4,5,6,13,16,19)]

#### Eploring the Proficiency data ####
names(proficiencycc)
head(proficiencycc)
unique(proficiencycc$value_2013)
unique(proficiencycc$value_2014)
unique(proficiencycc$value_2015)
unique(proficiencycc$value_2016)
unique(proficiencycc$value_2017)
unique(proficiencycc$value_2018)
unique(proficiencycc$value_latest_year)

# Are their distributions close to normal? If not, how would you transform them to make them more normal?
# For ALL primary, upper secondary, and lower secondary - latest year
hist(proficiencycc$value_latest_year, main = "Distribution of Proficiency Level in Countries for All Levels of Education (Primary, Lower and Upper Secondary)",
     cex.main = 1, col = "rosybrown")

# For only upper secondary 
proficiencyupsec <- proficiencycc[proficiencycc$educationLevelCode == "UPPSEC",] 
# Numeric Proficiency upper secondary only latest year
proficiencylatest <- proficiencyupsec[c(1,24)]
# Omit NAs - latest year
proficiencylatest <- na.omit(proficiencylatest)
hist(proficiencylatest$value_latest_year)
hist(proficiencyupsec$value_latest_year, main = "Distribution of Proficiency Level in Countries for Upper Secondary Level",
     col = "rosybrown")
# Omitting NA and not omitting NA generates the same graph

# For only lower secondary
proficiencylowsec <- proficiencycc[proficiencycc$educationLevelCode == "LOWSEC",] 
hist(proficiencylowsec$value_latest_year, main = "Distribution of Proficiency Level in Countries for Lower Secondary Level",
     col = "rosybrown")

# For only primary
proficiencyprim <- proficiencycc[proficiencycc$educationLevelCode == "PRIMAR",] 
hist(proficiencyprim$value_latest_year, main = "Distribution of Proficiency Level in Countries for Primary Level",
     col = "rosybrown")

# Checking the data that has most values - most important
unique(proficiencyupsec$value_latest_year)
unique(proficiencylowsec$value_latest_year)
unique(proficiencyprim$value_latest_year)
# primary is the most important data

# Plotting histogram for all data
par(mfrow=c(2,2))
hist(proficiencycc$value_latest_year, main = "Distribution of Proficiency Level in Countries for All Levels of Education (Primary, Lower and Upper Secondary)",
     cex.main = 1, col = "rosybrown", xlab = "Percentage of Proficiency")
p1 <- hist(proficiencyupsec$value_latest_year, main = "Distribution of Proficiency Level in Countries for Upper Secondary Level",
     col = "rosybrown", xlab = "Percentage of Proficiency")
p2 <- hist(proficiencylowsec$value_latest_year, main = "Distribution of Proficiency Level in Countries for Lower Secondary Level",
     col = "rosybrown", xlab = "Percentage of Proficiency")
p3 <- hist(proficiencyprim$value_latest_year, main = "Distribution of Proficiency Level in Countries for Primary Level",
     col = "rosybrown", xlab = "Percentage of Proficiency", xlim = c(0,100))
par(mfrow=c(1,1))
plot(p1, col="khaki1", xlim=c(0,100), main = "Distribution of Proficiency Level in Countries for Different Levels of Education (Primary, Lower, and Upper Secondary Level)",
     xlab = "Percentage of proficiency")
plot(p2, col="khaki3", xlim=c(0,100), add=T,  main = "Distribution of Proficiency Level in Countries for Different Levels of Education (Primary, Lower, and Upper Secondary Level)",
     xlab = "Percentage of proficiency") 
plot(p3, add = T, col = "khaki4", xlim=c(0,100),
     main = "Distribution of Proficiency Level in Countries for Different Levels of Education (Primary, Lower, and Upper Secondary Level)",
     xlab = "Percentage of proficiency")
legend("topleft", title = "Education Level", c("Primary", "Lower Secondary", "Upper Secondary"), 
       fill = c("khaki4", "khaki3", "khaki1"), cex=0.8)
# DEFINITELY NOT NORMAL! And a lot of countries are actually 100!

#### Eploring the skills data ####
names(skillscc)
head(skillscc)
str(skillscc)
unique(skillscc$value_2006)
unique(skillscc$value_2007)
unique(skillscc$value_2008)
unique(skillscc$value_2009)
unique(skillscc$value_2010)
unique(skillscc$value_2011)
unique(skillscc$value_2012)
unique(skillscc$value_2013)
unique(skillscc$value_2014)
unique(skillscc$value_2015)
unique(skillscc$value_2016)
unique(skillscc$value_2017)
unique(skillscc$value_latest_year)

hist(skillscc$value_latest_year)
# Only both sexes
skillsccboth <-skillscc[skillscc$sexDesc == "Both sexes",]
# literacy only
skillscclit <- skillsccboth[skillsccboth$typeOfSkillCode == "LITE",]
hist(skillscclit$value_latest_year)
# nume only
skillsccnum <- skillsccboth[skillsccboth$typeOfSkillCode == "NUME",]
hist(skillsccnum$value_latest_year)

p4 <- hist(skillscclit$value_latest_year, 
           main = "Distribution of Different Skills (Literacy and Numerical) in Countries",
           col = "rosybrown1", xlim =c(0,100), xlab = "Percentage of Proficiency in Skills")
p2 <- hist(proficiencylowsec$value_latest_year, main = "Distribution of Different Skills (Literacy and Numerical) in Countries",
           col = "rosybrown3", xlab = "Percentage of Proficiency in Skills")
plot(p1, col="rosybrown1", main = "Distribution of Different Skills (Literacy and Numerical) in Countries",
     xlab = "Percentage of proficiency")
plot(p2, col="rosybrown3", add = T, main = "Distribution of Different Skills (Literacy and Numerical) in Countries",
     xlab = "Percentage of proficiency") 
legend("topleft", title = "Skills", c("Literacy", "Numerical"), 
       fill = c("rosybrown1", "rosybrown3"), cex=0.8)

#### GOAL 1: CORRELATION OF TWO INDICATORS - latest year ####
unique(proficiencycc$geoAreaName)
unique(skillscc$geoAreaName)

# LITERATURE AND UPPER SECONDARY!
goal1 <- merge(proflatestup,skillatestlit,by=c("geoAreaName"))
plot(goal1$value_latest_year.x, goal1$value_latest_year.y)
cor(goal1$value_latest_year.x, goal1$value_latest_year.y)
# better correlation: 0.74
# NUMERICAL AND UPPER SECONDARY!
goal11 <- merge(proflatestup, skillatestnum, by =c("geoAreaName"))
plot(goal11$value_latest_year.x, goal11$value_latest_year.y)
cor(goal11$value_latest_year.x, goal11$value_latest_year.y)
# small correlation lmao: 0.405

# LITERATURE AND LOWER SECONDARY!
goal2 <- merge(proflatestlow, skillatestlit, by =c("geoAreaName"))
plot(goal2$value_latest_year.x, goal2$value_latest_year.y)
cor(goal2$value_latest_year.x, goal2$value_latest_year.y)
# better correlation: 0.7590884
# NUMERICAL AND LOWER SECONDARY!
goal22 <- merge(proflatestlow, skillatestnum, by =c("geoAreaName"))
plot(goal22$value_latest_year.x, goal22$value_latest_year.y)
cor(goal22$value_latest_year.x, goal22$value_latest_year.y)
# low correlation: 0.4053688

# LITERATURE AND PRIMARY!
goal3 <- merge(proflatestprim, skillatestlit, by = c("geoAreaName"))
plot(goal3$value_latest_year.x, goal3$value_latest_year.y)
cor(goal3$value_latest_year.x, goal3$value_latest_year.y)
# low correlation: 0.4564844
# NUMERICAL AND PRIMARY
goal33 <- merge(proflatestprim, skillatestnum, by = c("geoAreaName"))
plot(goal33$value_latest_year.x, goal33$value_latest_year.y)
cor(goal33$value_latest_year.x, goal33$value_latest_year.y)
# low correlation: 0.4053688

# PLOTTING IT AND MAKING IT PRETTY
 
# literacy
par(mfrow=c(3,1))
plot(goal3$value_latest_year.x, goal3$value_latest_year.y,
     main = "Plot between the primary level students who got access to water and the literacy rate",
     xlab = "Percentage of primary level students with water access",
     ylab = "Percentage of adults who are literated",
     col = "red")
plot(goal2$value_latest_year.x, goal2$value_latest_year.y,
     main = "Plot between the lower secondary level students who got access to water and the literacy rate",
     xlab = "Percentage of lower secondary level students with water access",
     ylab = "Percentage of adults who are literated",
     col = "blue")
plot(goal1$value_latest_year.x, goal1$value_latest_year.y,
     main = "Plot between the upper secondary level students who got access to water and the literacy rate",
     xlab = "Percentage of upper secondary level students with water access",
     ylab = "Percentage of adults who are literated",
     col = "green")

# numerical


ggplot(ultimate1, aes(skillite)) +                    # basic graphical object
        geom_line(aes(y=waterprimary), colour="red") +  # first layer
        geom_line(aes(y=waterlowersec), colour="green")  # second layer

g <- ggplot(ultimate1, aes(skillite))
g <- g + geom_line(aes(y=waterprimary), colour="red")
g <- g + geom_line(aes(y=waterlowersec), colour="green")
g <- g + geom_line(aes(y=wateruppersec), colour = "blue")
g

g <- g + ylab("") + xlab("X")
g

# ultimate merging
ultimate <- merge(goal3, goal22, by = c("geoAreaName"))
ultimate1 <- merge(ultimate, proflatestup, by = c("geoAreaName"))
ultimate1$waterprimary <- ultimate1$value_latest_year.x.x
ultimate1$waterlowersec <- ultimate$value_latest_year.x.y
ultimate1$wateruppersec <- ultimate1$value_latest_year
ultimate1$skillite <- ultimate1$value_latest_year.y.x
ultimate1$skillnume <- ultimate1$value_latest_year.y.y
ultimate1 <- ultimate1[c(7,8,9,10,11)]

# correlation heatmap! 
cormat <- round(cor(ultimate1), 2)
install.packages("corrplot")
library(corrplot)
corrplot(cormat)
heatmap(x=cormat, sym = TRUE)

library(reshape2)
meltedcormat <- melt(cormat)
head(meltedcormat)
library(ggplot2)
ggplot(data=meltedcormat, aes(x=Var1, y=Var2, fill=value))
plot <- ggplot(data = meltedcormat, aes(x=Var1, y=Var2, fill=value, 
                                        label= value))
plottitle <- plot + geom_tile()
plottitle
plotcolor <- plottitle + scale_fill_gradient2(low = "blue",high ="red"
                                              ,mid = "lightblue1")
plotlabel <- plotcolor + geom_text()
plotlabel
plotlabel2 <- plottitle + geom_text()
plotlabel2

#### GOAL 1: CORRELATION  OF TWO INDICATORS - year 2015 ####

# LITERATURE AND UPPER SECONDARY!
goal4 <- merge(profup5,skillit5,by=c("geoAreaName"))
plot(goal4$value_2015.x, goal4$value_2015.y)
cor(goal4$value_2015.x, goal4$value_2015.y)

# correlation: NA - non existent
# NUMERICAL AND UPPER SECONDARY!
goal44 <- merge(profup5, skillnum5, by =c("geoAreaName"))
plot(goal44$value_2015.x, goal44$value_2015.y)
cor(goal44$value_2015.x, goal44$value_2015.y)
# correlation: NA

# LITERATURE AND LOWER SECONDARY!
goal5 <- merge(proflow5, skillit5, by =c("geoAreaName"))
plot(goal5$value_2015.x, goal5$value_2015.y)
cor(goal5$value_2015.x, goal5$value_2015.y)
# corr NA
# NUMERICAL AND LOWER SECONDARY!
goal55 <- merge(proflow5, skillnum5, by =c("geoAreaName"))
plot(goal55$value_2015.x, goal55$value_2015.y)
cor(goal55$value_2015.x, goal55$value_2015.y)
# corr: NA

# LITERATURE AND PRIMARY!
goal6 <- merge(profprim5, skillit5, by = c("geoAreaName"))
plot(goal6$value_2015.x, goal6$value_2015.y)
cor(goal6$value_2015.x, goal6$value_2015.y)
# corr: NA
# NUMERICAL AND PRIMARY
goal66 <- merge(profprim5, skillnum5, by = c("geoAreaName"))
plot(goal66$value_2015.x, goal66$value_2015.y)
cor(goal66$value_2015.x, goal66$value_2015.y)
# corr: NA

# ultimate merging 
ultimate2 <- merge(goal6, goal55, by = c("geoAreaName"))
ultimate22 <- merge(ultimate2, profup5, by = c("geoAreaName"))
ultimate22$waterprimary <- ultimate22$value_2015.x.x
ultimate22$waterlowersec <- ultimate22$value_2015.x.y
ultimate22$wateruppersec <- ultimate22$value_2015
ultimate22$skillite <- ultimate22$value_2015.y.x
ultimate22$skillnume <- ultimate22$value_2015.y.y
ultimate22 <- ultimate22[c(7,8,9,10,11)]

# correlation heatmap! 
cormat1 <- round(cor(ultimate22), 2)
install.packages("corrplot")
library(corrplot)
corrplot(cormat1)
heatmap(x=cormat1, sym = TRUE)

library(reshape2)
meltedcormat1 <- melt(cormat1)
head(meltedcormat1)
library(ggplot2)
ggplot(data=meltedcormat1, aes(x=Var1, y=Var2, fill=value))
plot2 <- ggplot(data = meltedcormat1, aes(x=Var1, y=Var2, fill=value, 
                                        label= value))
plottitle1 <- plot2 + geom_tile()
plottitle1
plotcolor1 <- plottitle1 + scale_fill_gradient2(low = "blue",high ="red"
                                              ,mid = "lightblue1")
plotlabel1 <- plotcolor1 + geom_text()
plotlabel1

#### Eploring the bank data ####

str(bankcc)
unique(bankcc$value_latest_year)

# doing both sexes
bankccs <- bankcc[bankcc$sexDesc == "Both sexes",]
bankccs <- bankccs[c(1,8)]
bankccs <- na.omit(bankccs)

# The histogram - only 15-17 years old
hist(bankccs$value_latest_year)

#### Exploring the childecono data ####
hist(childeconocc$value_latest_year)
unique(childeconocc$value_latest_year)
str(childeconocc)
unique(childeconocc$ageDesc)

# doing both sexes
childeconoccs <- childeconocc[childeconocc$sexDesc == "Both sexes",]

# checking the data for different age range
unique(childeconoccs[childeconoccs$ageDesc == "10 to 17 years old",]$value_latest_year)
unique(childeconoccs[childeconoccs$ageDesc == "6 to 17 years old",]$value_latest_year)
unique(childeconoccs[childeconoccs$ageDesc == "5 to 17 years old",]$value_latest_year)
unique(childeconoccs[childeconoccs$ageDesc == "5 to 14 years old",]$value_latest_year)
unique(childeconoccs[childeconoccs$ageDesc == "7 to 17 years old",]$value_latest_year)
# 5 to 17 has more data! 5 to 14 as well!

# checking the data for years
unique(childeconoccs$value_2010)
unique(childeconoccs$value_2011)
unique(childeconoccs$value_2012)
unique(childeconoccs$value_2013)
unique(childeconoccs$value_2014)
unique(childeconoccs$value_2015)
unique(childeconoccs$value_2016)
unique(childeconoccs$value_2017)
# 2011 to 2016 data is usable

# using the 5 to 17 years old age range
childeconoccs <- childeconocc[childeconocc$sexDesc == "Both sexes",]
childeconoccsa <- childeconoccs[childeconoccs$ageDesc == "5 to 17 years old",]
childeconoccsa <- childeconoccsa[c(1,15)]
childeconoccsa <- na.omit(childeconoccsa)

# the histogram
hist(childeconocc$value_latest_year)
hist(childeconoccsa$value_latest_year)

#### GOAL 2 - CORRELATION OF TWO INDICATORS - 5-17 - BOTH SEXES - LATEST YEAR####
goal7 <- merge(bankccs, childeconoccsa, by = c("geoAreaName"))
plot(goal7$value_latest_year.x, goal7$value_latest_year.y,
     main = "Scatter Plot Between Adults with Bank Account and Children (5-17 years old) Engaged in Economic Activity in Different Countries",
     ylab = "Number of Children Engaged in Economic Activity",
     xlab = "Number of Adults with Bank Account",
     col = "blue")
cor(goal7$value_latest_year.x, goal7$value_latest_year.y)

is.num <- sapply(goal7, is.numeric)
goal7[is.num] <- lapply(goal7[is.num], round, 2)
print(goal7, digits = 2)
cor(goal7$value_latest_year.x, goal7$value_latest_year.y)
# correlation: NA

# correlation heatmap! 
goal7s <- goal7[c(2,3)]
cormat2 <- round(cor(goal7s), 2)
install.packages("corrplot")
library(corrplot)
corrplot(cormat2)

library(reshape2)
meltedcormat2 <- melt(cormat2)
head(meltedcormat2)
library(ggplot2)
ggplot(data=meltedcormat2, aes(x=Var1, y=Var2, fill=value))
plot3 <- ggplot(data = meltedcormat2, aes(x=Var1, y=Var2, fill=value, 
                                          label= value))
plottitle2 <- plot3 + geom_tile()
plottitle2
plotcolor2 <- plottitle2 + scale_fill_gradient2(low = "blue",high ="red"
                                                ,mid = "lightblue1")
plotlabel2 <- plotcolor2 + geom_text()
plotlabel2

#### CORRELATION ACROSS THE TWO GOALS ####
goal8 <- merge(bankccs, skillatestlit, by = c("geoAreaName"))
par(mfrow=c(1,1))
plot(goal8$value_latest_year.x, goal8$value_latest_year.y)
cor(goal8$value_latest_year.x, goal8$value_latest_year.y)
# 0.09928971

goal9 <- merge(bankccs, proflatestprim, by = c("geoAreaName"))
plot(goal9$value_latest_year.x, goal9$value_latest_year.y)
cor(goal9$value_latest_year.x, goal9$value_latest_year.y)
# 0.6615267

goal10 <- merge(childeconoccsa, skillatestlit, by = c("geoAreaName"))
plot(goal10$value_latest_year.x, goal10$value_latest_year.y)
cor(goal10$value_latest_year.x, goal10$value_latest_year.y)
# -0.2005311

goal11 <- merge(childeconoccsa, proflatestprim, by = c("geoAreaName"))
plot(goal11$value_latest_year.x, goal11$value_latest_year.y)
cor(goal11$value_latest_year.x, goal11$value_latest_year.y)
# -0.5319971

#### ULTIMATE ULTIMATE MERGING ####
ugoal <- merge(ultimate1, goal7, by = c("geoAreaName"))




