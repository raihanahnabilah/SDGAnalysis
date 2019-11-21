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

#### Eploring the water accessibility data ####
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
hist(proficiencycc$value_latest_year, main = "Distribution of Water Accessibility in Countries for All Levels of Education (Primary, Lower and Upper Secondary)",
     cex.main = 1, col = "rosybrown", xlab = "Percentage of Proficiency")
p1 <- hist(proficiencyupsec$value_latest_year, main = "Distribution of Water Accessibility in Countries for Upper Secondary Level",
           col = "rosybrown", xlab = "Percentage of Proficiency")
p2 <- hist(proficiencylowsec$value_latest_year, main = "Distribution of Water Accessibility in Countries for Lower Secondary Level",
           col = "rosybrown", xlab = "Percentage of Proficiency")
p3 <- hist(proficiencyprim$value_latest_year, main = "Distribution of Water Accessibilityin Countries for Primary Level",
           col = "rosybrown", xlab = "Percentage of Proficiency", xlim = c(0,100))
par(mfrow=c(1,1))
plot(p1, col="khaki1", xlim=c(0,100), main = "Distribution of Water Accessibility in Countries for Different Levels of Education (Primary, Lower, and Upper Secondary Level)",
     xlab = "Percentage of Water Accessibility")
plot(p2, col="khaki3", xlim=c(0,100), add=T,  main = "Distribution of Water Accessibility in Countries for Different Levels of Education (Primary, Lower, and Upper Secondary Level)",
     xlab = "Percentage of Water Accessibility") 
plot(p3, add = T, col = "khaki4", xlim=c(0,100),
     main = "Distribution of Water Accessibility in Countries for Different Levels of Education (Primary, Lower, and Upper Secondary Level)",
     xlab = "Percentage of Water Accessibility")
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

#### Eploring the bank data ####

str(bankcc)
unique(bankcc$value_latest_year)
unique(bankcc$value_2017)

# doing both sexes
bankccs <- bankcc[bankcc$sexDesc == "Both sexes",]
bankccs <- bankccs[c(1,8)]
bankccs <- na.omit(bankccs)

# The histogram - only 15-17 years old
hist(bankccs$value_latest_year)
qqnorm(bankcc$value_latest_year)

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
qqnorm(sqrt(childeconoccsa$value_latest_year))
hist(sqrt(childeconoccsa$value_latest_year))
qqnorm(sqrt(childeconoccsa$value_latest_year))

#### HISTOGRAM AND CHECKING THE DISTRIBUTIONS ####
# Water
hist(proficiencyupsec$value_latest_year, 
     main = "Histogram of Water Accessibility for Upper Secondary Level Students",
     col = "skyblue",
     xlab = "Percentage of Water Accessibility for Upper Secondary Level Students")
qqnorm(proficiencyupsec$value_latest_year, 
      col = "skyblue",
      main = "QQ Plot of Water Accessibility for Upper Secondary Level Students")
# ANSWER: Not normal
# If not, how would you transform them to make them more normal?
# ANSWER: Subset the data, and exclude the 100% - then we log it
proficiencyupsec1 <- proficiencyupsec[proficiencyupsec$value_latest_year != "100",]
hist(proficiencyupsec1$value_latest_year)
qqnorm(proficiencyupsec1$value_latest_year)
hist(log(proficiencyupsec1$value_latest_year),
     main = "Transformed Histogram of Water Accessibility for Upper Secondary Level Students",
     xlab = "Percentage of Water Accessibility for Upper Secondary Level Students",
     col = "skyblue2")
qqnorm(log(proficiencyupsec1$value_latest_year),
       col = "skyblue2",
       main = "Transformed QQ Plot of Water Accessibility for Upper Secondary Level Students")
hist(sqrt(proficiencyupsec1$value_latest_year))
hist(-1/sqrt(proficiencyupsec1$value_latest_year))
qqnorm((proficiencyupsec1$value_latest_year)^2)

# Skills
hist(skillatestlit$value_latest_year,
     main = "Histogram of Literacy Rate of Adults",
     col = "lightsalmon",
     xlab = "Percentage of Adults Who Are Literated")
qqnorm(skillatestlit$value_latest_year,
       main = "QQ Plot of Literacy Rate of Adults",
       col = "lightsalmon")
# ANSWER: Not normal
# To transform them - square them!
hist((skillatestlit$value_latest_year)^2,
     main = "Transformed Histogram of Literacy Rate of Adults",
     col = "lightsalmon3",
     xlab = "Percentage of Adults Who Are Literated")
qqnorm((skillatestlit$value_latest_year)^2,
       main = "Transformed QQ Plot of Literacy Rate of Adults",
       col = "lightsalmon3",
       xlab = "Percentage of Adults Who Are Literated")

# Bank
hist(bankccs$value_latest_year,
     main = "Histogram of Adults Who Own Bank Account",
     col = "mistyrose3",
     xlab = "Percentage of Adults Who Own Bank Account")
qqnorm(bankccs$value_latest_year,
     main = "QQ Plot of Adults Who Own Bank Account",
     col = "mistyrose3")
# not normal
# transform: logarithm! but because it is bimodal, so it won't become normal
hist(sqrt(bankccs$value_latest_year),
     main = "Transformed Histogram of Adults Who Own Bank Account",
     col = "mistyrose4",
     xlab = "Percentage of Adults Who Own Bank Account")
qqnorm(sqrt(bankccs$value_latest_year),
       col = "mistyrose4",
       main = "Transformed QQ Plot of Adults Who Own Bank Account")

# Child labor
hist(childeconoccsa$value_latest_year,
     main = "Histogram of Children (5 to 17 years old) Who Are Engaged in Economic Activity",
     col = "lightsteelblue3",
     xlab = "Percentage of Children (5 to 17 years old) Who Are Engaged in Economic Activity")
qqnorm(childeconoccsa$value_latest_year,
       main = "QQ Plot of Children (5 to 17 years old) Who Are Engaged in Economic Activity",
       col = "lightsteelblue3")
# Answer: NOT NORMAL
hist(sqrt(childeconoccsa$value_latest_year),
     main = "Transformed Histogram of Children (5 to 17 years old) Who Are Engaged in Economic Activity",
     col = "lightsteelblue4",
     xlab = "Percentage of Children (5 to 17 years old) Who Are Engaged in Economic Activity")
qqnorm(sqrt(childeconoccsa$value_latest_year),
       main = "Transformed QQ Plot of Children (5 to 17 years old) Who Are Engaged in Economic Activity",
       col = "lightsteelblue4")

#### GENERATE THE SIX SCATTER PLOTS  - BEFORE TRANSFORMATION ####

# goal 1 correlation - water accessibility and literacy
goal1 <- merge(proflatestup,skillatestlit,by=c("geoAreaName"))
plot(goal1$value_latest_year.x, goal1$value_latest_year.y,
     main = "Plot Between the Upper Secondary Level Students Who Got Access to Water and The Literacy Rate in Different Countries",
     xlab = "Percentage of upper secondary level students with water access",
     ylab = "Percentage of adults who are literated",
     col = "skyblue3")
cor(goal1$value_latest_year.x, goal1$value_latest_year.y)
(cor(goal1$value_latest_year.x, goal1$value_latest_year.y))^2
legend("topleft", title = "Correlation", c("0.5543319"), cex=0.8)
abline(lm(goal1$value_latest_year.y~goal1$value_latest_year.x), col="tomato3",lty=c(1,2), lwd=c(1, 4))

# Goal 2 correlation - bank account and child labour
goal7 <- merge(bankccs, childeconoccsa, by = c("geoAreaName"))
plot(goal7$value_latest_year.x, goal7$value_latest_year.y,
     main = "Plot Between Adults (15 years and older) with Bank Account and Children (5-17 years old) Engaged in Economic Activity in Different Countries",
     ylab = "Percentage of Children Engaged in Economic Activity",
     xlab = "Percentage of Adults with Bank Account",
     col = "lightsalmon3")
cor(goal7$value_latest_year.x, goal7$value_latest_year.y)
(cor(goal7$value_latest_year.x, goal7$value_latest_year.y))^2
legend("topleft", title = "Correlation", c("0.1911614"), cex=0.8)
abline(lm(goal7$value_latest_year.y~goal7$value_latest_year.x), col="tomato3",lty=c(1,2), lwd=c(1, 4))

# Bank and Literacy rate
goal8 <- merge(bankccs, skillatestlit, by = c("geoAreaName"))
plot(goal8$value_latest_year.x, goal8$value_latest_year.y,
     main = "Plot Between Adults (15 years and older) with Bank Account and Literacy Rate in Different Countries",
     xlab = "Percentage of Adults with Bank Account",
     ylab = "Percentage of Adults Who Are Literated",
     col = "navajowhite4")
cor(goal8$value_latest_year.x, goal8$value_latest_year.y)
(cor(goal8$value_latest_year.x, goal8$value_latest_year.y))^2
legend("topleft", title = "Correlation", c("0.009858447"), cex=0.8)
abline(lm(goal8$value_latest_year.y~goal8$value_latest_year.x), col="tomato3",lty=c(1,2), lwd=c(1, 4))

# child labour and literacy
goal10 <- merge(childeconoccsa, skillatestlit, by = c("geoAreaName"))
plot(goal10$value_latest_year.x, goal10$value_latest_year.y,
     main = "Plot Between Children (5-17 years old) Engaged in Economic Activity and Literacy Rate in Different Countries",
     xlab = "Percentage of Children (5-17 years old) Engaged in Economic Activity",
     ylab = "Percentage of Adults Who Are Literated",
     col = "plum4")
cor(goal10$value_latest_year.x, goal10$value_latest_year.y)
(cor(goal10$value_latest_year.x, goal10$value_latest_year.y))^2
legend("bottomright", title = "Correlation", c("0.04021274"), cex=0.8)
abline(lm(goal8$value_latest_year.y~goal8$value_latest_year.x), col="tomato3",lty=c(1,2), lwd=c(1, 4))

# water accessibility and child labour
goal12 <- merge(proflatestup, childeconoccsa, by = c("geoAreaName"))
plot(goal12$value_latest_year.x, goal12$value_latest_year.y,
     main = "Plot Between Water Accessibility and Children (5-17 years old) Engaged in Different Countries",
     ylab = "Percentage of Children (5-17 years old) Engaged in Economic Activity",
     xlab = "Percentage of upper secondary level students with water access",
     col = "orchid4")
cor(goal12$value_latest_year.x, goal12$value_latest_year.y)
(cor(goal12$value_latest_year.x, goal12$value_latest_year.y)^2)
legend("topleft", title = "Correlation", c("0.005642049"), cex=0.8)
abline(lm(goal12$value_latest_year.y~goal12$value_latest_year.x), col="tomato3",lty=c(1,2), lwd=c(1, 4))

# water accessibility and bank account
goal14 <- merge(proflatestup, bankccs, by = c("geoAreaName"))
plot(goal14$value_latest_year.x, goal14$value_latest_year.y,
     main = "Plot Between Adults (15 years and older) with Bank Account and Water Accessibility in Different Countries",
     ylab = "Percentage of Adults with Bank Accoun",
     xlab = "Percentage of upper secondary level students with water access",
     col = "orchid4")
(cor(goal14$value_latest_year.x, goal14$value_latest_year.y))^2
legend("topleft", title = "Correlation", c("0.2512748"), cex=0.8)
abline(lm(goal14$value_latest_year.y~goal14$value_latest_year.x), col="tomato3",lty=c(1,2), lwd=c(1, 4))

#### GENERATE THE SIX SCATTER PLOTS  - AFTER TRANSFORMATION ####

# goal 1 correlation - water accessibility and literacy
# first we need to merge the two dataset first
proficiencyupsec1 <- proficiencyupsec1[c(1,24)]
proficiencyupsec1 <- na.omit(proficiencyupsec1)
goal15 <- merge(proficiencyupsec1, skillatestlit, by=c("geoAreaName"))
plot(log(goal15$value_latest_year.x), (goal15$value_latest_year.y)^2,
     main = "Transformed Plot Between the Upper Secondary Level Students Who Got Access to Water and The Literacy Rate in Different Countries",
     xlab = "Percentage of upper secondary level students with water access",
     ylab = "Percentage of adults who are literated",
     col = "skyblue3")
cor(log(goal15$value_latest_year.x), (goal15$value_latest_year.y)^2)
cor(log(goal15$value_latest_year.x), (goal15$value_latest_year.y)^2)^2
legend("topleft", title = "Correlation", c("0.763839"), cex=0.8)
abline(a, col="tomato3",lty=c(1,2), lwd=c(1, 4))

# goal 2 correlation - bank account and child labor
plot(sqrt(goal7$value_latest_year.x), sqrt(goal7$value_latest_year.y),
     main = "Transformed Plot Between Adults (15 years and older) with Bank Account and Children (5-17 years old) Engaged in Economic Activity in Different Countries",
     ylab = "Percentage of Children Engaged in Economic Activity",
     xlab = "Percentage of Adults with Bank Account",
     col = "lightsalmon3")
cor(sqrt(goal7$value_latest_year.x), sqrt(goal7$value_latest_year.y))
cor(sqrt(goal7$value_latest_year.x), sqrt(goal7$value_latest_year.y))^2
legend("topleft", title = "Correlation", c("0.2078239"), cex=0.8)
abline(b, col="tomato3",lty=c(1,2), lwd=c(1, 4))

# Bank and Literacy rate
plot(sqrt(goal8$value_latest_year.x), (goal8$value_latest_year.y)^2,
     main = "Transformed Plot Between Adults (15 years and older) with Bank Account and Literacy Rate in Different Countries",
     xlab = "Percentage of Adults with Bank Account",
     ylab = "Percentage of Adults Who Are Literated",
     col = "navajowhite4")
cor(sqrt(goal8$value_latest_year.x), (goal8$value_latest_year.y)^2)
cor(sqrt(goal8$value_latest_year.x), (goal8$value_latest_year.y)^2)^2
legend("topleft", title = "Correlation", c("0.001988804"), cex=0.8)
abline(c, col="tomato3",lty=c(1,2), lwd=c(1, 4))

# child labour and literacy
plot(sqrt(goal10$value_latest_year.x), (goal10$value_latest_year.y)^2,
     main = "Transformed Plot Between Children (5-17 years old) Engaged in Economic Activity and Literacy Rate in Different Countries",
     xlab = "Percentage of Children (5-17 years old) Engaged in Economic Activity",
     ylab = "Percentage of Adults Who Are Literated",
     col = "plum4")
cor(sqrt(goal10$value_latest_year.x), (goal10$value_latest_year.y)^2)
cor(sqrt(goal10$value_latest_year.x), (goal10$value_latest_year.y)^2)^2
legend("bottomright", title = "Correlation", c("0.06312214"), cex=0.8)
abline(d, col="tomato3",lty=c(1,2), lwd=c(1, 4))

# water accessibility and child labour
plot(log(goal12$value_latest_year.x), sqrt(goal12$value_latest_year.y),
     main = "Transformed Plot Between Water Accessibility and Children (5-17 years old) Engaged in Different Countries",
     ylab = "Percentage of Children (5-17 years old) Engaged in Economic Activity",
     xlab = "Percentage of upper secondary level students with water access",
     col = "orchid4")
cor(log(goal12$value_latest_year.x), sqrt(goal12$value_latest_year.y))
cor(log(goal12$value_latest_year.x), sqrt(goal12$value_latest_year.y))^2
legend("topleft", title = "Correlation", c("0.001701394"), cex=0.8)
abline(e, col="tomato3",lty=c(1,2), lwd=c(1, 4))

# water accessibility and bank account
goal14 <- merge(proflatestup, bankccs, by = c("geoAreaName"))
plot(log(goal14$value_latest_year.x), sqrt(goal14$value_latest_year.y),
     main = "Transformed Plot Between Adults (15 years and older) with Bank Account and Water Accessibility in Different Countries",
     ylab = "Percentage of Adults with Bank Accoun",
     xlab = "Percentage of upper secondary level students with water access",
     col = "orchid4")
cor(log(goal14$value_latest_year.x), sqrt(goal14$value_latest_year.y))
legend("topleft", title = "Correlation", c("0.4385116"), cex=0.8)
abline(f, col="tomato3",lty=c(1,2), lwd=c(1, 4))

##### PLOTTING THE RESIDUALS BEFORE TRANSFORMED ####
plot(residuals(a1) ~ predict(a1),
     main = "Residual Plot Between the Upper Secondary Level Students Who Got Access to Water and The Literacy Rate in Different Countries",
     xlab = "Predicted of Percentage of upper secondary level students with water access",
     ylab = "Residuals of Percentage of adults who are literated",
     col = "skyblue3")
plot(residuals(b1) ~ predict(b1),
     main = "Residual Plot Between Adults (15 years and older) with Bank Account and Children (5-17 years old) Engaged in Economic Activity in Different Countries",
     ylab = "Residuals of Percentage of Children Engaged in Economic Activity",
     xlab = "Predicted of Percentage of Adults with Bank Account",
     col = "lightsalmon3")
plot(residuals(c1) ~ predict(c1),
     main = "Residual Plot Between Adults (15 years and older) with Bank Account and Literacy Rate in Different Countries",
     xlab = "Predicted of Percentage of Adults with Bank Account",
     ylab = "Residuals of Percentage of Adults Who Are Literated",
     col = "navajowhite4")
plot(residuals(d1) ~ predict(d1),
     main = "Residual Plot Between Children (5-17 years old) Engaged in Economic Activity and Literacy Rate in Different Countries",
     xlab = "Predicted of Percentage of Children (5-17 years old) Engaged in Economic Activity",
     ylab = "Residuals of Percentage of Adults Who Are Literated",
     col = "plum4")
plot(residuals(e1) ~ predict(e1),
     main = "Residual Plot Between Water Accessibility and Children (5-17 years old) Engaged in Different Countries",
     ylab = "Residuals of Percentage of Children (5-17 years old) Engaged in Economic Activity",
     xlab = "Predicted of Percentage of upper secondary level students with water access",
     col = "orchid4")
plot(residuals(f1) ~ predict(f1),
     main = "Residual Plot Between Adults (15 years and older) with Bank Account and Water Accessibility in Different Countries",
     ylab = "Residuals of Percentage of Adults with Bank Accoun",
     xlab = "Predicted of Percentage of upper secondary level students with water access",
     col = "orchid4")

#### PLOTTING THE RESIDUALS AFTER TRANSFORMED ####
plot(residuals(a) ~ predict(a),
     main = "Residual Transformed Plot Between the Upper Secondary Level Students Who Got Access to Water and The Literacy Rate in Different Countries",
     xlab = "Predicted of Percentage of upper secondary level students with water access",
     ylab = "Residuals of Percentage of adults who are literated",
     col = "skyblue3")
plot(residuals(b) ~ predict(b),
     main = "Residual Transformed Plot Between Adults (15 years and older) with Bank Account and Children (5-17 years old) Engaged in Economic Activity in Different Countries",
     ylab = "Residuals of Percentage of Children Engaged in Economic Activity",
     xlab = "Predicted of Percentage of Adults with Bank Account",
     col = "lightsalmon3")
plot(residuals(c) ~ predict(c),
     main = "Residual Transformed Plot Between Adults (15 years and older) with Bank Account and Literacy Rate in Different Countries",
     xlab = "Predicted of Percentage of Adults with Bank Account",
     ylab = "Residuals of Percentage of Adults Who Are Literated",
     col = "navajowhite4")
plot(residuals(d) ~ predict(d),
     main = "Residual Transformed Plot Between Children (5-17 years old) Engaged in Economic Activity and Literacy Rate in Different Countries",
     xlab = "Predicted of Percentage of Children (5-17 years old) Engaged in Economic Activity",
     ylab = "Residuals of Percentage of Adults Who Are Literated",
     col = "plum4")
plot(residuals(e) ~ predict(e),
     main = "Residual Transformed Plot Between Water Accessibility and Children (5-17 years old) Engaged in Different Countries",
     ylab = "Residuals of Percentage of Children (5-17 years old) Engaged in Economic Activity",
     xlab = "Predicted of Percentage of upper secondary level students with water access",
     col = "orchid4")
plot(residuals(f) ~ predict(f),
     main = "Residual Transformed Plot Between Adults (15 years and older) with Bank Account and Water Accessibility in Different Countries",
     ylab = "Residuals of Percentage of Adults with Bank Accoun",
     xlab = "Predicted of Percentage of upper secondary level students with water access",
     col = "orchid4")

#### LINEAR MODEL BEFORE TRANSFORMATION ####
a1 <- lm(goal1$value_latest_year.y ~ goal1$value_latest_year.x)
b1 <- lm(goal7$value_latest_year.y ~ goal7$value_latest_year.x)
c1 <- lm(goal8$value_latest_year.y ~ goal8$value_latest_year.x)
d1 <- lm(goal10$value_latest_year.y ~ goal10$value_latest_year.x)
e1 <- lm(goal12$value_latest_year.y ~ goal12$value_latest_year.x)
f1<- lm(goal14$value_latest_year.y ~ goal14$value_latest_year.x)

#### LINEAR MODEL AFTER TRANSFORMATION ####
a <- lm((goal15$value_latest_year.y)^2 ~ log(goal15$value_latest_year.x))
b <- lm(sqrt(goal7$value_latest_year.y) ~ sqrt(goal7$value_latest_year.x))
c <- lm((goal8$value_latest_year.y)^2 ~ sqrt(goal8$value_latest_year.x))
d <- lm((goal10$value_latest_year.y)^2 ~ sqrt(goal10$value_latest_year.x))
e <- lm(sqrt(goal12$value_latest_year.y) ~ log(goal12$value_latest_year.x))
f<- lm(sqrt(goal14$value_latest_year.y) ~ log(goal14$value_latest_year.x))

#### SUMMARY OF LINEAR MODEL BEFORE TRANSFORMATION ####
summary(a1)
summary(b1)
summary(c1)
summary(d1)
summary(e1)
summary(f1)
#### SUMMARY OF LINEAR MODEL AFTER TRANSFORMATION ####
summary(a)
summary(b)
summary(c)
summary(d)
summary(e)
summary(f)

