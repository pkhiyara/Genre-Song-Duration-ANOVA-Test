d = read.csv("/MillionSongsFinal.csv") # read in the Million Songs dataset
# Exploratory Analysis
head(d)
d
d$genre
summary(d$genre) 
# Conducted an ANOVA test
a = aov(duration~genre,data = d)
a
summary(a) 
# Create subsets and histograms for each genre to test for the normality assumption for ANOVA
d.classicpr = subset(d, genre=="classic pop and rock")
hist(d.classicpr$duration)
d.classical = subset(d, genre=="classical")
hist(d.classical$duration)
d.de= subset(d, genre=="dance and electronica")
hist(d.de$duration)
d.folk= subset(d, genre=="folk")
hist(d.folk$duration)
d.hiphop = subset(d, genre=="hip-hop")
hist(d.hiphop$duration)
d.jb = subset(d, genre=="jazz and blues")
hist(d.jb$duration)
d.metal = subset(d, genre=="metal")
hist(d.metal$duration)
d.pop = subset(d, genre=="pop")
hist(d.pop$duration)
d.punk = subset(d, genre=="punk")
hist(d.punk$duration)
d.sr = subset(d, genre=="soul and reggae")
hist(d.sr$duration)
# Import car library
library(car)
# Conduct a Levene's Test for the homogeneity of variance assumption for ANOVA
leveneTest(d$duration~d$genre)
# Conduct Corrected Pairwise t-tests for each genre
pairwise.t.test(d$duration,d$genre,p.adj="none")
# Found the mean and standard deviation for each genre subset created
mean(d.classicpr$duration)
sd(d.classicpr$duration)
mean(d.classical$duration)
sd(d.classical$duration)
mean(d.de$duration)
sd(d.de$duration)
mean(d.folk$duration)
sd(d.folk$duration)
mean(d.hiphop$duration)
sd(d.hiphop$duration)
mean(d.jb$duration)
sd(d.jb$duration)
mean(d.metal$duration)
sd(d.metal$duration)
mean(d.pop$duration)
sd(d.pop$duration)
mean(d.punk$duration)
sd(d.punk$duration)
mean(d.sr$duration)
sd(d.sr$duration)
# Calculate the 95% Confidence Intervals for each genre subset
qt(0.025,225,lower.tail=FALSE)
qt(0.025,7,lower.tail=FALSE)
qt(0.025,29,lower.tail=FALSE)
qt(0.025,121,lower.tail=FALSE)
qt(0.025,6,lower.tail=FALSE)
qt(0.025,41,lower.tail=FALSE)
qt(0.025,31,lower.tail=FALSE)
qt(0.025,12,lower.tail=FALSE)
qt(0.025,8,lower.tail=FALSE)
qt(0.025,56,lower.tail=FALSE)
# Create a dataframe with the mean song duration, upper confidence limit, and lower confidence limit for each genre
d.ci = data.frame(genre=c("classic pop and rock","classical","dance and electronica","folk", "hip-hop", "jazz and blues", "metal", "pop", "punk", "soul and reggae"), duration = c(222.4497, 441.2338, 236.8892, 232.2343, 182.4872, 273.9929, 295.6404, 228.2213, 206.7587, 241.0172), ci.upper = c(234.4262438, 662.6622777, 287.362367, 245.378064, 267.5159788, 312.567583, 336.039265, 264.3808087, 235.8506546, 260.0290904), ci.lower = c(210.4731562, 219.8053223, 186.416033, 219.090536, 97.45842122, 235.418217, 255.241535, 192.0617913, 177.6667454, 222.0053096))
# Import the ggplot2 library
library(ggplot2)
# Plot a bar graph with error bars to visualize the relationship between genres and song durations
ggplot(d.ci,aes(x=genre,y=duration))+geom_bar(stat="identity")+geom_errorbar(aes(ymin=ci.lower,ymax=ci.upper),width=.2)

