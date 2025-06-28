#Name: Joel Munoz
#Course: STAT 43000
#Assignment: Project
#Due: May 3, 2025

#----------------------------------------
#          Preparing the Data           |
#---------------------------------------

#import
white = read.csv("C:/Users/Joel/Downloads/wine+quality/winequality-white.csv", header = T, sep = ";")
red = read.csv("C:/Users/Joel/Downloads/wine+quality/winequality-red.csv", header = T, sep = ";")

#add column "color"
white$color = "white"
red$color = "red"

head(white)
head(red)
dim(white)
dim(red)

#combine
data = rbind(white, red)
data
dim(data)
attach(data)

4898+1599

#percentages
percentOfRed=4898/6497
percentOfRed
percentOfWhite=1599/6497
percentOfWhite
percentHQ = 1277/6497
percentHQ

sum(quality<6)
sum(quality>5)
sum(quality>6)
sum(quality>7)
sum(quality>8)

#add column "highQuality" which only includes data points with a quality of 7 or more.

data$highQuality = ifelse(quality > 6, 1, 0)
head(data)

tail(data)

names(data)
#-----------------------------------------
#          Graphical Analysis            |
#-----------------------------------------

library(ggplot2)
library(geomtextpath)


#-----------Boxplots--------------------

ggplot(data, aes(x=color, y=quality)) + 
  geom_boxplot(fill = c("red4", "lightyellow")) + 
  xlab("Color") + ylab("Quality") + 
  ggtitle("Wine Color vs Quality")

ggplot(data, aes(x=color, y=fixed.acidity)) +
  geom_boxplot(fill = c("red4", "lightyellow")) + 
  xlab("Color")  + 
  ggtitle("Wine Color vs Fixed Acitdity")

ggplot(data, aes(x=color, y=volatile.acidity)) + 
  geom_boxplot(fill = c("red4", "lightyellow")) +
  xlab("Color") + 
  ggtitle("Wine Color vs Volatile Acidity")

ggplot(data, aes(x=color, y=citric.acid)) +
  geom_boxplot(fill = c("red4", "lightyellow")) +
  xlab("Color") + 
  ggtitle("Wine Color vs Citric Acid")

ggplot(data, aes(x=color, y=residual.sugar)) +
  geom_boxplot(fill = c("red4", "lightyellow")) +
  xlab("Color") +
  ggtitle("Wine Color vs Residual Sugar") +
  coord_cartesian(ylim = c(0,30))

ggplot(data, aes(x=color, y=chlorides)) +
  geom_boxplot(fill = c("red4", "lightyellow")) + 
  xlab("Color") +
  ggtitle("Wine Color vs Chlorides")

ggplot(data, aes(x=color, y=free.sulfur.dioxide)) +
  geom_boxplot(fill = c("red4", "lightyellow")) +
  xlab("Color") + 
  ggtitle("Wine Color vs Free Sulfur Dioxide")

ggplot(data, aes(x=color, y=total.sulfur.dioxide)) + 
  geom_boxplot(fill = c("red4", "lightyellow")) + 
  xlab("Color") +
  ggtitle("Wine Color vs Total Sulfur Dioxide")

ggplot(data, aes(x=color, y=density)) + 
  geom_boxplot(fill = c("red4", "lightyellow")) +
  xlab("Color") +
  ggtitle("Wine Color vs Density")

ggplot(data, aes(x=color, y=pH)) + 
  geom_boxplot(fill = c("red4", "lightyellow")) +
  xlab("Color") + 
  ggtitle("Wine Color vs pH")

ggplot(data, aes(x=color, y=sulphates)) +
  geom_boxplot(fill = c("red4", "lightyellow")) +
  xlab("Color") + 
  ggtitle("Wine Color vs Sulphates")

#-------------Line Plot: y=quality-----------------

ggplot(data, aes(x=fixed.acidity, y=quality, group=color, color=color)) +
  geom_line(alpha = 0.8) +
  geom_labelsmooth(aes(label = color))

ggplot(data, aes(x=fixed.acidity, y=quality, group=color, color=color)) +
  geom_line(alpha = 0.8)+
  geom_labelsmooth(aes(label = color))

ggplot(data, aes(x=volatile.acidity, y=quality, group=color, color=color)) +
  geom_line(alpha = 0.8)+
  geom_labelsmooth(aes(label = color))

ggplot(data, aes(x=citric.acid, y=quality, group=color, color=color)) +
  geom_line(alpha = 0.8)+
  geom_labelsmooth(aes(label = color))+
  coord_cartesian(xlim = c(0,1))

ggplot(data, aes(x=residual.sugar, y=quality, group=color, color=color)) +
  geom_line(alpha=0.8) + coord_cartesian(xlim = c(0,25))+
  geom_labelsmooth(aes(label = color))

ggplot(data, aes(x=chlorides, y=quality, group=color, color=color))+   
  geom_line(alpha = 0.8) + coord_cartesian(xlim=c(0,0.2))+
  geom_labelsmooth(aes(label = color))

ggplot(data, aes(x= free.sulfur.dioxide, y=quality, group=color, color=color))+   
  geom_line(alpha = 0.8)+
  geom_labelsmooth(aes(label = color))

ggplot(data, aes(x=total.sulfur.dioxide, y=quality, group=color, color=color))+   
  geom_line(alpha = 0.8)+
  geom_labelsmooth(aes(label = color))

ggplot(data, aes(x=density, y=quality, group=color, color=color))+   geom_line(alpha = 0.8) +    coord_cartesian(xlim=c(0.989,1.005))+
  geom_labelsmooth(aes(label = color))

ggplot(data, aes(x=pH, y=quality, group=color, color=color))+   
  geom_line(alpha = 0.8)+
  geom_labelsmooth(aes(label = color))

ggplot(data, aes(x=sulphates, y=quality, group=color, color=color))+   
  geom_line(alpha = 0.8)+
  geom_labelsmooth(aes(label = color))

ggplot(data, aes(x=alcohol, y=quality, group=color, color=color))+   
  geom_line(alpha = 0.8)+
  geom_labelsmooth(aes(label = color))


#-----------Line plot: y = ?-----------------------

ggplot(data, aes(x=fixed.acidity, y=pH, group=color, color=color)) +
  geom_line(alpha = 0.8)

ggplot(data, aes(x=volatile.acidity, y=pH, group=color, color=color)) +
  geom_line(alpha = 0.8)

ggplot(data, aes(x=residual.sugar, y=pH, group=color, color=color)) +
  geom_line(alpha=0.8) + coord_cartesian(xlim = c(0,25))

ggplot(data, aes(x=fixed.acidity, y=citric.acid, group=color, color=color))+   
  geom_line(alpha = 0.8)



#---histogram----------------


library(ggplot2)
library(dplyr)
library(hrbrthemes)

ggplot(data, aes(x=fixed.acidity, fill=color))+
  geom_histogram( color="#e9ecef", alpha = 0.6, position = "identity" )+
  scale_fill_manual(values = c("darkred", "yellow"))+
  theme_ipsum()

ggplot(data, aes(x=volatile.acidity, fill=color))+
  geom_histogram( color="#e9ecef", alpha = 0.6, position = "identity" )+
  scale_fill_manual(values = c("darkred", "yellow"))+
  theme_ipsum()

ggplot(data, aes(x=citric.acid, fill=color))+
  geom_histogram( color="#e9ecef", alpha = 0.6, position = "identity" )+
  scale_fill_manual(values = c("darkred", "yellow"))+
  theme_ipsum()

ggplot(data, aes(x=residual.sugar, fill=color))+
  geom_histogram( color="#e9ecef", alpha = 0.6, position = "identity" )+
  scale_fill_manual(values = c("darkred", "yellow"))+
  theme_ipsum()

ggplot(data, aes(x=chlorides, fill=color))+
  geom_histogram( color="#e9ecef", alpha = 0.6, position = "identity" )+
  scale_fill_manual(values = c("darkred", "yellow"))+
  theme_ipsum()

ggplot(data, aes(x=free.sulfur.dioxide, fill=color))+
  geom_histogram( color="#e9ecef", alpha = 0.6, position = "identity" )+
  scale_fill_manual(values = c("darkred", "yellow"))+
  theme_ipsum()

ggplot(data, aes(x=total.sulfur.dioxide, fill=color))+
  geom_histogram( color="#e9ecef", alpha = 0.6, position = "identity" )+
  scale_fill_manual(values = c("darkred", "yellow"))+
  theme_ipsum()

ggplot(data, aes(x=density, fill=color))+
  geom_histogram( color="#e9ecef", alpha = 0.6, position = "identity" )+
  scale_fill_manual(values = c("darkred", "yellow"))+
  theme_ipsum()

ggplot(data, aes(x=pH, fill=color))+
  geom_histogram( color="#e9ecef", alpha = 0.6, position = "identity" )+
  scale_fill_manual(values = c("darkred", "yellow"))+
  theme_ipsum()

ggplot(data, aes(x=sulphates, fill=color))+
  geom_histogram( color="#e9ecef", alpha = 0.6, position = "identity" )+
  scale_fill_manual(values = c("darkred", "yellow"))+
  theme_ipsum()

ggplot(data, aes(x=alcohol, fill=color))+
  geom_histogram( color="#e9ecef", alpha = 0.6, position = "identity" )+
  scale_fill_manual(values = c("darkred", "yellow"))+
  theme_ipsum()

ggplot(data, aes(x=quality, fill=color))+
  geom_histogram( color="#e9ecef", alpha = 0.6, position = "identity" )+
  scale_fill_manual(values = c("darkred", "yellow"))+
  theme_ipsum()

#-------------------------
#      T Testing        |
#------------------------

t.test(fixed.acidity~color)
t.test(volatile.acidity~color)
t.test(citric.acid~color)
t.test(residual.sugar~color)
t.test(chlorides~color)
t.test(free.sulfur.dioxide~color)
t.test(total.sulfur.dioxide~color)
t.test(data$density~color)
t.test(pH~color)
t.test(sulphates~color)
t.test(alcohol~color)
t.test(quality~color)

#-----------------------------------------
#                  Modeling              |
#-----------------------------------------
library(rcompanion)
library(MASS)
library(rms)

#linear model of entire dataset
lmodel = lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol+color)
lmodel
summary(lmodel)

#logistic model of entire dataset
gmodel = glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol+color)
summary(gmodel)
nagelkerke(gmodel)

#linear model of only white wine
whiteLmodel = lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data = white)
summary(whiteLmodel)

#linear model of only red wine
redLmodel = lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data = red)
summary(redLmodel)



#logistic model of only red wine
redGmodel = glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data = red)
summary(redGmodel)
nagelkerke(redGmodel)


#logistic model of only white wine
whiteGmodel = glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data = white)
summary(whiteGmodel)
nagelkerke(whiteGmodel)


#-----------odds ratio------------------

fa =  2.499e-02  * mean(red$fixed.acidity)
va =   -1.084e+00 * mean(red$volatile.acidity)
ca =  -1.826e-01 * mean(red$citric.acid)
rs =  1.633e-02  * mean(red$residual.sugar)
ch = -1.874e+00  * mean(red$chlorides)
fsd = 4.361e-03 * mean(red$free.sulfur.dioxide)
tsd =  -3.265e-03 * mean(red$total.sulfur.dioxide)
den = -1.788e+01 * mean(red$density)
ph = -4.137e-01 * mean(red$pH)
sul = 9.163e-01 * mean(red$sulphates)
alc = 2.762e-01 * mean(red$alcohol)

2.197e+01 + fa +va + ca + rs + ch + fsd + tsd + den + ph + sul + alc

varNames= c("fixed.acidity" ,       "volatile.acidity" ,   
            "citric.acid"  ,        "residual.sugar"    ,  
            "chlorides"     ,       "free.sulfur.dioxide" ,
            "total.sulfur.dioxide", "density"    ,         
            "pH"        ,           "sulphates"  , "alcohol")
varNames
redcoefs = c(fa, va, ca, rs, ch, fsd, tsd, den, ph, sul, alc)
redcoefs

avgRedWine = data.frame(varNames, redcoefs)
avgRedWine



fa =   6.552e-02  * mean(red$fixed.acidity)
va =   -1.863e+00 * mean(red$volatile.acidity)
ca =  2.209e-02 * mean(red$citric.acid)
rs =   8.148e-02   * mean(red$residual.sugar)
ch =  -2.473e-01 * mean(red$chlorides)
fsd =  3.733e-03* mean(red$free.sulfur.dioxide)
tsd =  -2.857e-04  * mean(red$total.sulfur.dioxide)
den = -1.503e+02 * mean(red$density)
ph =  6.863e-01  * mean(red$pH)
sul =  6.315e-01  * mean(red$sulphates)
alc =  1.935e-01  * mean(red$alcohol)

1.502e+02 + fa +va + ca + rs + ch + fsd + tsd + den + ph + sul + alc

whitecoefs = c(fa ,va , ca , rs , ch , fsd , tsd , den , ph , sul , alc)

avgWhiteWine = data.frame(varNames, whitecoefs)
avgWhiteWine

print(avgRedWine[order(abs(avgRedWine$redcoefs), decreasing = T),])
print(avgWhiteWine[order(abs(avgWhiteWine$whitecoefs), decreasing = T),])
