###EXPLORATORY DATA ANALYSIS WEEK 1

##PRINCIPLES OF ANALYTICAL GRAPHICS

# 1. always show comparisons
# 2. show causlity, mechanism
# 3. show multivariate data
# 4. integrate multiple modes of evidence
# 5. Describe and document the evidence
# 6. Content is king

## EXPLANATORY GRAPHS (PART 1) 1 DIMENSIONAL SUMMARIES

download.file("https://raw.githubusercontent.com/jtleek/modules/master/04_ExploratoryAnalysis/exploratoryGraphs/data/avgpm25.csv", 
              destfile = "data/avgpm25.csv", method = 'curl')
?read.csv
pollution <- read.csv('data/avgpm25.csv', stringsAsFactors = F)
head(pollution)
#which contries have pm25 > 12 

library(dplyr)

class(pollution$pm25)
filter(pollution, pm25 >12) %>% select(fips) 
pollution[pollution$pm25 >12, 'fips']

summary(pollution$pm25)
boxplot(pollution$pm25)
boxplot(pollution$pm25, col = "sky blue")
hist(pollution$pm25, col = "green")
hist(pollution$pm25, col = "green", breaks = 100) #breaks give the number of bars
?abline
abline(h = 15) #adds line into whatever plot is last open
abline(v = mean(pollution$pm25))
abline(v = mean(pollution$pm25), lwd = 3, col = "grey")
barplot(table(pollution$region), col = 'navy blue', main = "Number of Countries in each Region")

## EXPLANATORY GRAPHS (PART 2) 2 DIMENSIONAL SUMMARIES

boxplot(pm25 ~ region, data = pollution, col = "orange")
?par
par(mfrow = c(2, 1), mar = c(3,3,2,1))
hist(filter(pollution, region == 'east')$pm25, col = 'green')
hist(subset(pollution, region == 'west')$pm25, col = 'blue')

par(mfrow=c(1, 1))
with(pollution, plot(latitude, pm25))
?abline
#for details of line types colors or other graphic things related go to ?par and search the parameter in help
abline(h = 10, lwd = 2, col = 'dark grey', lty = 6)

head(pollution)
with(pollution, plot(longitude, latitude))
with(pollution, plot(latitude, pm25, col=region)) #not working, because region is character not factor
pollution$region <- as.factor(pollution$region)
with(pollution, plot(latitude, pm25, col=region)) #now working
with(pollution, plot(longitude, latitude, col = region))
abline(v = mean(pollution$longitude))

par(mfrow = c(1, 2), mar = c(4,4,2,1))
with(filter(pollution, region == 'east'), plot(latitude, pm25, main = 'East'))
with(filter(pollution, region == 'west'), plot(latitude, pm25, main = 'West'))

## PLOTTING SYSTEMS

#introductiona and differences between base, lattice and ggplot2

## BASE PLOTTING SYSTEM (PART 1)

par('lty') # this gives you default value for every parameter
par('col')
par('mar')
par('mfrow')
par(mfrow = (c(1,1)))

hist(airquality$Ozone)
with(airquality, plot(Wind, Ozone))

colnames(airquality)
table(airquality$Month)
?transform

boxplot(Ozone ~ Month, airquality, xlab = 'Month', ylab = 'Ozone')

airquality <- transform(airquality, Month = factor(Month))
class(airquality$Month)

boxplot(Ozone ~ Month, airquality, xlab = 'Month', ylab = 'Ozone') #exactly the same dont know why he did it in the first place

#important base plot parameters pch (point symbol), lty (line type), lwd (line widthe), col, xlab, ylab, 
#                               las(orientation of axis label), bg(background color), mar(margin), oma(outer margin size), 
#                               mfrow (plots filled row-wise), mfcol (filled col-wise)


#important base plotting functions, plot, lines, points, legend, text, title, mtext, axis #all examplified below
library(help = 'graphics') #for the details, library(help = 'package name')

with(airquality, plot(Wind, Ozone))
title("Wind and Ozone in NewYork City")
with(subset(airquality, Month == 5), points(Wind, Ozone, col = 'red'))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = 'blue'))
legend("topright", pch = 1, col = c("red", "blue"), legend = c("May", "Other months"))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2, col = "purple")


par(mfrow = c(1, 2))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation" )
})


colnames(airquality)

par('oma')
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0,0,2, 0))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone vs Wind")
  plot(Solar.R, Ozone, main = "Ozone vs Solar Radiation")
  plot(Temp, Ozone, main = "Ozone vs Temperature")
  mtext("Ozone and Weather in New York", outer = T, cex = 1.5)
})

dev.off() #resets par values

## BASE PLOTTING EXAMPLE

head(mtcars)

with(mtcars, plot(mpg, hp, type = 'n')) #type 'n' plots empty plot
with(filter(mtcars, am == 1), points(mpg, hp, col = "red"))
with(filter(mtcars, am == 0), points(mpg, hp, col = "blue"))
with(filter(mtcars, am == 1), abline(lm(hp~mpg), col = "red"))
with(filter(mtcars, am == 0), abline(lm(hp~mpg), col = "blue"))
legend("topright", pch = 1, col = c('red', 'blue'), legend = c("Automatic", "Manual"))

##GRAPHICS DEVICES IN R (PART 1)

?pdf
pdf(file = "data/firstpdfplot.pdf") #initiates a pdf graphing device
with(mtcars, plot(mpg, hp, type = 'n')) #type 'n' plots empty plot
with(filter(mtcars, am == 1), points(mpg, hp, col = "red"))
with(filter(mtcars, am == 0), points(mpg, hp, col = "blue"))
with(filter(mtcars, am == 1), abline(lm(hp~mpg), col = "red"))
with(filter(mtcars, am == 0), abline(lm(hp~mpg), col = "blue"))
legend("topright", pch = 1, col = c('red', 'blue'), legend = c("Automatic", "Manual")) 
title(main = "Average vs Horsepower")  #make whatever plot you want to make
dev.off() # this closes the graphics and finalize the pdf

##GRAPHICS DEVICES IN R (PART 2)

#vector formats : pdf, svg, win.metafile, postscript, #size doesn't matter, can be zoomed endlessly
#bitmap formats : png, jpeg, tiff, bmp, fixed pixels

jpeg("data/firstjpegplot.jpg", quality = 200)
with(mtcars, plot(mpg, hp, type = 'n')) #type 'n' plots empty plot
with(filter(mtcars, am == 1), points(mpg, hp, col = "red"))
with(filter(mtcars, am == 0), points(mpg, hp, col = "blue"))
with(filter(mtcars, am == 1), abline(lm(hp~mpg), col = "red"))
with(filter(mtcars, am == 0), abline(lm(hp~mpg), col = "blue"))
legend("topright", pch = 1, col = c('red', 'blue'), legend = c("Automatic", "Manual")) 
title(main = "Average vs Horsepower")  #make whatever plot you want to make
dev.off() # this closes the graphics and finalize the pdf
plot.new() # same as above
dev.cur() # which graphics device is currently functional

jpeg()
bitmap()
pdf()
dev.cur() #tells me 3 is active
dev.list() #tells me two are open
dev.set(2)
dev.cur()
dev.off()
dev.list()
dev.off()

with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser Data")
dev.copy(png, file = "data/geyserplot.png") #whenever you feel like the plot is ready, export it in the required format
dev.off()


##SWIRL

library(swirl)
packageVersion('swirl')
install_from_swirl("Exploratory Data Analysis")

swirl()

