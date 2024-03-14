# Matloff Fast R intro
# https://github.com/matloff/fasteR

# Lesson 1 and 2 ----

mean(Nile)
Nile

plot(mtcars$wt,mtcars$mpg)
plot(Nile)
Nile

# Nile is a vector. What is the length of this vector?

length(Nile)

# args can check what a function does
sort(Nile)
args(sort)

# A first graph
hist(Nile)

hist(Nile, breaks = 20)

# Ex
sort(Nile, decreasing = TRUE)
hist(Nile, plot = FALSE)

# Lesson 3 ----

Nile[c(2, 5, 6)]
mean(Nile[81:100])
# find the mean and the standard deviation of the time period
n81100 <- Nile[81:100]
mean(n81100)
sd(n81100)

mean(Nile[74:99])
mean(Nile[1:50])
mean(Nile[50:100])

# Lesson 4 ----

sum(Nile > 1200)
# which function selecting which elements have the query
which(Nile > 1200)
length(which(Nile > 1200))
# what were the river flows in those years?
Nile[Nile > 1200]
# or
which1200 <- which(Nile > 1200)
Nile[which1200]

# Ex
length(which(Nile < 950))
which950 <- which(Nile > 950)
Nile[which950]
median(Nile[which950])

sum(c(2, 1, 1, 6, 8, 5))
sum(Nile[1:25])
# find all except for the first (i.e. use negative indices)
x <- c(5,12,13,8)
x[-1]
x[c(-1,-4)] # takes away the first and fourth index

# Lesson 5 ----
?ToothGrowth

head(ToothGrowth)
tg <- ToothGrowth

mean(tg$len)
# get element in row 3 column 1
tg[3,1]
tg$len[3]
tg$dose[6]
tg[3,2]
tg$supp

# taking mean of an entire column without a name
mean(tg[,1])
z <- tg[2:5,c(1,3)] # extracting rows 2 through 5 and columns 1 and 3
z
# extract columns but keep all rows
y <- tg[ ,c(1,3)]
y
nrow(tg)

# Ex

length(which(tg$len < 16 & tg$supp == "VC"))
selecttg <- which(tg$len < 16 & tg$supp == "VC")
tg[selecttg,]
mean(tg[selecttg,"len"])
mean(tg[selecttg,1])

head(faithful)
max(faithful$eruptions)

order(faithful$eruptions)
# returns index of the highest values
index_highest <- sort(faithful$eruptions, index.return=TRUE, decreasing=TRUE)$ix
index_highest[1:5] #index of the 5 highest

# of the highest 50, what is the lowest waiting?
highest_50 <- index_highest[1:50]
faithful[highest_50,]
min(faithful[highest_50,"waiting"])

# get all with waiting lower than 50
faithful[faithful$waiting < 50,]
hist(faithful[faithful$waiting < 50, "eruptions"])

# get high waiting and low eruptions
length(which(faithful$eruptions < 2 & faithful$waiting > 50))
mean(faithful[which(faithful$eruptions < 2 & faithful$waiting > 50),"waiting"])

# Lesson 6 ----

class(tg$supp)
levels(tg$supp)

# EX
head(CO2)
class(CO2$Plant)
class(CO2$Type)
class(CO2$Treatment)
class(CO2$conc)
class(CO2$uptake)

# Lesson 7 ----

whichOJ <- which(tg$supp == "OJ")
mean(tg[whichOJ,1])

tgoj <- tg[tg$supp == "OJ",]
mean(tgoj$len)

# Lesson 8 ----

tg[tg$supp=='OJ' & tg$len < 8.8,]

w <- tg[tg$len > 28 | tg$dose == 1.0,]
head(w)
nrow(w) # how many cases satisfy the condition

# selecting columns we want
lendose <- tg[,c(1,3)]
head(lendose)

lendose <- tg[,c("len", "dose")]
head(lendose)

# Ex

whicherup <- which(faithful$eruptions>3 & faithful$waiting>80)
nrow(faithful[whicherup,])
length(whicherup)

# Lesson 9 ----
# tapply(what to split, what criterion to use for splitting, 
#   what to do with the resulting grouped data)
tapply(tg$len,tg$supp,mean) # result is saved as named vector

# Getting the length of each group
tapply(PlantGrowth$weight,PlantGrowth$group,length)
tapply(PlantGrowth$group,PlantGrowth$group,length)

# Ex

tapply(mtcars$mpg,mtcars$cyl,mean)
table(mtcars$cyl)

# only for vs = tru
mt_vs <- mtcars[mtcars$vs == 1,]
nrow(mtcars)
tapply(mt_vs$mpg,mt_vs$cyl,mean)

row.names(mtcars)

# Lession 10 ----

pima <- read.csv('http://heather.cs.ucdavis.edu/FasteR/data/Pima.csv',header=TRUE)
head(pima)
nrow(pima)
dim(pima)

pg <- pima$glucose
pg1 <- pg[pg > 0] # excluding glucose levels of 0
length(pg1)

# let's code the 0 as NA, since they are obviously not actually 0

pima$glucose[pima$glucose == 0] <- NA

# easier version:
glc <- pima$glucose # just a copy of the vector
z <- glc == 0 # indexing all the 0
glc[z] <- NA # assigning NA to all with index z
pima$glucose <- glc # replace original data with NA data

sum(is.na(pima$glucose))

# Ex
table(pima$pregnant)
table(pima$diastolic)
pima$diastolic[pima$diastolic == 0] <- NA
table(pima$triceps)
table(pima$insulin)
table(pima$bmi)
pima$bmi[pima$bmi == 0] <- NA
table(pima$diabetes)
table(pima$age)

hist(Nile)

# Nile[Nile < 600] <- NA
mean(Nile, na.rm = TRUE)

# Lesson 11 ----

# split(what to split, what criterion to use for splitting) 
mtmpg <- mtcars$mpg
mtl <- split(mtmpg,mtcars$cyl)
mtl

mtl[[1]]
mtl$`4`
names(mtl) <- c('four','six','eight') # changing names of list
mtl
mtl[[2]][3] # third element in the vector which is in the list
mtl$six[3]

# Ex

mtl$four[5:8]
mtl$six[length(mtl$six) - 2 : 0]
tail(mtl$six, 3)
#easier:
last_three <- length(mtl$six) - 2 # last 3 index starting with
mtl$six[last_three : length(mtl$six)] # last 3 until end of vector

mean(mtcars$drat)

horse_t_weight <- mtcars$hp/mtcars$wt
mean(horse_t_weight)

# Lesson 12 ----

plot(Nile)
which(Nile < 600) # which tells us which of these are TRUE

# Ex
which(Nile > 1200) + 1871

plot(AirPassengers)
AirPassengers[which(AirPassengers > 500)]

z <- hist(AirPassengers)
str(z)
mean(z$counts)

# flow of the Nile in year 1925
Nile[1 + 1925 - 1871]

# Lesson 13 ----

# Lesson 14 ----

load(url(
'https://github.com/matloff/fasteR/blob/master/data/prgeng.RData?raw=true'))
head(prgeng)
nrow(prgeng)

plot(prgeng$age,prgeng$wageinc)
# prevent overplotting
rowNumbers <- sample(1:nrow(prgeng),2500)
pe2500 <- prgeng[rowNumbers,]
plot(pe2500$age,pe2500$wageinc)

plot(pe2500$age,pe2500$wageinc,col=pe2500$sex)
plot(pe2500$age,pe2500$wageinc,col=as.factor(pe2500$sex),
   xlab='age',ylab='wage',cex=0.6)

# Ex
plot(pe2500$age,pe2500$wageinc,col=as.factor(pe2500$educ),
   xlab='age',ylab='wage',cex=0.6)

# Lesson 15 ----

wageByGender <- split(prgeng$wageinc,prgeng$sex)
dm <- density(wageByGender[[1]])
dw <- density(wageByGender[[2]])
plot(dw,col='red')
points(dm,cex=0.2)

# Ex
head(prgeng)

wageByOcc <- split(prgeng$wageinc,prgeng$occ)
d100 <- density(wageByOcc[[1]])
d101 <- density(wageByOcc[[2]])
d102 <- density(wageByOcc[[3]])
d106 <- density(wageByOcc[[4]])
d140 <- density(wageByOcc[[5]])
d141 <- density(wageByOcc[[6]])

plot(d100,col='red')
points(d101,cex=0.2)
points(d102,cex=0.2,col='blue')
points(d106,cex=0.2,col='yellow')

ageByOcc <- split(prgeng$age,prgeng$occ)

rowNumbers <- sample(1:nrow(prgeng),1000)
plot(wageByOcc[[1]][rowNumbers],ageByOcc[[1]][rowNumbers])

# Lesson 16 ----

# mean of elements greater than d
mgd <- function(x,d) mean(x[x > d])
mgd(tg$len,10.2)

# you can save any function
save(mgd,file='mean_greater_than_d')
load('mean_greater_than_d')

# Ex:
cdg <- function(x,d) length(which(x>d))

tgsupp <- split(tg,tg$supp)

cdg(tgsupp$OJ$len,15)


n0 <- function(x) length(which(x==0))

n0(prgeng$wageinc)

hld <- function(x,d) hist(x[which(x>d)])

hld(tgsupp$OJ$len,15)


# Lesson 17 ----

for (i in 1:9) print(sum(pima[,i] == 0)) # find sum of all 0

for (i in 2:6) {
   zeroIndices <- which(pima[,i] == 0)
   pima[zeroIndices,i] <- NA
}

# Lesson 18 ----

zerosToNAs <- function(d,cols)
	{
 for (j in cols) {
      NArows <- which(d[,j] == 0)
	      d[NArows,j] <- NA
	   }
	   d
	}

# The loop goes through d, one column at a time. Since d[,j] means all of column j of d, then which(d[,j] == 0) will give us the indices in that column of elements that are 0s. Those indices in turn are row numbers in d. In other words, NArows is a vector cntaining the row numbers of the 0s in column j. In line 5, then, we replace the 0s we've found in column j by NAs.


countNAs <- function(dfr){
   for (j in colnames(dfr)) {
      print(length(which(is.na(dfr[,j]))))
      }
}

d <- data.frame(x=c(1,0,NA),y=c(NA,NA,13)) 
d

length(is.na(d[,2]))
length(which(is.na(d[,1])))
length(which(is.na(d[,2])))

countNAs(dfr = d)


# Lesson 19 ----

f <- function(x,y)
{
   s <- x + y
   d <- x - y
   c(s,d)
}

# Lesson 20 ----


