#####################################################################
### influence of software model on student performance and affect ###
#####################################################################

#install.packages("reshape"); install.packages("plyr"); install.packages("car"); install.packages("reverse.code"); install.packages("dplyr"); install.packages("MASS")
library("reshape"); library("plyr"); library("dplyr"); library("car"); library("MASS")

setwd("~/Desktop/Freeman_stats/")

#read in data
a <- read.csv("data_final.csv", na.strings=c("", "NA", "<NA>"), stringsAsFactors=FALSE)
#attach(a)
head(a)

#reverse code where necessary
which(colnames(a)=="PreQ12a")
a[,47] <- ifelse(a[,47] == 5, 1,
                 ifelse(a[,47] == 4, 2,
                        ifelse(a[,47] == 3, 3, 
                               ifelse(a[,47] == 2, 4, 
                                      ifelse(a[,47] == 1, 5, "NA"))))) 
which(colnames(a)=="PostQ12a")
a[,105] <- ifelse(a[,105] == 5, 1,
                  ifelse(a[,105] == 4, 2,
                         ifelse(a[,105] == 3, 3, 
                                ifelse(a[,105] == 2, 4, 
                                       ifelse(a[,105] == 1, 5, "NA"))))) 
which(colnames(a)=="PreQ12g")
a[,53] <- ifelse(a[,53] == 5, 1,
                 ifelse(a[,53] == 4, 2,
                        ifelse(a[,53] == 3, 3, 
                               ifelse(a[,53] == 2, 4, 
                                      ifelse(a[,53] == 1, 5, "NA"))))) 
which(colnames(a)=="PostQ12g")
a[,111] <- ifelse(a[,111] == 5, 1,
                  ifelse(a[,111] == 4, 2,
                         ifelse(a[,111] == 3, 3, 
                                ifelse(a[,111] == 2, 4, 
                                       ifelse(a[,111] == 1, 5, "NA"))))) 
a[,47] <- as.numeric(a[,47])
as.numeric(a[,105])
as.numeric(a[,53])
as.numeric(a[,111])

#melt and reshape
b <- melt(a, id=c("ID", "section","Gender","URM","FirstGen","EOP","UW_GPA","Transfer","SATM","SATV", "Ex3tot"))
head(b)
#attach(b)
#c <- b[order("ID"),]

c <- b
dim(c)

# create "student of interest" ID column
c$SOI <- ifelse(c$URM == 1, 1, 
                ifelse(c$EOP == 1, 1,
                       ifelse(c$FirstGen == 1, 1, 0)))
head(c)
dim(c)

# designate affect questions by cluster (1 = Effort Reg; 2 = Intrinsic Goal; 3 = Task Value)

c$AC <- ifelse(c$variable =="PreQ12a", 1,
               ifelse(c$variable =="PreQ12b", 1, 
                      ifelse(c$variable =="PreQ12c", 2, 
                             ifelse(c$variable =="PreQ12d", 3, 
                                    ifelse(c$variable =="PreQ12e", 3,
                                           ifelse(c$variable =="PreQ12f", 3,
                                                  ifelse(c$variable =="PreQ12g", 1, 
                                                         ifelse(c$variable =="PreQ12h", 3, 
                                                                ifelse(c$variable =="PreQ12i", 2, 
                                                                       ifelse(c$variable =="PreQ12j", 3, 
                                                                              ifelse(c$variable =="PreQ12k", 3, 
                                                                                     ifelse(c$variable =="PreQ12l", 2, 
                                                                                            ifelse(c$variable =="PreQ12m", 1, 
                                                                                                   ifelse(c$variable =="PreQ12n", 2, NA)
                                                                                            )
                                                                                     )
                                                                              )
                                                                       )
                                                                )
                                                         )
                                                  )
                                           )
                                    )
                             )
                      )
               )
)

# create column identifying whether question content is covered in the software model 
n <- length(c$ID)
InApp <- rep(0,n)
for(i in 1:n)
{
  if (c$variable[i] == "PreQ1a") InApp[i] <- 1
  if (c$variable[i] == "PreQ1b") InApp[i] <- 1 
  if (c$variable[i] == "PreQ2a") InApp[i] <- 1 
  if (c$variable[i] == "PreQ2b") InApp[i] <- 1 
  if (c$variable[i] == "PreQ2c") InApp[i] <- 1 
  if (c$variable[i] == "PreQ2d") InApp[i] <- 1  
  if (c$variable[i] == "PreQ4a") InApp[i] <- 1  
  if (c$variable[i] == "PreQ4b") InApp[i] <- 1   
  if (c$variable[i] == "PreQ4c") InApp[i] <- 1
  if (c$variable[i] == "PreQ4d") InApp[i] <- 1
  if (c$variable[i] == "PreQ5a") InApp[i] <- 1
  if (c$variable[i] == "PreQ6a") InApp[i] <- 1  
  if (c$variable[i] == "PreQ6b") InApp[i] <- 1 
  if (c$variable[i] == "PreQ6d") InApp[i] <- 1 
  if (c$variable[i] == "PreQ7a") InApp[i] <- 1
  if (c$variable[i] == "PreQ7b") InApp[i] <- 1
  if (c$variable[i] == "PreQ7c") InApp[i] <- 1
  if (c$variable[i] == "PreQ7d") InApp[i] <- 1
  if (c$variable[i] == "PreQ8a") InApp[i] <- 1
  if (c$variable[i] == "PreQ8c") InApp[i] <- 1
  if (c$variable[i] == "PreQ9a") InApp[i] <- 1
  if (c$variable[i] == "PreQ9b") InApp[i] <- 1
  if (c$variable[i] == "PreQ9c") InApp[i] <- 1
  if (c$variable[i] == "PreQ9d") InApp[i] <- 1
  if (c$variable[i] == "PreQ9e") InApp[i] <- 1
  if (c$variable[i] == "PreQ10a") InApp[i] <- 1
  if (c$variable[i] == "PreQ10b") InApp[i] <- 1
  if (c$variable[i] == "PreQ10c") InApp[i] <- 1
  if (c$variable[i] == "PreQ10d") InApp[i] <- 1
  if (c$variable[i] == "PostQ1a") InApp[i] <- 1
  if (c$variable[i] == "PostQ1b") InApp[i] <- 1 
  if (c$variable[i] == "PostQ2a") InApp[i] <- 1 
  if (c$variable[i] == "PostQ2b") InApp[i] <- 1 
  if (c$variable[i] == "PostQ2c") InApp[i] <- 1 
  if (c$variable[i] == "PostQ2d") InApp[i] <- 1  
  if (c$variable[i] == "PostQ4a") InApp[i] <- 1  
  if (c$variable[i] == "PostQ4b") InApp[i] <- 1   
  if (c$variable[i] == "PostQ4c") InApp[i] <- 1
  if (c$variable[i] == "PostQ4d") InApp[i] <- 1
  if (c$variable[i] == "PostQ5a") InApp[i] <- 1
  if (c$variable[i] == "PostQ6a") InApp[i] <- 1  
  if (c$variable[i] == "PostQ6b") InApp[i] <- 1 
  if (c$variable[i] == "PostQ6d") InApp[i] <- 1 
  if (c$variable[i] == "PostQ7a") InApp[i] <- 1
  if (c$variable[i] == "PostQ7b") InApp[i] <- 1
  if (c$variable[i] == "PostQ7c") InApp[i] <- 1
  if (c$variable[i] == "PostQ7d") InApp[i] <- 1
  if (c$variable[i] == "PostQ8a") InApp[i] <- 1
  if (c$variable[i] == "PostQ8c") InApp[i] <- 1
  if (c$variable[i] == "PostQ9a") InApp[i] <- 1
  if (c$variable[i] == "PostQ9b") InApp[i] <- 1
  if (c$variable[i] == "PostQ9c") InApp[i] <- 1
  if (c$variable[i] == "PostQ9d") InApp[i] <- 1
  if (c$variable[i] == "PostQ9e") InApp[i] <- 1
  if (c$variable[i] == "PostQ10a") InApp[i] <- 1
  if (c$variable[i] == "PostQ10b") InApp[i] <- 1
  if (c$variable[i] == "PostQ10c") InApp[i] <- 1
  if (c$variable[i] == "PostQ10d") InApp[i] <- 1
}

d <- cbind(c, InApp)
head(d)

# separate pre- and post- content scores from single column
pre <- d[grep("Pre", d$variable),]
post <- d[grep("Post", d$variable),]
names(pre)[names(pre)=="variable"] <- "ContentID"
names(pre)[names(pre)=="value"] <- "ContentPreScore"
names(post)[names(post)=="value"] <- "ContentPostScore"

#combine pre- and post- content scores as separate columns
ContentPostScore <- post$ContentPostScore
data <- cbind(pre, ContentPostScore)
data$ContentPreScore<-as.numeric(data$ContentPreScore)
content_data <- data

# separate pre- and post- affect scores
affect_data <- data[grep("Q12", data$ContentID),]
names(affect_data)[names(affect_data)=="ContentID"] <- "AffectID"
names(affect_data)[names(affect_data)=="ContentPreScore"] <- "AffectPreScore"
names(affect_data)[names(affect_data)=="ContentPostScore"] <- "AffectPostScore"

#cleaning data

content_data <- content_data[complete.cases(content_data[c(2,17)]),]
affect_data <- affect_data[complete.cases(affect_data[c(2,17)]),]
content_data$ContentPreScore <- as.numeric(as.factor(content_data$ContentPreScore)) # 0,1 becomes 1,2
content_data$ContentPostScore <- as.numeric(as.factor(content_data$ContentPostScore)) # 0,1 becomes 1,2
affect_data$AffectPreScore <- as.numeric(as.factor(affect_data$AffectPreScore)) # 0,1 becomes 1,2
affect_data$AffectPostScore <- as.numeric(as.factor(affect_data$AffectPostScore)) # 0,1 becomes 1,2
content_data <- subset(content_data, select=-c(AC))
content_data <- na.omit(content_data)
affect_data <- subset(affect_data, select=-c(AC))
affect_data <- na.omit(affect_data)
head(content_data)
head(affect_data)

# add variable SumPreQ which is the sum of the content prescores
content_data_NEWall <-ddply(content_data, c('ID'), transform, SumPreQ = sum(ContentPreScore)) 

# add variable SumPostQ which is the sum of the content postscores - probably don't need this but will include for consistency
content_data_NEWPostall <-ddply(content_data_NEWall, c('ID'), transform, SumPostQ = sum(ContentPostScore))

# Drop duplicates so that there is only one observation per stuent
content_data_NEW1 <- content_data_NEWPostall[!duplicated(content_data_NEWPostall$ID),]

# Drop NAs (15 NAs in SumPreQ and 3 NAs in ex3tot): table(is.na(content_data_NEW1$SumPreQ))
content_data <- content_data_NEW1[is.na(content_data_NEW1$SumPreQ)=="FALSE" & is.na(content_data_NEW1$Ex3tot)=="FALSE",]

##########################
# basic patterns #
##########################

#basic patterns with exam totals
aggregate(Ex3tot~section + Gender, content_data, mean)
aggregate(Ex3tot~section, content_data, mean)
ttest_gender <- t.test(Ex3tot~Gender, content_data)
print(ttest_gender)
ttest_section <- t.test(Ex3tot~section, content_data)
print(ttest_section)
boxplot(Ex3tot~section, data=content_data, main="Exam 3 Scores by Section", xlab="Section", ylab="Mean Exam Score")
boxplot(Ex3tot~Gender, data=content_data, main="Exam 3 Scores by Gender", xlab="Gender", ylab="Mean Exam Score")

#basic patterns w/ post scores
aggregate(ContentPostScore~section+Gender, content_data, mean)
aggregate(ContentPostScore~section, content_data, mean)
ttest_CPS_gender <- t.test(ContentPostScore~Gender, content_data)
print(ttest_CPS_gender)
ttest_CPS_section <- t.test(ContentPostScore~section, content_data)
print(ttest_CPS_section)
boxplot(ContentPostScore~section, data=content_data, main="Post Scores by Section", xlab="Section", ylab="Mean Post Score")
boxplot(ContentPostScore~Gender, data=content_data, main="Post Scores by Gender", xlab="Gender", ylab="Mean Post Score")

#basic patterns w/ affect
aggregate(AffectPostScore~section+Gender, affect_data, mean)
aggregate(AffectPostScore~section, affect_data, mean)
ttest_affect_gender <- t.test(AffectPostScore~Gender, affect_data)
print(ttest_affect_gender)
ttest_affect_section <- t.test(AffectPostScore~section, affect_data)
print(ttest_affect_section)
boxplot(AffectPostScore~section, data=affect_data, main="Affect Scores by Section", xlab="Section", ylab="Mean Post Score")
boxplot(AffectPostScore~Gender, data=affect_data, main="Post Scores by Gender", xlab="Gender", ylab="Mean Post Score")

##########################
# lookin' at some models #
##########################

### modeling for exam total
modAll.1e <- lm(Ex3tot ~ UW_GPA + ContentID + SATM + SATV + ContentPreScore + SOI + section + Gender + section*SOI + section*Gender, data=content_data)
# need data=data or to name each variable data$UW_GPA to avoid using attach()
summary(modAll.1e)
anova(modAll.1e, test="Chisq") 

# remove ContentID as least sig
modAll.2e <- lm(Ex3tot ~ UW_GPA + SATM + SATV + ContentPreScore + SOI + section + Gender + section*SOI + section*Gender, data=content_data)
summary(modAll.2e)
anova(modAll.1e, modAll.2e, test="Chisq") 

#remove SATM as least significant
modAll.3e <- lm(Ex3tot ~ UW_GPA + SATV + ContentPreScore + SOI + section + Gender + section*SOI + section*Gender, data=content_data)
summary(modAll.3e)
anova(modAll.2e, modAll.3e, test="Chisq") 

#remove content pre-score as least significant
modAll.4e <- lm(Ex3tot ~ UW_GPA + SATV + SOI + section + Gender + section*SOI + section*Gender, data=content_data)
summary(modAll.4e)
anova(modAll.3e, modAll.4e, test="Chisq") 

#remove Section*Gender as least significant
modAll.5e <- lm(Ex3tot ~ UW_GPA + SATV + SOI + section + Gender + section*SOI, data=content_data)
summary(modAll.5e)
anova(modAll.4e, modAll.5e, test="Chisq") 

#all variables now highly significant
AIC(modAll.1e,modAll.2e,modAll.3e,modAll.4e,modAll.5e)
BIC(modAll.1e,modAll.2e,modAll.3e,modAll.4e,modAll.5e)

#AIC and BIC agree on modAll.2e

### modeling for ContentPostScore
modAll.1p <- lm(ContentPostScore ~ UW_GPA + ContentID + SATM + SATV + ContentPreScore + SOI + section + Gender + section*SOI + section*Gender, data=content_data)
summary(modAll.1p)
anova(modAll.1p, test="Chisq") 

#remove SOI*section
modAll.2p <- lm(ContentPostScore ~ UW_GPA + ContentID + SATM + SATV + ContentPreScore + SOI + section + Gender + section*Gender, data=content_data)
summary(modAll.2p)
anova(modAll.1p, modAll.2p, test="Chisq") 

#remove section*Gender
modAll.3p <- lm(ContentPostScore ~ UW_GPA + ContentID + SATM + SATV + ContentPreScore + SOI + section + Gender, data=content_data)
summary(modAll.3p)
anova(modAll.2p, modAll.3p, test="Chisq") 

#remove SATM
modAll.4p <- lm(ContentPostScore ~ UW_GPA + ContentID + SATV + ContentPreScore + SOI + section + Gender, data=content_data)
summary(modAll.4p)
anova(modAll.3p, modAll.4p, test="Chisq") 

#remove SATV
modAll.5p <- lm(ContentPostScore ~ UW_GPA + ContentID + ContentPreScore + SOI + section + Gender, data=content_data)
summary(modAll.4p, modAll.5p, test="Chisq")
#anova(modAll.modAll.5p, test="Chisq") 

#remove Gender
modAll.6p <- lm(ContentPostScore ~ UW_GPA + ContentID + ContentPreScore + SOI + section, data=content_data)
summary(modAll.5p, modAll.6p)
#anova(modAll.4p, modAll.5p, test="Chisq") 

#remove section
modAll.7p <- lm(ContentPostScore ~ UW_GPA + ContentID + ContentPreScore + SOI, data=content_data)
summary(modAll.7p)
anova(modAll.6p, modAll.7p, test="Chisq") 

#all variables are highly significant

#AIC(modAll.1p,modAll.2p,modAll.3p,modAll.4p,modAll.5p)
#BIC(modAll.1p,modAll.2p,modAll.3p,modAll.4p,modAll.6p)

### modelling for AffectPostScore
modAll.1b <- polr(as.factor(AffectPostScore) ~ UW_GPA + AffectPreScore + AffectID + as.factor(AC) + SOI + Gender + section*SOI + AC*SOI + section*Gender, data=affect_data,Hess=TRUE) #, na.action=na.omit)
summary(modAll.1a)

modAll.2b <- polr(as.factor(AffectPostScore) ~ UW_GPA + AffectPreScore + AffectID + as.factor(AC) + SOI + Gender + section*SOI + AC*SOI + section*Gender, data=affect_data,Hess=TRUE) #, na.action=na.omit)
summary(modAll.1a)

# dropping gender*section
modAll.2a <- polr(as.factor(AffectPostScore) ~ UW_GPA + AffectPreScore + AffectID + as.factor(AC) + SOI + Gender + section*SOI, data=affect_data,Hess=TRUE) #, na.action=na.omit)
summary(modAll.2a)
anova(modAll.1a, modAll.2a)

# dropping Gender
modAll.3a <- polr(as.factor(AffectPostScore) ~ UW_GPA + AffectPreScore + AffectID + as.factor(AC) + SOI + section*SOI, data=affect_data,Hess=TRUE) #, na.action=na.omit)
summary(modAll.3a)
anova(modAll.2a, modAll.3a)

# dropping SOI
modAll.4a <- polr(as.factor(AffectPostScore) ~ UW_GPA + AffectPreScore + AffectID + as.factor(AC) + section*SOI, data=affect_data,Hess=TRUE) #, na.action=na.omit)
summary(modAll.4a)
anova(modAll.3a, modAll.4a)

AIC(modAll.1a,modAll.2a,modAll.3a,modAll.4a)
BIC(modAll.1a,modAll.2a,modAll.3a,modAll.4a)

# dropping SOI
modAll.1a <- polr(as.factor(AffectPostScore) ~ UW_GPA + AffectPreScore + AffectID + as.factor(AC) + SOI + Gender + section*SOI + section*Gender, data=affect_data,Hess=TRUE) #, na.action=na.omit)

AffectPostScore ~ section + SOI + affectID + AC*section + AC*SOI

#### Notes: ####
# - keep code in safe place
# - write a report (models and betas) (rmarkdown)
# - timesheet (bring hours forward)
# - remove *all* of the NAs
# - polr: can't include question ID and cluster in same model. but if we want to know *what* aspect of affect is interacting with SOI, interact cluster with SOI (affect ID, not affect cluster)
# - (would look like: AffectPostScore ~ section + SOI + affectID + AC*section + AC*SOI) (do full model selection process)
####
