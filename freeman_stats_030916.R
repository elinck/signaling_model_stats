#####################################################################
### influence of software model on student performance and affect ###
#####################################################################

#install.packages("reshape"); install.packages("plyr"); install.packages("car"); install.packages("reverse.code"); install.packages("dplyr")
library("reshape"); library("plyr"); library("dplyr"); library("car"); library("reverse.code")

setwd("~/Desktop/Freeman_stats/")

#read in data
a <- read.csv("data_final.csv")
attach(a)
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

#melt and reshape
b <- melt(a, id=c("ID", "section","Gender","URM","FirstGen","EOP","UW_GPA","Transfer","SATM","SATV", "Ex3tot"))
head(b)
attach(b)
c <- b[order(ID),]
head(c)


# create "student of interest" ID column
c$SOI <- ifelse(c$URM == 1, 1, 
                ifelse(c$EOP == 1, 1,
                       ifelse(c$FirstGen == 1, 1, 0)))
head(c)

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
                  
# separate pre- and post- content scores
pre <- d[grep("Pre", e$variable),]
post <- d[grep("Post", e$variable),]
names(pre)[names(pre)=="variable"] <- "ContentID"
names(pre)[names(pre)=="value"] <- "ContentPreScore"
names(post)[names(post)=="value"] <- "ContentPostScore"
ContentPostScore <- post[,13]
g <- cbind(pre, ContentPostScore)
attach(g)

# separate pre- and post- affect scores
affect <- g[grep("Q12", g$ContentID),]
names(affect)[names(affect)=="ContentID"] <- "AffectID"
names(affect)[names(affect)=="ContentPreScore"] <- "AffectPreScore"
names(affect)[names(affect)=="ContentPostScore"] <- "AffectPostScore"

# merge dataframes
data <- merge(g,affe)
head(data)

### now let's look at some *models!* ###

modAll.1e <- lm(Ex3tot ~ UW_GPA + ContentID + ContentPreScore + SOI + AC + section*SOI + section*Gender)




