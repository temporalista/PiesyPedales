##############################################
###Source packloader for easely load packs
##############################################

##biocLite packages required only to produce the graphs
source("http://bioconductor.org/biocLite.R")
#biocLite("graph")
#biocLite("Rgraphviz")


install.packages("devtools")
library(devtools)
SourceURL <- "https://raw.githubusercontent.com/temporalista/r-essentials/master/packloader.R"
source_url(SourceURL)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c("psych","qmethod", "Rgraphviz", "graph"))

#######################
###Input data: statements, sorts and participants
#######################
#update you working directory accordingly
setwd("/Volumes/MacData/odrive/GDrive_UC/Proyectos/riourbano/Q methodology percepcion/")

qsort <- read.csv('inputs/q_sort_rio.csv', header = TRUE, sep = ',', quote = '"', row.names=1)
qstatements <- read.csv('inputs/q_concurso_rio.csv', header = TRUE, sep = ',', quote = '"')
qparticip <- read.csv('inputs/q_participantes_rio.csv', header = TRUE, sep = ',', quote = '"', row.names=1)



#######################
#######################
####RUN QMETHOD COMPUTATION
#######################
nf <-2

qresults <- qmethod(qsort, nfactors = nf, rotation = 'varimax')
summary(qresults)

##Screeplot to check the variances
screeplot(prcomp(qsort), 
          main = "Screeplot of unrotated factors", 
          type = "l")


###plotting a diagram of z-scores for each statement
par(lwd = 1.5, mar = c(4, 4, 0, 0) + 0.1)
plot(qresults, sub='Plot of statement z-scores')
abline(v=0, col='grey')
abline(h = seq(from = 5, to = qresults$brief$nstat, by = 5), col = grey(0.1), lty = 2)

###Check loadings and flags
###Sorts are flagged (*) if they have a significant load (p<0.05) and the
###squared loading is larger than the sum of squared diffs
round(qresults$loa, digits = 2)
qresults$flag
print(qresults$load.and.flags <- loa.and.flags(qresults))



##Characteristics of the factors: 
##av_rel_coef: Average reliability coefficient
##nload: number of loading Qsorts
##Eigenvalues
##expl_var: percentage of explained variance
##composite reliability
##Sandard error of factor scores
qresults$f_char$characteristics



##Screeplot to check the variances
screeplot(prcomp(qsort), 
          main = "Screeplot of unrotated factors", 
          type = "l")

# Put z-scores and factor scores together
scores <- cbind(round(qresults$zsc, digits=2), qresults$zsc_n)

nfactors <- ncol(qresults$zsc)
col.order <- as.vector(rbind(1:nfactors, (1:nfactors)+nfactors))
scores <- scores[col.order]

# merge original statements
scores <- cbind(scores, qstatements$Afirmacion)
scores <- cbind(scores, qstatements$sta_id)

#View ordered scores for each factor
for (i in 1:length(qresults$loa)) {
  View(scores[order(scores[i*2], decreasing = TRUE), ],
       title = paste0("Order for f", i))
}

# Order the table from highest to lowest z-scores for factor 1
scores[order(scores$zsc_f1, decreasing = T), ]
# (to order according to other factors, replace 'f1' for 'f2' etc.)



#### Explore the table of distinguishing and consensus statements
# Full table
qresults$qdc


# Consensus statements
qresults$qdc[which(qresults$qdc$dist.and.cons == "Consensus"), ]

# Statements distinguishing all factors
qresults$qdc[which(qresults$qdc$dist.and.cons == "Distinguishes all"), ]

# Statements distinguishing factor 1 (for results of > 2 factors)
qresults$qdc[which(qresults$qdc$dist.and.cons == "Distinguishes f2 only"), ]


###export all results if needed
export.qm(qresults, file = paste("outputs/qreport_",nf,"f.txt", sep=""), style = "R")
write.csv(qresults$qdc, file = paste("outputs/qdc_",nf,"f.txt", sep=""))
write.csv(scores, file = paste("outputs/scores_",nf,"f.txt", sep=""))

