###analysen###
###version 22-08-2019###
###J Tolsma###
###last data update 22-08-2019

#always start with cleaning up your workspace if you want to run your script again
rm (list = ls( ))

#intall additional packages you will need later.
library(RSiena)
library(igraph)
library(devtools)
require("isnar")
library(foreign)
require(xtable)
#install.packages("devtools")
#install_github("mbojan/isnar")
#install.packages("xtable")

#define additional customized functions
{
fdensity <- function(x) {
	#x is your nomination network
	#should be relationship, divided by all possible off diagonal ties.
	#make sure diagonal cells are NA
	diag(x) <- NA
	x[x==10] <- NA
	sum(x==1, na.rm=T) / (sum(x==1 | x==0, na.rm=T))
}

fdensityintra <- function(x, A) {
	#A is matrix indicating whether nodes constituting dyad have same characteristics
	diag(x) <- NA
	x[x==10] <- NA
	diag(A) <- NA
	sum(x==1 & A==1, na.rm=T) / (sum((x==1 | x==0) & A==1, na.rm=T))
}

fdensityinter <- function(x, A) {
	#A is matrix indicating whether nodes constituting dyad have same characteristics
	diag(x) <- NA
	x[x==10] <- NA
	diag(A) <- NA
	sum(x==1 & A!=1, na.rm=T) / (sum((x==1 | x==0) & A!=1, na.rm=T))
}

fhomomat <- function(x) {
xmat <- matrix(x, nrow=length(x), ncol=length(x))
xmatt <- t(xmat)
xhomo <- xmat==xmatt
return(xhomo)
}

fndyads <- function(x) {
	diag(x) <- NA
	x[x==10] <- NA
	(sum((x==1 | x==0) , na.rm=T))
}

fndyads2 <- function(x, A) {
	diag(x) <- NA
	x[x==10] <- NA
	diag(A) <- NA
	(sum((x==1 | x==0) & A==1, na.rm=T))
}


fscolnet <- function(network, ccovar) {
#Calculate coleman on network level: https://reader.elsevier.com/reader/sd/pii/S0378873314000239?token=A42F99FF6E2B750436DD2CB0DB7B1F41BDEC16052A45683C02644DAF88215A3379636B2AA197B65941D6373E9E2EE413

	fhomomat <- function(x) {
	xmat <- matrix(x, nrow=length(x), ncol=length(x))
	xmatt <- t(xmat)
	xhomo <- xmat==xmatt
	return(xhomo)
	}

	fsumintra <- function(x, A) {
	#A is matrix indicating whether nodes constituting dyad have same characteristics
	diag(x) <- NA
	x[x==10] <- NA
	diag(A) <- NA
	sum(x==1 & A==1, na.rm=T)
	}

	#expecation w*=sum_g sum_i (ni((ng-1)/(N-1)))
	network[network==10] <- NA
	ni <- rowSums(network, na.rm=T)
	ng <- NA
	for (i in 1:length(ccovar)) {ng[i] <- table(ccovar)[rownames(table(ccovar))==ccovar[i]]}
	N <- length(ccovar)
	wexp <- sum(ni*((ng-1)/(N-1)), na.rm=T)

	#wgg1 how many intragroup ties
	w <- fsumintra(network, fhomomat(ccovar))

	Scol_net <- ifelse(w>=wexp, (w-wexp) / (sum(ni, na.rm=T) - wexp), (w - wexp)/wexp)
	return(Scol_net)

}
}


#setwd
setwd("C:\\Users\\Administrator\\Documents\\Shared\\Twitter\\Network data")

#STAP 1: read in data
key <- read.spss('C:\\Users\\Administrator\\Documents\\Shared\\Twitter\\Ego data\\key moederbestand 20171114.sav', use.value.labels=T, to.data.frame=T)
names(key)

load("twitter_20190829.RData")
#load("ans_list.Rdata")
str(twitter_20190829,1)
keyf <- twitter_20190829[[1]]
mydata <- twitter_20190829[[2]]
seats <- twitter_20190829[[3]]

#STAP 2a: descriptive statistics - tables
{
fnet <- mydata$depvars$fnet
atmnet <- mydata$depvars$atmnet
rtnet <- mydata$depvars$rtnet

vrouw <- mydata$cCovars$vrouw
partij <- mydata$cCovars$partij
ethminz <- mydata$cCovars$ethminz
lft <- mydata$cCovars$lft

ethminz <- ethminz + attributes(ethminz)$mean
partij <- partij + attributes(partij)$mean
vrouw <- vrouw + attributes(vrouw)$mean
lft <- lft + attributes(lft)$mean

vrouwm <- fhomomat(vrouw)
partijm <- fhomomat(partij)
ethminzm <- fhomomat(ethminz)

xmat <- matrix(ethminz, nrow=length(ethminz), ncol=length(ethminz))
xmatt <- t(xmat)
minoritym <- xmat==1 & xmatt==1

#for age max 5 year difference / for descriptives
xmat <- matrix(lft, nrow=length(lft), ncol=length(lft))
xmatt <- t(xmat)
lftm <- (abs(xmat - xmatt) < 6)

fndyads2(fnet[,,1], vrouwm)
fndyads2(fnet[,,3], vrouwm)
fndyads2(fnet[,,1], partijm)
fndyads2(fnet[,,3], partijm)
fndyads2(fnet[,,1], ethminzm)
fndyads2(fnet[,,3], ethminzm)

desmat <- matrix(NA, nrow=10, ncol=9)
desmat[1,1] <- fdensity(fnet[,,1])
desmat[1,2] <- fdensity(fnet[,,2])
desmat[1,3] <- fdensity(fnet[,,3])
desmat[2,1] <- fdensityintra(fnet[,,1], vrouwm)
desmat[2,2] <- fdensityintra(fnet[,,2], vrouwm)
desmat[2,3] <- fdensityintra(fnet[,,3], vrouwm)
desmat[3,1] <- fdensityinter(fnet[,,1], vrouwm)
desmat[3,2] <- fdensityinter(fnet[,,2], vrouwm)
desmat[3,3] <- fdensityinter(fnet[,,3], vrouwm)
desmat[4,1] <- fdensityintra(fnet[,,1], partijm)
desmat[4,2] <- fdensityintra(fnet[,,2], partijm)
desmat[4,3] <- fdensityintra(fnet[,,3], partijm)
desmat[5,1] <- fdensityinter(fnet[,,1], partijm)
desmat[5,2] <- fdensityinter(fnet[,,2], partijm)
desmat[5,3] <- fdensityinter(fnet[,,3], partijm)
desmat[6,1] <- fdensityintra(fnet[,,1], ethminzm)
desmat[6,2] <- fdensityintra(fnet[,,2], ethminzm)
desmat[6,3] <- fdensityintra(fnet[,,3], ethminzm)
desmat[7,1] <- fdensityinter(fnet[,,1], ethminzm)
desmat[7,2] <- fdensityinter(fnet[,,2], ethminzm)
desmat[7,3] <- fdensityinter(fnet[,,3], ethminzm)
desmat[8,1] <- fdensityinter(fnet[,,1], minoritym)
desmat[8,2] <- fdensityinter(fnet[,,2], minoritym)
desmat[8,3] <- fdensityinter(fnet[,,3], minoritym)
desmat[9,1] <- fdensityintra(fnet[,,1], lftm)
desmat[9,2] <- fdensityintra(fnet[,,2], lftm)
desmat[9,3] <- fdensityintra(fnet[,,3], lftm)
desmat[10,1] <- fdensityinter(fnet[,,1], lftm)
desmat[10,2] <- fdensityinter(fnet[,,2], lftm)
desmat[10,3] <- fdensityinter(fnet[,,3], lftm)

desmat[1,1 +3] <- fdensity(atmnet[,,1])
desmat[1,2+3] <- fdensity(atmnet[,,2])
desmat[1,3+3] <- fdensity(atmnet[,,3])
desmat[2,1+3] <- fdensityintra(atmnet[,,1], vrouwm)
desmat[2,2+3] <- fdensityintra(atmnet[,,2], vrouwm)
desmat[2,3+3] <- fdensityintra(atmnet[,,3], vrouwm)
desmat[3,1+3] <- fdensityinter(atmnet[,,1], vrouwm)
desmat[3,2+3] <- fdensityinter(atmnet[,,2], vrouwm)
desmat[3,3+3] <- fdensityinter(atmnet[,,3], vrouwm)
desmat[4,1+3] <- fdensityintra(atmnet[,,1], partijm)
desmat[4,2+3] <- fdensityintra(atmnet[,,2], partijm)
desmat[4,3+3] <- fdensityintra(atmnet[,,3], partijm)
desmat[5,1+3] <- fdensityinter(atmnet[,,1], partijm)
desmat[5,2+3] <- fdensityinter(atmnet[,,2], partijm)
desmat[5,3+3] <- fdensityinter(atmnet[,,3], partijm)
desmat[6,1+3] <- fdensityintra(atmnet[,,1], ethminzm)
desmat[6,2+3] <- fdensityintra(atmnet[,,2], ethminzm)
desmat[6,3+3] <- fdensityintra(atmnet[,,3], ethminzm)
desmat[7,1+3] <- fdensityinter(atmnet[,,1], ethminzm)
desmat[7,2+3] <- fdensityinter(atmnet[,,2], ethminzm)
desmat[7,3+3] <- fdensityinter(atmnet[,,3], ethminzm)
desmat[8,1+3] <- fdensityinter(atmnet[,,1], minoritym)
desmat[8,2+3] <- fdensityinter(atmnet[,,2], minoritym)
desmat[8,3+3] <- fdensityinter(atmnet[,,3], minoritym)
desmat[9,1+3] <- fdensityintra(atmnet[,,1], lftm)
desmat[9,2+3] <- fdensityintra(atmnet[,,2], lftm)
desmat[9,3+3] <- fdensityintra(atmnet[,,3], lftm)
desmat[10,1+3] <- fdensityinter(atmnet[,,1], lftm)
desmat[10,2+3] <- fdensityinter(atmnet[,,2], lftm)
desmat[10,3+3] <- fdensityinter(atmnet[,,3], lftm)

desmat[1,1 +6] <- fdensity(rtnet[,,1])
desmat[1,2+6] <- fdensity(rtnet[,,2])
desmat[1,3+6] <- fdensity(rtnet[,,3])
desmat[2,1+6] <- fdensityintra(rtnet[,,1], vrouwm)
desmat[2,2+6] <- fdensityintra(rtnet[,,2], vrouwm)
desmat[2,3+6] <- fdensityintra(rtnet[,,3], vrouwm)
desmat[3,1+6] <- fdensityinter(rtnet[,,1], vrouwm)
desmat[3,2+6] <- fdensityinter(rtnet[,,2], vrouwm)
desmat[3,3+6] <- fdensityinter(rtnet[,,3], vrouwm)
desmat[4,1+6] <- fdensityintra(rtnet[,,1], partijm)
desmat[4,2+6] <- fdensityintra(rtnet[,,2], partijm)
desmat[4,3+6] <- fdensityintra(rtnet[,,3], partijm)
desmat[5,1+6] <- fdensityinter(rtnet[,,1], partijm)
desmat[5,2+6] <- fdensityinter(rtnet[,,2], partijm)
desmat[5,3+6] <- fdensityinter(rtnet[,,3], partijm)
desmat[6,1+6] <- fdensityintra(rtnet[,,1], ethminzm)
desmat[6,2+6] <- fdensityintra(rtnet[,,2], ethminzm)
desmat[6,3+6] <- fdensityintra(rtnet[,,3], ethminzm)
desmat[7,1+6] <- fdensityinter(rtnet[,,1], ethminzm)
desmat[7,2+6] <- fdensityinter(rtnet[,,2], ethminzm)
desmat[7,3+6] <- fdensityinter(rtnet[,,3], ethminzm)
desmat[8,1+6] <- fdensityinter(rtnet[,,1], minoritym)
desmat[8,2+6] <- fdensityinter(rtnet[,,2], minoritym)
desmat[8,3+6] <- fdensityinter(rtnet[,,3], minoritym)
desmat[9,1+6] <- fdensityintra(rtnet[,,1], lftm)
desmat[9,2+6] <- fdensityintra(rtnet[,,2], lftm)
desmat[9,3+6] <- fdensityintra(rtnet[,,3], lftm)
desmat[10,1+6] <- fdensityinter(rtnet[,,1], lftm)
desmat[10,2+6] <- fdensityinter(rtnet[,,2], lftm)
desmat[10,3+6] <- fdensityinter(rtnet[,,3], lftm)

colnames(desmat) <- c("friends w1", "friends w2", "friends w3","atmentions w1","atmentions w2","atmentions w3","retweets w1","retweets w2","retweets w3")
rownames(desmat) <- c("total", "same sex", "different sex", "same party", "different party", "same ethnicity", "different ethnicity", "both minority", "same age (<6)", "different age (>5)")
desmat
#write.csv2(desmat, "20190829desmat.csv")

###assortivity coefficients
fnet[fnet==10] <- NA
rtnet[rtnet==10] <- NA
atmnet[atmnet==10] <- NA

vrouw_int <- as.integer(vrouw) + 1
partij_int <- as.integer(partij)
ethminz <- ethminz + 1

asmat <- matrix(NA, nrow=4, ncol=9)

gfnet1 <- graph_from_adjacency_matrix(fnet[,,1], mode = "directed", weighted = NULL, diag = FALSE,  add.colnames = NA, add.rownames = NA)
gfnet2 <- graph_from_adjacency_matrix(fnet[,,2], mode = "directed", weighted = NULL, diag = FALSE,  add.colnames = NA, add.rownames = NA)
gfnet3 <- graph_from_adjacency_matrix(fnet[,,3], mode = "directed", weighted = NULL, diag = FALSE,  add.colnames = NA, add.rownames = NA)

grtnet1 <- graph_from_adjacency_matrix(rtnet[,,1], mode = "directed", weighted = NULL, diag = FALSE,  add.colnames = NA, add.rownames = NA)
grtnet2 <- graph_from_adjacency_matrix(rtnet[,,2], mode = "directed", weighted = NULL, diag = FALSE,  add.colnames = NA, add.rownames = NA)
grtnet3 <- graph_from_adjacency_matrix(rtnet[,,3], mode = "directed", weighted = NULL, diag = FALSE,  add.colnames = NA, add.rownames = NA)

gatmnet1 <- graph_from_adjacency_matrix(atmnet[,,1], mode = "directed", weighted = NULL, diag = FALSE,  add.colnames = NA, add.rownames = NA)
gatmnet2 <- graph_from_adjacency_matrix(atmnet[,,2], mode = "directed", weighted = NULL, diag = FALSE,  add.colnames = NA, add.rownames = NA)
gatmnet3 <- graph_from_adjacency_matrix(atmnet[,,3], mode = "directed", weighted = NULL, diag = FALSE,  add.colnames = NA, add.rownames = NA)

asmat[1,1] <- assortativity_nominal(gfnet1, types=partij_int, directed = TRUE)
asmat[1,2] <- assortativity_nominal(gfnet2, types=partij_int, directed = TRUE)
asmat[1,3] <- assortativity_nominal(gfnet3, types=partij_int, directed = TRUE)
asmat[2,1] <- assortativity_nominal(gfnet1, types=vrouw_int, directed = TRUE)
asmat[2,2] <- assortativity_nominal(gfnet2, types=vrouw_int, directed = TRUE)
asmat[2,3] <- assortativity_nominal(gfnet3, types=vrouw_int, directed = TRUE)
asmat[3,1] <- assortativity_nominal(gfnet1, types=ethminz, directed = TRUE)
asmat[3,2] <- assortativity_nominal(gfnet2, types=ethminz, directed = TRUE)
asmat[3,3] <- assortativity_nominal(gfnet3, types=ethminz, directed = TRUE)
asmat[4,1] <- assortativity(gfnet1, types1=lft, directed = TRUE)
asmat[4,2] <- assortativity(gfnet2, types1=lft, directed = TRUE)
asmat[4,3] <- assortativity(gfnet3, types1=lft, directed = TRUE)

asmat[1,4] <- assortativity_nominal(grtnet1, types=partij_int, directed = TRUE)
asmat[1,5] <- assortativity_nominal(grtnet2, types=partij_int, directed = TRUE)
asmat[1,6] <- assortativity_nominal(grtnet3, types=partij_int, directed = TRUE)
asmat[2,4] <- assortativity_nominal(grtnet1, types=vrouw_int, directed = TRUE)
asmat[2,5] <- assortativity_nominal(grtnet2, types=vrouw_int, directed = TRUE)
asmat[2,6] <- assortativity_nominal(grtnet3, types=vrouw_int, directed = TRUE)
asmat[3,4] <- assortativity_nominal(grtnet1, types=ethminz, directed = TRUE)
asmat[3,5] <- assortativity_nominal(grtnet2, types=ethminz, directed = TRUE)
asmat[3,6] <- assortativity_nominal(grtnet3, types=ethminz, directed = TRUE)
asmat[4,4] <- assortativity(grtnet1, types1=lft, directed = TRUE)
asmat[4,5] <- assortativity(grtnet2, types1=lft, directed = TRUE)
asmat[4,6] <- assortativity(grtnet3, types1=lft, directed = TRUE)

asmat[1,7] <- assortativity_nominal(gatmnet1, types=partij_int, directed = TRUE)
asmat[1,8] <- assortativity_nominal(gatmnet2, types=partij_int, directed = TRUE)
asmat[1,9] <- assortativity_nominal(gatmnet3, types=partij_int, directed = TRUE)
asmat[2,7] <- assortativity_nominal(gatmnet1, types=vrouw_int, directed = TRUE)
asmat[2,8] <- assortativity_nominal(gatmnet2, types=vrouw_int, directed = TRUE)
asmat[2,9] <- assortativity_nominal(gatmnet3, types=vrouw_int, directed = TRUE)
asmat[3,7] <- assortativity_nominal(gatmnet1, types=ethminz, directed = TRUE)
asmat[3,8] <- assortativity_nominal(gatmnet2, types=ethminz, directed = TRUE)
asmat[3,9] <- assortativity_nominal(gatmnet3, types=ethminz, directed = TRUE)
asmat[4,7] <- assortativity(gatmnet1, types1=lft, directed = TRUE)
asmat[4,8] <- assortativity(gatmnet2, types1=lft, directed = TRUE)
asmat[4,9] <- assortativity(gatmnet3, types1=lft, directed = TRUE)

colnames(asmat) <- c("friends w1", "friends w2", "friends w3","atmentions w1","atmentions w2","atmentions w3","retweets w1","retweets w2","retweets w3")
rownames(asmat) <- c("party", "sex", "ethnicity", "age")
asmat
#write.csv2(asmat, "20190828asmat.csv")

fnet[fnet==10] <- NA
rtnet[rtnet==10] <- NA
atmnet[atmnet==10] <- NA

colmat <- matrix(NA, nrow=3, ncol=9)

colmat[1,1] <- fscolnet(fnet[,,1], partij)
colmat[1,2] <- fscolnet(fnet[,,2], partij)
colmat[1,3] <- fscolnet(fnet[,,3], partij)
colmat[1,4] <- fscolnet(atmnet[,,1], partij)
colmat[1,5] <- fscolnet(atmnet[,,2], partij)
colmat[1,6] <- fscolnet(atmnet[,,3], partij)
colmat[1,7] <- fscolnet(rtnet[,,1], partij)
colmat[1,8] <- fscolnet(rtnet[,,2], partij)
colmat[1,9] <- fscolnet(rtnet[,,3], partij)

colmat[2,1] <- fscolnet(fnet[,,1], vrouw)
colmat[2,2] <- fscolnet(fnet[,,2], vrouw)
colmat[2,3] <- fscolnet(fnet[,,3], vrouw)
colmat[2,4] <- fscolnet(atmnet[,,1], vrouw)
colmat[2,5] <- fscolnet(atmnet[,,2], vrouw)
colmat[2,6] <- fscolnet(atmnet[,,3], vrouw)
colmat[2,7] <- fscolnet(rtnet[,,1], vrouw)
colmat[2,8] <- fscolnet(rtnet[,,2], vrouw)
colmat[2,9] <- fscolnet(rtnet[,,3], vrouw)

colmat[3,1] <- fscolnet(fnet[,,1], ethminz)
colmat[3,2] <- fscolnet(fnet[,,2], ethminz)
colmat[3,3] <- fscolnet(fnet[,,3], ethminz)
colmat[3,4] <- fscolnet(atmnet[,,1], ethminz)
colmat[3,5] <- fscolnet(atmnet[,,2], ethminz)
colmat[3,6] <- fscolnet(atmnet[,,3], ethminz)
colmat[3,7] <- fscolnet(rtnet[,,1], ethminz)
colmat[3,8] <- fscolnet(rtnet[,,2], ethminz)
colmat[3,9] <- fscolnet(rtnet[,,3], ethminz)

colnames(colmat) <- c("friends w1", "friends w2", "friends w3","atmentions w1","atmentions w2","atmentions w3","retweets w1","retweets w2","retweets w3")
rownames(colmat) <- c("party", "sex", "ethnicity")
colmat

#write.csv2(colmat, "20190829colmat.csv")

#coleman homophily per group
coleman(gfnet1, partij)
coleman(grtnet1, partij)
coleman(gatmnet1, partij)

coleman(gfnet1, vrouw)
coleman(grtnet1, vrouw)
coleman(gatmnet1, vrouw)

coleman(gfnet1, ethminz)
coleman(grtnet1, ethminz)
coleman(gatmnet1, ethminz)
}
desmat
asmat
colmat


#STAP 2b: descriptive statistics - figures

#STAP 3: RSiena
# m1: basic model, only minimal structural effects, only same party (controlling for egox en alterx)
# m2: basic model + : uniplex structural effects
# m3: m2 + : by-product hypo: same age, sex, minority status (controlling for egox en alterx)
# m4: m3 + : additional controls "kamerlid2016" "kabinet2016"  "pleklijst"    "pleklijst1"
# m5: m4 + : multiplex effects
# m6: m5 + : time heterogeneity


#print01Report( mydata, modelname="Reporttwitter_20190829")
#let us have a look at all possible effects
#effectsDocumentation()

###model 1: define myeff_m1
{

#check if we observe outdegree differences per partij
fnet <- mydata$depvars$fnet
atmnet <- mydata$depvars$atmnet
rtnet <- mydata$depvars$rtnet
testfnet <- fnet[,,1]
testfnet[testfnet==10] <- NA
testrtnet <- rtnet[,,1]
testrtnet[testrtnet==10] <- NA
testatmnet <- atmnet[,,1]
testatmnet[testatmnet==10] <- NA

outdegreesf <- rowSums(testfnet, na.rm=TRUE)
indegreesf <- colSums(testfnet, na.rm=TRUE)

outdegreesrt <- rowSums(testrtnet, na.rm=TRUE)
indegreesrt <- colSums(testrtnet, na.rm=TRUE)

outdegreesatm <- rowSums(testatmnet, na.rm=TRUE)
indegreesatm <- colSums(testatmnet, na.rm=TRUE)

summary(lm(outdegreesf ~ as.factor(keyf$Partij)))
summary(lm(indegreesf ~ as.factor(keyf$Partij)))
#de enige grote partij die afwijkt is pvv.

summary(lm(outdegreesrt ~ as.factor(keyf$Partij)))
summary(lm(indegreesrt ~ as.factor(keyf$Partij)))
#de enige grote partij die afwijkt is d66.
#alter effect blijkt niet significant, dus weer geschrapt

summary(lm(outdegreesatm ~ as.factor(keyf$Partij)))
#de enige grote partij die afwijkt is d66 en GL.
summary(lm(indegreesatm ~ as.factor(keyf$Partij)))
# de enige grote partij die afwijkt is pvv

myeff <- getEffects( mydata )

#de enige grote partij die afwijkt is pvv.
myeff_m1 <- myeff
myeff_m1
myeff_m1 <- includeEffects(myeff_m1, sameX, interaction1 = "partij", name="fnet" )

# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "cda", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "cu", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "denk", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "d66", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "fvd", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "gl", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "pvda", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "pvdd", name="fnet" )
 myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "pvv", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "sgp", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "sp", name="fnet" )
# # myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "vvd", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "plus", name="fnet" )

# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "cda", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "cu", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "denk", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "d66", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "fvd", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "gl", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "pvda", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "pvdd", name="fnet" )
 myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "pvv", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "sgp", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "sp", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "vvd", name="fnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "plus", name="fnet" )

myeff_m1 <- includeEffects(myeff_m1, sameX, interaction1 = "partij", name="atmnet" )

# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "cda", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "cu", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "denk", name="atmnet" )
 myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "d66", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "fvd", name="atmnet" )
 myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "gl", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "pvda", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "pvdd", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "pvv", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "sgp", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "sp", name="atmnet" )
# # myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "vvd", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "plus", name="atmnet" )

# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "cda", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "cu", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "denk", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "d66", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "fvd", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "gl", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "pvda", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "pvdd", name="atmnet" )
 myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "pvv", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "sgp", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "sp", name="atmnet" )
# # myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "vvd", name="atmnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "plus", name="atmnet" )

myeff_m1 <- includeEffects(myeff_m1, sameX, interaction1 = "partij", name="rtnet" )

# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "cda", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "cu", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "denk", name="rtnet" )
 myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "d66", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "fvd", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "gl", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "pvda", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "pvdd", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "pvv", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "sgp", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "sp", name="rtnet" )
# # myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "vvd", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, egoX, interaction1 = "plus", name="rtnet" )

# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "cda", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "cu", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "denk", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "d66", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "fvd", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "gl", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "pvda", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "pvdd", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "pvv", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "sgp", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "sp", name="rtnet" )
# # myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "vvd", name="rtnet" )
# myeff_m1 <- includeEffects( myeff_m1, altX, interaction1 = "plus", name="rtnet" )

myeff_m1
save(myeff_m1, file="myeff_m1.Rdata")
}
#estimate model
{
# Estimate parameters
myalgorithm <- sienaAlgorithmCreate( projname = 'test')
load("myeff_m1.Rdata")
ansM1 <- siena07( myalgorithm, data = mydata, effects = myeff_m1, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM1
ansM1b <- siena07( myalgorithm, data = mydata, prevAns=ansM1, effects = myeff_m1, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM1b
ansM1c <- siena07( myalgorithm, data = mydata, prevAns=ansM1b, effects = myeff_m1, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM1c
save(ansM1c, file="ansM1c.Rdata")
}

###model 2: define myeff_m2 #structural effects
{
#effectsDocumentation(myeff)

myeff_m2 <- myeff_m1

#according to suggestion of rsiena manual
myeff_m2 <- includeEffects( myeff_m2, inPopSqrt, name="fnet" )
myeff_m2 <- includeEffects( myeff_m2, inPopSqrt, name="atmnet" )
myeff_m2 <- includeEffects( myeff_m2, inPopSqrt, name="rtnet" )

myeff_m2 <- includeEffects( myeff_m2, outActSqrt, name="fnet" )
myeff_m2 <- includeEffects( myeff_m2, outActSqrt, name="atmnet" )
myeff_m2 <- includeEffects( myeff_m2, outActSqrt, name="rtnet" )

myeff_m2 <- includeEffects( myeff_m2, outPopSqrt, name="fnet" )
myeff_m2 <- includeEffects( myeff_m2, outPopSqrt, name="atmnet" )
myeff_m2 <- includeEffects( myeff_m2, outPopSqrt, name="rtnet" )

myeff_m2 <- includeEffects( myeff_m2, transTrip, name="fnet" )
#myeff_m2 <- includeEffects( myeff_m2, transTrip, name="atmnet" )
myeff_m2 <- includeEffects( myeff_m2, transTrip, name="rtnet" )

#test for 3cycle if very significant, try transitive reciprocated triplets effects
myeff_m2 <- includeEffects( myeff_m2, cycle3, name="fnet" )
#myeff_m2 <- includeEffects( myeff_m2, cycle3, name="atmnet" )
myeff_m2 <- includeEffects( myeff_m2, cycle3, name="rtnet" )

#also for proximity
myeff_m2 <- includeEffects( myeff_m2, X, interaction1="afstand", name="fnet" )
#myeff_m2 <- includeEffects( myeff_m2, X, interaction1="afstand", name="atmnet" )
myeff_m2 <- includeEffects( myeff_m2, X, interaction1="afstand", name="rtnet" )

#we additionally for sharedPop
myeff_m2 <- includeEffects( myeff_m2, sharedPop, name="fnet" )
#myeff_m2 <- includeEffects( myeff_m2, sharedPop, name="atmnet" )
myeff_m2 <- includeEffects( myeff_m2, sharedPop, name="rtnet" )

myeff_m2
save(myeff_m2, file="myeff_m2.Rdata")

}
#estimate model #note myeff_m2 is updated
{
myalgorithm <- sienaAlgorithmCreate( projname = 'test')
load("myeff_m2.Rdata")
load("ansM1c.Rdata")
ansM2 <- siena07( myalgorithm, data = mydata, prevAns=ansM1c, effects = myeff_m2, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM2
ansM2b <- siena07( myalgorithm, data = mydata, prevAns=ansM2, effects = myeff_m2, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM2b
ansM2c <- siena07( myalgorithm, data = mydata, prevAns=ansM2b, effects = myeff_m2, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM2c
save(ansM2c, file="ansM2c.Rdata")
#bij atm de ego/alter effecten er nog uit.

myeff_m2 <- includeEffects( myeff_m2, egoX, name="atmnet", interaction1='d66' ,include=FALSE)
myeff_m2 <- includeEffects( myeff_m2, egoX, name="atmnet", interaction1='gl' ,include=FALSE)
myeff_m2 <- includeEffects( myeff_m2, altX, name="atmnet", interaction1='pvv' ,include=FALSE)
myeff_m2
ansM2d <- siena07( myalgorithm, data = mydata, prevAns=ansM2c, effects = myeff_m2, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
myeff_m2 <- includeEffects( myeff_m2, altX, name="fnet", interaction1='pvv' ,include=FALSE)
ansM2e <- siena07( myalgorithm, data = mydata, prevAns=ansM2d, effects = myeff_m2, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM2e
ansM2f <- siena07( myalgorithm, data = mydata, prevAns=ansM2e, effects = myeff_m2, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM2f
ansM2g <- siena07( myalgorithm, data = mydata, prevAns=ansM2f, effects = myeff_m2, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM2g
save(ansM2g, file="ansM2g.Rdata")
save(myeff_m2, file="myeff_m2.Rdata")
}

###model 3: define myeff_m3 #byproduct hypo
{
myeff_m3 <- myeff_m2
#effectsDocumentation(myeff_m3)

#initial checks for ego/alt effects
summary(lm(outdegreesf ~ as.factor(keyf$Geslacht)))
summary(lm(indegreesf ~ as.factor(keyf$Geslacht)))
#geen geslacht effect

summary(lm(outdegreesrt ~ as.factor(keyf$Geslacht)))
summary(lm(indegreesrt ~ as.factor(keyf$Geslacht)))
#ego geslacht effect

summary(lm(outdegreesatm ~ as.factor(keyf$Geslacht)))
summary(lm(indegreesatm ~ as.factor(keyf$Geslacht)))
#geen geslacht effect

names(keyf)
summary(lm(outdegreesf ~ (keyf$GebJaar)))
summary(lm(indegreesf ~ (keyf$GebJaar)))
#ego lft effect

summary(lm(outdegreesrt ~ keyf$GebJaar))
summary(lm(indegreesrt ~ keyf$GebJaar))
#ego / alt lft effect

summary(lm(outdegreesatm ~ keyf$GebJaar))
summary(lm(indegreesatm ~ keyf$GebJaar))
#geen lft effect

summary(lm(outdegreesf ~ as.factor(keyf$EthMinZ)))
summary(lm(indegreesf ~ as.factor(keyf$EthMinZ)))
#geen minority effect

summary(lm(outdegreesrt ~ as.factor(keyf$EthMinZ)))
summary(lm(indegreesrt ~ as.factor(keyf$EthMinZ)))
#geen minority effect

summary(lm(outdegreesatm ~ as.factor(keyf$EthMinZ)))
summary(lm(indegreesatm ~ as.factor(keyf$EthMinZ)))
#geen minority effect

myeff_m3 <- includeEffects(myeff_m3, sameX, interaction1 = "vrouw", name="fnet" )
#myeff_m3 <- includeEffects(myeff_m3, egoX, interaction1 = "vrouw", name="fnet" )
#myeff_m3 <- includeEffects(myeff_m3, altX, interaction1 = "vrouw", name="fnet" )

myeff_m3 <- includeEffects(myeff_m3, sameX, interaction1 = "vrouw", name="rtnet" )
myeff_m3 <- includeEffects(myeff_m3, egoX, interaction1 = "vrouw", name="rtnet" )
#myeff_m3 <- includeEffects(myeff_m3, altX, interaction1 = "vrouw", name="rtnet" )

myeff_m3 <- includeEffects(myeff_m3, sameX, interaction1 = "vrouw", name="atmnet" )
myeff_m3 <- includeEffects(myeff_m3, egoX, interaction1 = "vrouw", name="atmnet" )
#myeff_m3 <- includeEffects(myeff_m3, altX, interaction1 = "vrouw", name="atmnet" )

myeff_m3 <- includeEffects(myeff_m3, absDiffX , interaction1 = "lft", name="fnet" )
myeff_m3 <- includeEffects(myeff_m3, egoX, interaction1 = "lft", name="fnet" )
#myeff_m3 <- includeEffects(myeff_m3, altX, interaction1 = "lft", name="fnet" )

myeff_m3 <- includeEffects(myeff_m3, absDiffX , interaction1 = "lft", name="rtnet" )
myeff_m3 <- includeEffects(myeff_m3, egoX, interaction1 = "lft", name="rtnet" )
myeff_m3 <- includeEffects(myeff_m3, altX, interaction1 = "lft", name="rtnet" )

myeff_m3 <- includeEffects(myeff_m3, absDiffX , interaction1 = "lft", name="atmnet" )
#myeff_m3 <- includeEffects(myeff_m3, egoX, interaction1 = "lft", name="atmnet" )
#myeff_m3 <- includeEffects(myeff_m3, altX, interaction1 = "lft", name="atmnet" )

myeff_m3 <- includeEffects(myeff_m3, sameX, interaction1 = "ethminz", name="fnet" )
#myeff_m3 <- includeEffects(myeff_m3, egoX, interaction1 = "ethminz", name="fnet" )
#myeff_m3 <- includeEffects(myeff_m3, altX, interaction1 = "ethminz", name="fnet" )

myeff_m3 <- includeEffects(myeff_m3, sameX, interaction1 = "ethminz", name="rtnet" )
#myeff_m3 <- includeEffects(myeff_m3, egoX, interaction1 = "ethminz", name="rtnet" )
#myeff_m3 <- includeEffects(myeff_m3, altX, interaction1 = "ethminz", name="rtnet" )

myeff_m3 <- includeEffects(myeff_m3, sameX, interaction1 = "ethminz", name="atmnet" )
#myeff_m3 <- includeEffects(myeff_m3, egoX, interaction1 = "ethminz", name="atmnet" )
#myeff_m3 <- includeEffects(myeff_m3, altX, interaction1 = "ethminz", name="atmnet" )

myeff_m3
save(myeff_m3, file="myeff_m3.Rdata")
}

#estimate parameters
{
myalgorithm <- sienaAlgorithmCreate( projname = 'test')
load("myeff_m3.Rdata")
load("ansM2g.Rdata")
ansM3 <- siena07( myalgorithm, data = mydata, prevAns=ansM2g, effects = myeff_m3, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM3
ansM3b <- siena07( myalgorithm, data = mydata, prevAns=ansM3, effects = myeff_m3, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM3b
ansM3c <- siena07( myalgorithm, data = mydata, prevAns=ansM3b, effects = myeff_m3, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM3c
ansM3d <- siena07( myalgorithm, data = mydata, prevAns=ansM3c, effects = myeff_m3, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM3d
save(ansM3c, file="ansM3c.Rdata")
}

###model 4: define myeff4 include additional controls
{
load("myeff_m3.Rdata")

myeff4 <- myeff_m3

#"kamerlid2016" "kabinet2016"  "pleklijst"    "pleklijst1"

myeff4 <- includeEffects( myeff4, interaction1="kamerlid2016", altX, name="fnet" )
myeff4 <- includeEffects( myeff4, interaction1="kamerlid2016", altX, name="atmnet" )
myeff4 <- includeEffects( myeff4, interaction1="kamerlid2016", altX, name="rtnet" )

myeff4 <- includeEffects( myeff4, interaction1="kamerlid2016", egoX, name="fnet" )
myeff4 <- includeEffects( myeff4, interaction1="kamerlid2016", egoX, name="atmnet" )
myeff4 <- includeEffects( myeff4, interaction1="kamerlid2016", egoX, name="rtnet" )

myeff4 <- includeEffects( myeff4, interaction1="kabinet2016", altX, name="fnet" )
myeff4 <- includeEffects( myeff4, interaction1="kabinet2016", altX, name="atmnet" )
myeff4 <- includeEffects( myeff4, interaction1="kabinet2016", altX, name="rtnet" )

myeff4 <- includeEffects( myeff4, interaction1="kabinet2016", egoX, name="fnet" )
myeff4 <- includeEffects( myeff4, interaction1="kabinet2016", egoX, name="atmnet" )
myeff4 <- includeEffects( myeff4, interaction1="kabinet2016", egoX, name="rtnet" )

myeff4 <- includeEffects( myeff4, interaction1="pleklijst1", altX, name="fnet" )
myeff4 <- includeEffects( myeff4, interaction1="pleklijst1", altX, name="atmnet" )
myeff4 <- includeEffects( myeff4, interaction1="pleklijst1", altX, name="rtnet" )

myeff4 <- includeEffects( myeff4, interaction1="pleklijst1", egoX, name="fnet" )
myeff4 <- includeEffects( myeff4, interaction1="pleklijst1", egoX, name="atmnet" )
myeff4 <- includeEffects( myeff4, interaction1="pleklijst1", egoX, name="rtnet" )

myeff4 <- includeEffects( myeff4, interaction1="pleklijst", diffX, name="fnet" )
myeff4 <- includeEffects( myeff4, interaction1="pleklijst", diffX, name="atmnet" )
myeff4 <- includeEffects( myeff4, interaction1="pleklijst", diffX, name="rtnet" )

myeff4
save(myeff4, file="myeff4.Rdata")
}

# estimate parameters
{

myalgorithm <- sienaAlgorithmCreate( projname = 'test')
load("myeff4.Rdata")
load("ansM3c.Rdata")
ansM4 <- siena07( myalgorithm, data = mydata, prevAns=ansM3c, effects = myeff4, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM4
ansM4b <- siena07( myalgorithm, data = mydata, prevAns=ansM4, effects = myeff4, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM4b
ansM4c <- siena07( myalgorithm, data = mydata, prevAns=ansM4b, effects = myeff4, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM4c
ansM4d <- siena07( myalgorithm, data = mydata, prevAns=ansM4c, effects = myeff4, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM4d
save(ansM4d, file="ansM4d.Rdata")
}

#update myeff4 #remove covariate variables with t-value <1
{
myeff4 <- includeEffects( myeff4, interaction1="pvv", altX, include=FALSE, name="fnet" )
myeff4 <- includeEffects( myeff4, interaction1="pvv", egoX, include=FALSE, name="fnet" )
myeff4 <- includeEffects(myeff4, absDiffX , interaction1 = "lft" , include=FALSE, name="atmnet" )
myeff4 <- includeEffects( myeff4, interaction1="kamerlid2016", altX, include=FALSE, name="atmnet" )
myeff4 <- includeEffects( myeff4, interaction1="pleklijst", diffX, include=FALSE, name="atmnet" )
myeff4 <- includeEffects(myeff4, sameX, interaction1 = "ethminz", include=FALSE, name="rtnet" )
myeff4 <- includeEffects( myeff4, interaction1="lft", egoX, include=FALSE, name="rtnet" )
myeff4 <- includeEffects(myeff4, absDiffX , interaction1 = "lft", include=FALSE, name="rtnet" )
myeff4
myeff4b <- myeff4
save(myeff4b, file="myeff4b.Rdata")
}

# estimate parameters
{
myalgorithm <- sienaAlgorithmCreate( projname = 'test')
load("myeff4b.Rdata")
load("ansM4d.Rdata")
ansM4e <- siena07( myalgorithm, data = mydata, prevAns=ansM4d, effects = myeff4b, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM4e
ansM4f <- siena07( myalgorithm, data = mydata, prevAns=ansM4e, effects = myeff4b, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM4f
ansM4g <- siena07( myalgorithm, data = mydata, prevAns=ansM4f, effects = myeff4b, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM4g
ansM4h <- siena07( myalgorithm, data = mydata, prevAns=ansM4g, effects = myeff4b, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM4h
save(ansM4h, file="ansM4h.Rdata")
}
#ansm4e is best!
save(ansM4e, file="ansM4e.Rdata")
#maar f heeft een t voor otudegree atmnet
save(ansM4f, file="ansM4f.Rdata")
#iets fout gedaan maar wel beter model
save(ansM5, file="ansM4i.Rdata")
ansM4i <- ansM5
ansM4i
###model 5: crossnetwork effects
{
load("myeff4b.Rdata")
myeff5 <- myeff4b
#ik doe het een, ik doe het ander
myeff5 <- includeEffects( myeff5, crprod, interaction1="fnet", name="rtnet" )
myeff5 <- includeEffects( myeff5, crprod, interaction1="fnet", name="atmnet" )
myeff5 <- includeEffects( myeff5, crprod, interaction1="rtnet", name="fnet" )
myeff5 <- includeEffects( myeff5, crprod, interaction1="rtnet", name="atmnet" )
myeff5 <- includeEffects( myeff5, crprod, interaction1="atmnet", name="fnet" )
myeff5 <- includeEffects( myeff5, crprod, interaction1="atmnet", name="rtnet" )

#ik doe het een, jij doet het ander terug
myeff5 <- includeEffects( myeff5, crprodRecip, interaction1="fnet", name="rtnet" )
myeff5 <- includeEffects( myeff5, crprodRecip, interaction1="fnet", name="atmnet" )
myeff5 <- includeEffects( myeff5, crprodRecip, interaction1="rtnet", name="fnet" )
myeff5 <- includeEffects( myeff5, crprodRecip, interaction1="rtnet", name="atmnet" )
myeff5 <- includeEffects( myeff5, crprodRecip, interaction1="atmnet", name="fnet" )
myeff5 <- includeEffects( myeff5, crprodRecip, interaction1="atmnet", name="rtnet" )
myeff5

save(myeff5, file="myeff5.Rdata")
}

# model 5 estimate parameters
{
myalgorithm <- sienaAlgorithmCreate( projname = 'test')
load("myeff5.Rdata")
load("ansM4i.Rdata")
ansM5 <- siena07( myalgorithm, data = mydata, prevAns=ansM4f, effects = myeff5, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM5
save(ansM5, file="ansM5.Rdata")
ansM5b <- siena07( myalgorithm, data = mydata, prevAns=ansM5, effects = myeff5, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM5b
save(ansM5b, file="ansM5b.Rdata")
ansM5c <- siena07( myalgorithm, data = mydata, prevAns=ansM5b, effects = myeff5, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM5c
save(ansM5c, file="ansM5c.Rdata")
ansM5d <- siena07( myalgorithm, data = mydata, prevAns=ansM5c, effects = myeff5, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM5d
save(ansM5d, file="ansM5d.Rdata")

ansM5e <- siena07( myalgorithm, data = mydata, prevAns=ansM5d, effects = myeff5, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM5e
save(ansM5e, file="ansM5e.Rdata")
ansM5f <- siena07( myalgorithm, data = mydata, prevAns=ansM5e, effects = myeff5, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM5f
save(ansM5f, file="ansM5f.Rdata")
ansM5g <- siena07( myalgorithm, data = mydata, prevAns=ansM5f, effects = myeff5, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM5g
save(ansM5g, file="ansM5g.Rdata")

}

#update parameters
{
load("myeff5.Rdata")
myeff5b <- myeff5
#ik doe het een, ik doe het ander
myeff5b <- includeEffects( myeff5b, crprod, interaction1="atmnet", name="fnet" , include=FALSE)

#ik doe het een, jij doet het ander terug
myeff5b <- includeEffects( myeff5b, crprodRecip, interaction1="atmnet", name="fnet" , include=FALSE)

myeff5b

save(myeff5b, file="myeff5b.Rdata")
}


# reestimate model 5 estimate parameters
{
myalgorithm <- sienaAlgorithmCreate( projname = 'test')
load("myeff5b.Rdata")
load("ansM5e.Rdata")
ansM5h <- siena07( myalgorithm, data = mydata, prevAns=ansM5e, effects = myeff5b, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM5h
save(ansM5h, file="ansM5h.Rdata")
ansM5i <- siena07( myalgorithm, data = mydata, prevAns=ansM5h, effects = myeff5b, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM5i
save(ansM5i, file="ansM5i.Rdata")
ansM5j <- siena07( myalgorithm, data = mydata, prevAns=ansM5i, effects = myeff5b, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM5j
save(ansM5j, file="ansM5j.Rdata")
ansM5k <- siena07( myalgorithm, data = mydata, prevAns=ansM5j, effects = myeff5b, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM5k
save(ansM5k, file="ansM5k.Rdata")


}


###model 6 : time heterogeneity

{
tt <- sienaTimeTest(ansM5i)
#plot(tt, effects=1:70)
tt$EffectTest
load("myeff5b.Rdata")
myeff6 <- myeff5b
myeff6 <- includeTimeDummy(myeff6, sameX, interaction1 = "partij", name="atmnet", timeDummy="2")
myeff6 <- includeTimeDummy(myeff6, sameX, interaction1 = "partij", name="rtnet", timeDummy="2")
myeff6
save(myeff6, file="myeff6.Rdata")
}

# estimate parameters Model 6
{
#ansM5i best
myalgorithm <- sienaAlgorithmCreate( projname = 'test')
load("myeff6.Rdata")
load("ansM5i.Rdata")
ansM6 <- siena07( myalgorithm, data = mydata, prevAns=ansM5i, effects = myeff6, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM6
save(ansM6, file="ansM6.Rdata")
ansM6b <- siena07( myalgorithm, data = mydata, prevAns=ansM6, effects = myeff6, useCluster=TRUE, nbrNodes=4, initC=TRUE, batch=TRUE)
ansM6b
save(ansM6b, file="ansM6.Rdata")

}

#prepare output table
load("ansM1c.Rdata")
load("ansM2g.Rdata")
load("ansM3c.Rdata")
load("ansM4i.Rdata")
ansM4i <- ansM5
load("ansM5i.Rdata")
load("ansM6.Rdata")
ansM6

fanscsv <- function(ans=ans, filename="ans.csv") {
	ans1_mat <- matrix(NA, nrow=length(ans$effects[,2]),ncol=3)
	row.names(ans1_mat) <- ans$effects[,2]
	ans1_mat[,1] <- (ans$theta)
	ans1_mat[,2] <- (ans$se)
	ans1_mat[,3] <- (ans$tconv)
	ans1_mat <- rbind(ans1_mat, c(ans$tconv.max))
	row.names(ans1_mat)[length(row.names(ans1_mat))] <- "Overall maximum convergence ratio:"
	colnames(ans1_mat) <- c("Estimate", "Standard Error", "Convergence t-ratio")
	print(ans1_mat)
	write.csv(ans1_mat, filename)
}

fanscsv(ansM2g, "ansM2.csv")

ans1_mat
?write.csv

ans1_mat[length(ansM1c$effects[,2])+1,1] <- ansM1c$tconv.max

ans1_mat

colnames(ans1_mat) <- c("effect name", "Estimate", "Standard Error", "Convergence t-ratio")
ans1_mat
ansM1c

colnames(ans1_mat) <- c("effect name", "Estimate", "Standard Error", "Convergence t-ratio")

ansM1c$theta
ansM1c$effects[,2]
ansM1c$se
