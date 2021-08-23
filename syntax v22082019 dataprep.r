###dataprep###
###version 22-08-2019###
###J Tolsma###
###last data update 29-08-2019


#always start with cleaning up your workspace if you want to run your script again
rm (list = ls( )) 

#intall additional packages you will need later. 
require(foreign)
library(RSiena) 

#additional functions
# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

#setwd
setwd("C:\\Users\\Administrator\\Documents\\Shared\\Twitter")

#STAP 1: read in data
#HET FEITELIJKE NETWERK (netwerken)

#some terminology I always forget
#ego follows alter
#ego is thus sender, alter is receiver
#ego is follower, alter is followee/friend

###friends###
#politician_name / EGOhandle: EGO
#screen_name / name: ALTER (no ALTERhandle yet)
#friends t1
mpnet <- read.spss('Ties data\\friends 20170405.sav', use.value.labels=F, to.data.frame=T)
#names(mpnet)
#mpnet_t1 <- mpnet[,c(1,2,5,7)]
mpnet_t1 <- mpnet
mpnet_t1$sn <- trim.trailing(as.character(mpnet_t1$screen_name))
#friends t2
mpnet2 <- read.spss('Ties data\\friends 20170629.sav', use.value.labels=F, to.data.frame=T)
#mpnet_t2 <- mpnet2[,c(1,2,5,7)]
mpnet_t2 <- mpnet2
mpnet_t2$sn <- trim.trailing(as.character(mpnet_t2$screen_name))
#friends t3
mpnet3 <- read.spss('Ties data\\friends 20170922.sav', use.value.labels=F, to.data.frame=T)
#mpnet_t3 <- mpnet3[,c(1,2,5,7)]
mpnet_t3 <- mpnet3
mpnet_t3$sn <- trim.trailing(as.character(mpnet_t3$screen_name))

###atmentions: 26-06-2018 LET OP EGO EN ALTER ZIJN OMGEDRAAID!! 
###DUS UITEINDELIJK DE TRANSPOSE NEMEN VAN NOMINATIE-NETWERK!!!
###DIT SPEELT ALLEEN IN atMENTION-NETWERK
#politician_name / EGOhandle: EGO
#atmentioner_screen_name / name: ALTER (no ALTERhandle yet)
#atmentions t1
mpnetatm_t1 <- read.spss('Ties data\\atmentions 20170405.sav', use.value.labels=F, to.data.frame=T)
#names(mpnetatm_t1)
mpnetatm_t1$sn <- trim.trailing(as.character(mpnetatm_t1$atmentioner_screen_name))
#atmentions t2
mpnetatm_t2 <- read.spss('Ties data\\atmentions 20170629.sav', use.value.labels=F, to.data.frame=T)
mpnetatm_t2$sn <- trim.trailing(as.character(mpnetatm_t2$atmentioner_screen_name))
#atmentions t3
mpnetatm_t3 <- read.spss('Ties data\\atmentions 20170922.sav', use.value.labels=F, to.data.frame=T)
mpnetatm_t3$sn <- trim.trailing(as.character(mpnetatm_t3$atmentioner_screen_name))

###retweets
#politician_name / EGOhandle: EGO
#original_screen_name / name: ALTER (no ALTERhandle yet)
#retweets t1
mpnetrt_t1 <- read.spss('Ties data\\retweets 20170405.sav', use.value.labels=F, to.data.frame=T)
#names(mpnetrt_t1)
mpnetrt_t1$sn <- trim.trailing(as.character(mpnetrt_t1$original_screen_name))
#rtentions t2
mpnetrt_t2 <- read.spss('Ties data\\retweets 20170629.sav', use.value.labels=F, to.data.frame=T)
mpnetrt_t2$sn <- trim.trailing(as.character(mpnetrt_t2$original_screen_name))
#rtentions t3
mpnetrt_t3 <- read.spss('Ties data\\retweets 20170922.sav', use.value.labels=F, to.data.frame=T)
mpnetrt_t3$sn <- trim.trailing(as.character(mpnetrt_t3$original_screen_name))

#STAP 2 create ALTERhandle
#need to have info on combination screenname and ALTERhandle
key_old <- read.spss('Ego data\\key moederbestand 20170721.sav', use.value.labels=T, to.data.frame=T)
key <- read.spss('Ego data\\key moederbestand 20171114.sav', use.value.labels=T, to.data.frame=T)
names(key_old)
#head(key)
key2<-key[,c("Twitterhandle","EGOhandle","Naam")]
names(key2) <- c("screen_name" ,"EGOhandle"  , "name")
key2$ALTERhandle <- key2$EGOhandle
key2$sn<- trim.trailing(as.character(key2$screen_name))
key2$name<- trim.trailing(as.character(key2$name))
key2 <- key2[,c("EGOhandle","name", "ALTERhandle", "sn")]
sum(duplicated(key2$EGOhandle))
#0, dus das goed. 

#match ALTERhandle
mpinfo <- key2[,c("sn", "ALTERhandle")]

mpnet_t1 <- merge(mpnet_t1, mpinfo, by=c("sn"), incomparables="")
mpnet_t2 <- merge(mpnet_t2, mpinfo, by=c("sn"), incomparables="")
mpnet_t3 <- merge(mpnet_t3, mpinfo, by=c("sn"), incomparables="")

mpnetatm_t1 <- merge(mpnetatm_t1, mpinfo, by=c("sn"), incomparables="")
mpnetatm_t2 <- merge(mpnetatm_t2, mpinfo, by=c("sn"), incomparables="")
mpnetatm_t3 <- merge(mpnetatm_t3, mpinfo, by=c("sn"), incomparables="")

mpnetrt_t1 <- merge(mpnetrt_t1, mpinfo, by=c("sn"), incomparables="")
mpnetrt_t2 <- merge(mpnetrt_t2, mpinfo, by=c("sn"), incomparables="")
mpnetrt_t3 <- merge(mpnetrt_t3, mpinfo, by=c("sn"), incomparables="")


#STAP 3: select dyads in which both EGO and ALTER have been elected
key$ego_elected <- key$alter_elected <- key$Elected20170315 
key$ALTERhandle <- key$EGOhandle
#nu selectie op EGO en ALTER die zijn verkozen
mpnet_t1 <- merge(mpnet_t1, key[,c("EGOhandle", "ego_elected")], by=c("EGOhandle"))
mpnet_t1 <- merge(mpnet_t1, key[,c("ALTERhandle", "alter_elected")], by=c("ALTERhandle"))
mpnet_t1 <- mpnet_t1[as.numeric(mpnet_t1$ego_elected)==2,]
mpnet_t1 <- mpnet_t1[as.numeric(mpnet_t1$alter_elected)==2,]

mpnet_t2 <- merge(mpnet_t2, key[,c("EGOhandle", "ego_elected")], by=c("EGOhandle"))
mpnet_t2 <- merge(mpnet_t2, key[,c("ALTERhandle", "alter_elected")], by=c("ALTERhandle"))
mpnet_t2 <- mpnet_t2[as.numeric(mpnet_t2$ego_elected)==2,]
mpnet_t2 <- mpnet_t2[as.numeric(mpnet_t2$alter_elected)==2,]

mpnet_t3 <- merge(mpnet_t3, key[,c("EGOhandle", "ego_elected")], by=c("EGOhandle"))
mpnet_t3 <- merge(mpnet_t3, key[,c("ALTERhandle", "alter_elected")], by=c("ALTERhandle"))
mpnet_t3 <- mpnet_t3[as.numeric(mpnet_t3$ego_elected)==2,]
mpnet_t3 <- mpnet_t3[as.numeric(mpnet_t3$alter_elected)==2,]

mpnetatm_t1 <- merge(mpnetatm_t1, key[,c("EGOhandle", "ego_elected")], by=c("EGOhandle"))
mpnetatm_t1 <- merge(mpnetatm_t1, key[,c("ALTERhandle", "alter_elected")], by=c("ALTERhandle"))
mpnetatm_t1 <- mpnetatm_t1[as.numeric(mpnetatm_t1$ego_elected)==2,]
mpnetatm_t1 <- mpnetatm_t1[as.numeric(mpnetatm_t1$alter_elected)==2,]

mpnetatm_t2 <- merge(mpnetatm_t2, key[,c("EGOhandle", "ego_elected")], by=c("EGOhandle"))
mpnetatm_t2 <- merge(mpnetatm_t2, key[,c("ALTERhandle", "alter_elected")], by=c("ALTERhandle"))
mpnetatm_t2 <- mpnetatm_t2[as.numeric(mpnetatm_t2$ego_elected)==2,]
mpnetatm_t2 <- mpnetatm_t2[as.numeric(mpnetatm_t2$alter_elected)==2,]

mpnetatm_t3 <- merge(mpnetatm_t3, key[,c("EGOhandle", "ego_elected")], by=c("EGOhandle"))
mpnetatm_t3 <- merge(mpnetatm_t3, key[,c("ALTERhandle", "alter_elected")], by=c("ALTERhandle"))
mpnetatm_t3 <- mpnetatm_t3[as.numeric(mpnetatm_t3$ego_elected)==2,]
mpnetatm_t3 <- mpnetatm_t3[as.numeric(mpnetatm_t3$alter_elected)==2,]

mpnetrt_t1 <- merge(mpnetrt_t1, key[,c("EGOhandle", "ego_elected")], by=c("EGOhandle"))
mpnetrt_t1 <- merge(mpnetrt_t1, key[,c("ALTERhandle", "alter_elected")], by=c("ALTERhandle"))
mpnetrt_t1 <- mpnetrt_t1[as.numeric(mpnetrt_t1$ego_elected)==2,]
mpnetrt_t1 <- mpnetrt_t1[as.numeric(mpnetrt_t1$alter_elected)==2,]

mpnetrt_t2 <- merge(mpnetrt_t2, key[,c("EGOhandle", "ego_elected")], by=c("EGOhandle"))
mpnetrt_t2 <- merge(mpnetrt_t2, key[,c("ALTERhandle", "alter_elected")], by=c("ALTERhandle"))
mpnetrt_t2 <- mpnetrt_t2[as.numeric(mpnetrt_t2$ego_elected)==2,]
mpnetrt_t2 <- mpnetrt_t2[as.numeric(mpnetrt_t2$alter_elected)==2,]

mpnetrt_t3 <- merge(mpnetrt_t3, key[,c("EGOhandle", "ego_elected")], by=c("EGOhandle"))
mpnetrt_t3 <- merge(mpnetrt_t3, key[,c("ALTERhandle", "alter_elected")], by=c("ALTERhandle"))
mpnetrt_t3 <- mpnetrt_t3[as.numeric(mpnetrt_t3$ego_elected)==2,]
mpnetrt_t3 <- mpnetrt_t3[as.numeric(mpnetrt_t3$alter_elected)==2,]

#STAP 3: atmentions en retweets in juiste time window
#voor de atmentions en retweets moeten we nog selecteren op atmention binnen bepaalde tijd. 
#voor t3 is dat atmention na datum t2.
#voor t2 is dat atmention na datum t1.
#voor t1 is dat t1 - tijd tussen t2 en t1. 

t3 <- as.Date("2017-09-22")
t2 <- as.Date("2017-06-29")
t1 <- as.Date("2017-04-05")
dift3t2 <- t3 - t2
dift3t2
dift2t1 <- t2 - t1
dift2t1
t0 <- t1 - dift2t1
t0
#Time difference of 85 days

#construct data variables
mpnetatm_t1$date_last_atm <- as.Date(substr(as.character(mpnetatm_t1$last_atmention),1,10))
mpnetatm_t2$date_last_atm <- as.Date(substr(as.character(mpnetatm_t2$last_atmention),1,10))
mpnetatm_t3$date_last_atm <- as.Date(substr(as.character(mpnetatm_t3$last_atmention),1,10))

mpnetrt_t1$date_last_rt <- as.Date(substr(as.character(mpnetrt_t1$last_retweet),1,10))
mpnetrt_t2$date_last_rt <- as.Date(substr(as.character(mpnetrt_t2$last_retweet),1,10))
mpnetrt_t3$date_last_rt <- as.Date(substr(as.character(mpnetrt_t3$last_retweet),1,10))

#check selections
#sum(mpnetatm_t2$date_last_atm > t1)
#sum(mpnetatm_t2$date_last_atm > t1) / length(mpnetatm_t2$date_last_atm)
#sum(mpnetatm_t3$date_last_atm > t2)
#sum(mpnetatm_t3$date_last_atm > t2) / length(mpnetatm_t3$date_last_atm)
#mpnetatm_t2_copy <- mpnetatm_t2
#mpnetatm_t3_copy <- mpnetatm_t3

#run selection 
mpnetatm_t1 <- mpnetatm_t1[mpnetatm_t1$date_last_atm > t0,]
#mpnetatm_t2_mistake <- mpnetatm_t3[mpnetatm_t2$date_last_atm > t1,]
mpnetatm_t2 <- mpnetatm_t2[mpnetatm_t2$date_last_atm > t1,]
mpnetatm_t3 <- mpnetatm_t3[mpnetatm_t3$date_last_atm > t2,]

#mpnetatm_t2_mistake$testid <- mpnetatm_t2_mistake$EGOhandle * 1000000000 + mpnetatm_t2_mistake$ALTERhandle
#mpnetatm_t2$testid <- mpnetatm_t2$EGOhandle * 1000000000 + mpnetatm_t2$ALTERhandle
#mpnetatm_t3$testid <- mpnetatm_t3$EGOhandle * 1000000000 + mpnetatm_t3$ALTERhandle

#sum(mpnetatm_t2_mistake$testid %in% mpnetatm_t2$testid) / length(mpnetatm_t2_mistake$testid)
#[1] 0.3873457
#sum(mpnetatm_t2$testid %in% mpnetatm_t3$testid) / length(mpnetatm_t2$testid)
#[1] 0.2416999
#sum(mpnetatm_t3$testid %in% mpnetatm_t2$testid) / length(mpnetatm_t3$testid)
#[1] 0.6211604

mpnetrt_t1 <- mpnetrt_t1[mpnetrt_t1$date_last_rt > t0,]
#mpnetrt_t2_mistake <- mpnetrt_t3[mpnetrt_t2$date_last_rt > t1,]
mpnetrt_t2 <- mpnetrt_t2[mpnetrt_t2$date_last_rt > t1,]
mpnetrt_t3 <- mpnetrt_t3[mpnetrt_t3$date_last_rt > t2,]


####STAP 4: check ego's en alters
#we moeten aantal ego's en alters controleren. 
#ego kunnen ontbreken want niet iedereen heeft twitterhandle en theoretisch omdat ze geen andere kamerleden volgen.
#ego kunnen ontbreken omdat wij niet juiste twitterhandle hebben opgegeven aan Nik: dit betreft kamerlid Theo Hiddema (wel account opgeven, maar niet het juiste)/deze zijn achter niet te herstellen
#alters kunnen ontbreken want niet iedereen heeft twitterhandle en theoretisch omdat geen enkel kamerlid hen volgt.
#alters kunnen ontbreken omdat wij niet juiste twitterhandle hebben opgegeven aan Nik: dit betreft kamerlid Theo Hiddema (wel account opgeven, maar niet het juiste)/deze zijn achteraf nog te herstellen
#er kunnen dubbele inzitten omdat sommige kamerleden meerdere twitterhandles hebben. dit zien we door laatste cijfer van EGOhandle
#doel: geen dubbele. 
#doel: indien TK geen twitterhandle dan ook niet in data. (dit worden later joiners in Rsiena terminologie)
#doel: indien TK wel twitterhandle, maar niet volgt of wordt gevolgd dan wel in data.  

EGOhandles <- (unique(c(
mpnet_t1$EGOhandle,
mpnet_t2$EGOhandle, 
mpnet_t3$EGOhandle,
mpnet_t1$ALTERhandle,
mpnet_t2$ALTERhandle, 
mpnet_t3$ALTERhandle,
mpnetatm_t1$EGOhandle,
mpnetatm_t2$EGOhandle, 
mpnetatm_t3$EGOhandle,
mpnetatm_t1$ALTERhandle,
mpnetatm_t2$ALTERhandle, 
mpnetatm_t3$ALTERhandle,
mpnetrt_t1$EGOhandle,
mpnetrt_t2$EGOhandle, 
mpnetrt_t3$EGOhandle,
mpnetrt_t1$ALTERhandle,
mpnetrt_t2$ALTERhandle, 
mpnetrt_t3$ALTERhandle
 )))
EGOhandles <- EGOhandles[order(EGOhandles)]

EGOhandles
table(trunc(EGOhandles/10))[table(trunc(EGOhandles/10))>1]
#1040 (Raymond De Roon, EGOhandle 10402 meenemen, hiermee zijn 10401 en 10403 fout) 
#1310 (Barbara Visser, EGOhandle 13101 meenemen, hiermee is 13102 fout (pas geldig na oktober))
#1510 (Mark Rutte/minister president) 15101 meenemen)
#10610 (Theo Hiddema) 106102 meenemen LET OP, EGO IS MISSING (DATA NIET OPGEHAALD) ALTER INFO WEL BEKEND
#dus voor 106102 structural zeros voor ego!!

wronghandles <- c(10401, 10403, 13102, 15102, 106101)

#haal verkeerder dyades uit bestanden, dus zowel ego als als Alter mag niet in wronghandles zitten
mpnet_t1 <- mpnet_t1[!(mpnet_t1$EGOhandle %in% wronghandles | mpnet_t1$ALTERhandle %in% wronghandles),]
mpnet_t2 <- mpnet_t2[!(mpnet_t2$EGOhandle %in% wronghandles | mpnet_t2$ALTERhandle %in% wronghandles),]
mpnet_t3 <- mpnet_t3[!(mpnet_t3$EGOhandle %in% wronghandles | mpnet_t3$ALTERhandle %in% wronghandles),]

mpnetatm_t1 <- mpnetatm_t1[!(mpnetatm_t1$EGOhandle %in% wronghandles | mpnetatm_t1$ALTERhandle %in% wronghandles),]
mpnetatm_t2 <- mpnetatm_t2[!(mpnetatm_t2$EGOhandle %in% wronghandles | mpnetatm_t2$ALTERhandle %in% wronghandles),]
mpnetatm_t3 <- mpnetatm_t3[!(mpnetatm_t3$EGOhandle %in% wronghandles | mpnetatm_t3$ALTERhandle %in% wronghandles),]

mpnetrt_t1 <- mpnetrt_t1[!(mpnetrt_t1$EGOhandle %in% wronghandles | mpnetrt_t1$ALTERhandle %in% wronghandles),]
mpnetrt_t2 <- mpnetrt_t2[!(mpnetrt_t2$EGOhandle %in% wronghandles | mpnetrt_t2$ALTERhandle %in% wronghandles),]
mpnetrt_t3 <- mpnetrt_t3[!(mpnetrt_t3$EGOhandle %in% wronghandles | mpnetrt_t3$ALTERhandle %in% wronghandles),]

EGOhandles <- EGOhandles[!(EGOhandles %in% wronghandles)]
EGOhandles
length(EGOhandles)
#147

#welke ontbreken
ego <- as.data.frame(EGOhandles)
names(ego) <- "EGOhandle"
ego$present <- 1 
egomissing <- merge(ego, key, by=c("EGOhandle"), all=TRUE)
#wie ontbreekt: wel gekozen, maar geen geldige score op present
egomissing <- egomissing[(is.na(egomissing$present) & as.numeric(egomissing$ego_elected)==2),]
#haal de ego's er uit met dubbele accounts: 
egomissing <- egomissing[!(egomissing$EGOhandle %in% wronghandles),]
egomissing$EGOhandle
# [1]  2600  3800 41400 58000
egomissing$Naam
#[1] Van Dijck, Tony                         
#[2] Fritsma, Sietse                         
#[3] Chris van Dam     ####let op zit met 41401 in data.                       
#[4] Albert van den Bosch 
#van deze 4 hebben we geen account. 

#wie is aanwezig
egopresent <- merge(ego, key, by=c("EGOhandle"), all=TRUE)
egopresent
egopresent <- egopresent[(!is.na(egopresent$present) & as.numeric(egopresent$ego_elected)==2),]
egopresent
#haal de ego's er uit met dubbele accounts: 
egopresent <- egopresent[!(egomissing$EGOhandle %in% wronghandles),]
egopresent$EGOhandle
egopresent$Naam
#hoe kan Chris van Dam er nu op eens in zitten???
egopresent[104,]

###final check if we have no dyad duplicates in networks, we do in friendship and rtweet. need to check talk to NIELS/NIK!!
sum(duplicated(mpnet_t1[,c("EGOhandle", "ALTERhandle")]))
head(duplicated(mpnet_t1[,c("EGOhandle", "ALTERhandle")]))

sum(!duplicated(mpnet_t1[,c("EGOhandle", "ALTERhandle")]))
mpnetrt_t1[duplicated(mpnetrt_t1[,c("EGOhandle", "ALTERhandle")]),]
sum(!duplicated(mpnet_t3[,c("EGOhandle", "ALTERhandle")]))
sum(duplicated(mpnet_t2[,c("EGOhandle", "ALTERhandle")]))
sum(duplicated(mpnet_t3[,c("EGOhandle", "ALTERhandle")]))

sum(duplicated(mpnetatm_t1[,c("EGOhandle", "ALTERhandle")]))
sum(duplicated(mpnetatm_t2[,c("EGOhandle", "ALTERhandle")]))
sum(duplicated(mpnetatm_t3[,c("EGOhandle", "ALTERhandle")]))

sum(duplicated(mpnetrt_t1[,c("EGOhandle", "ALTERhandle")]))
sum(duplicated(mpnetrt_t2[,c("EGOhandle", "ALTERhandle")]))
sum(duplicated(mpnetrt_t3[,c("EGOhandle", "ALTERhandle")]))

#save the duplicates, for inspection later. 
mpnet_t1_duplicated <- mpnet_t1[duplicated(cbind(mpnet_t1$EGOhandle, mpnet_t1$ALTERhandle)),]
mpnet_t2_duplicated <- mpnet_t2[duplicated(cbind(mpnet_t2$EGOhandle, mpnet_t2$ALTERhandle)),]
mpnetrt_t1_duplicated <- mpnetrt_t1[duplicated(cbind(mpnetrt_t1$EGOhandle, mpnetrt_t1$ALTERhandle)),]

mpnet_t1_duplicated
mpnet_t2_duplicated 
mpnetrt_t1_duplicated
#only ego's Ozturk, Selcuk (9601) and van Kent, Bart (31501)

duplicates_friends_t1 <- mpnet_t1[mpnet_t1$EGOhandle==9601 | mpnet_t1$EGOhandle==31501,]
duplicates_friends_t2 <- mpnet_t2[mpnet_t2$EGOhandle==9601 | mpnet_t2$EGOhandle==31501,]
duplicates_rt_t1 <- mpnetrt_t1[mpnetrt_t1$EGOhandle==9601 | mpnetrt_t1$EGOhandle==31501,]

#write.csv2(duplicates_friends_t1, "duplicates\\duplicates_friends_t1.csv")
#write.csv2(duplicates_friends_t2, "duplicates\\duplicates_friends_t2.csv")
#write.csv2(duplicates_rt_t1, "duplicates\\duplicates_rt_t1.csv")

#remove duplicates, simply take first?? looking at friendship nominations it seems that not all duplicates are the same. 
mpnet_t1 <- mpnet_t1[!duplicated(cbind(mpnet_t1$EGOhandle, mpnet_t1$ALTERhandle)),]
mpnet_t2 <- mpnet_t2[!duplicated(cbind(mpnet_t2$EGOhandle, mpnet_t2$ALTERhandle)),]
mpnet_t3 <- mpnet_t3[!duplicated(cbind(mpnet_t3$EGOhandle, mpnet_t3$ALTERhandle)),]

mpnetatm_t1 <- mpnetatm_t1[!duplicated(cbind(mpnetatm_t1$EGOhandle, mpnetatm_t1$ALTERhandle)),]
mpnetatm_t2 <- mpnetatm_t2[!duplicated(cbind(mpnetatm_t2$EGOhandle, mpnetatm_t2$ALTERhandle)),]
mpnetatm_t3 <- mpnetatm_t3[!duplicated(cbind(mpnetatm_t3$EGOhandle, mpnetatm_t3$ALTERhandle)),]

mpnetrt_t1 <- mpnetrt_t1[!duplicated(cbind(mpnetrt_t1$EGOhandle, mpnetrt_t1$ALTERhandle)),]
mpnetrt_t2 <- mpnetrt_t2[!duplicated(cbind(mpnetrt_t2$EGOhandle, mpnetrt_t2$ALTERhandle)),]
mpnetrt_t3 <- mpnetrt_t3[!duplicated(cbind(mpnetrt_t3$EGOhandle, mpnetrt_t3$ALTERhandle)),]


####STAP 5 bestand met ego data in orde maken###
#let op dit is laatste bestand. als het goed is heeft Niels de 'fouten' er uit gehaald. 
#1040 (Raymond De Roon, EGOhandle 10402 meenemen, hiermee zijn 10401 en 10403 fout) 
#1510 (Mark Rutte/minister president) 15101 meenemen)
#10610 (Theo Hiddema) 106102 meenemen LET OP, EGO IS MISSING (DATA NIET OPGEHAALD) ALTER INFO WEL BEKEND

key <- read.spss('Ego data\\key moederbestand 20171114.sav', use.value.labels=T, to.data.frame=T)
names(key)
key$ego_elected <- key$alter_elected <- key$Elected20170315 
key$ALTERhandle <- key$EGOhandle
#alleen de nodige meenemen
key <- key[key$EGOhandle %in% EGOhandles,]
length(key$EGOhandle)
key[key$EGOhandle==41401,]
#yes

#ego's are rows
#need to attach rankids instead of original ids. 
rankids <- data.frame(cbind(EGOhandles, rank(EGOhandles)))
names(rankids) <- c("EGOhandle", "EGOid")
rankids$ALTERhandle <- rankids$EGOhandle
rankids$ALTERid <- rankids$EGOid
rankids
key <- key[order(key$EGOhandle),]
head(key)

#STAP 6: RSiena oject maken# 
#nominatie-netwerk maken. ook nullen toevoegen. 
ids <- EGOhandles
EGOhandle <- rep(ids, each=length(ids))
ALTERhandle <- rep(ids,length(ids))

#arclist
rtnet1 <- rtnet2 <- rtnet3 <- atmnet1 <- atmnet2 <- atmnet3 <- fnet1 <- fnet2 <- fnet3 <- cbind(EGOhandle, ALTERhandle)
mpnetrt_t1$tie <- mpnetrt_t2$tie <- mpnetrt_t3$tie <- mpnetatm_t1$tie <- mpnetatm_t2$tie <- mpnetatm_t3$tie <- mpnet_t1$tie <- mpnet_t2$tie <- mpnet_t3$tie <- 1

fnet1 <- merge(fnet1, mpnet_t1[,c("EGOhandle", "ALTERhandle", "tie")], all.x=TRUE)
fnet1$tie[is.na(fnet1$tie)] <- 0
fnet2 <- merge(fnet2, mpnet_t2[,c("EGOhandle", "ALTERhandle", "tie")], all.x=TRUE)
fnet2$tie[is.na(fnet2$tie)] <- 0
fnet3 <- merge(fnet3, mpnet_t3[,c("EGOhandle", "ALTERhandle", "tie")], all.x=TRUE)
fnet3$tie[is.na(fnet3$tie)] <- 0

atmnet1 <- merge(atmnet1, mpnetatm_t1[,c("EGOhandle", "ALTERhandle", "tie")], all.x=TRUE)
atmnet1$tie[is.na(atmnet1$tie)] <- 0
atmnet2 <- merge(atmnet2, mpnetatm_t2[,c("EGOhandle", "ALTERhandle", "tie")], all.x=TRUE)
atmnet2$tie[is.na(atmnet2$tie)] <- 0
atmnet3 <- merge(atmnet3, mpnetatm_t3[,c("EGOhandle", "ALTERhandle", "tie")], all.x=TRUE)
atmnet3$tie[is.na(atmnet3$tie)] <- 0

rtnet1 <- merge(rtnet1, mpnetrt_t1[,c("EGOhandle", "ALTERhandle", "tie")], all.x=TRUE)
rtnet1$tie[is.na(rtnet1$tie)] <- 0
rtnet2 <- merge(rtnet2, mpnetrt_t2[,c("EGOhandle", "ALTERhandle", "tie")], all.x=TRUE)
rtnet2$tie[is.na(rtnet2$tie)] <- 0
rtnet3 <- merge(rtnet3, mpnetrt_t3[,c("EGOhandle", "ALTERhandle", "tie")], all.x=TRUE)
rtnet3$tie[is.na(rtnet3$tie)] <- 0

#ego's are rows
#need to attach rankids instead of original ids. 
names(rankids)
fnet1 <- merge(fnet1, rankids[,c("EGOhandle", "EGOid")])
fnet1 <- merge(fnet1, rankids[,c("ALTERhandle", "ALTERid")])
fnet1 <- fnet1[order(fnet1$EGOhandle, fnet1$ALTERhandle), ]
fnet2 <- merge(fnet2, rankids[,c("EGOhandle", "EGOid")])
fnet2 <- merge(fnet2, rankids[,c("ALTERhandle", "ALTERid")])
fnet2 <- fnet2[order(fnet2$EGOhandle, fnet2$ALTERhandle), ]
fnet3 <- merge(fnet3, rankids[,c("EGOhandle", "EGOid")])
fnet3 <- merge(fnet3, rankids[,c("ALTERhandle", "ALTERid")])
fnet3 <- fnet3[order(fnet3$EGOhandle, fnet3$ALTERhandle), ]

atmnet1 <- merge(atmnet1, rankids[,c("EGOhandle", "EGOid")])
atmnet1 <- merge(atmnet1, rankids[,c("ALTERhandle", "ALTERid")])
atmnet1 <- atmnet1[order(atmnet1$EGOhandle, atmnet1$ALTERhandle), ]
atmnet2 <- merge(atmnet2, rankids[,c("EGOhandle", "EGOid")])
atmnet2 <- merge(atmnet2, rankids[,c("ALTERhandle", "ALTERid")])
atmnet2 <- atmnet2[order(atmnet2$EGOhandle, atmnet2$ALTERhandle), ]
atmnet3 <- merge(atmnet3, rankids[,c("EGOhandle", "EGOid")])
atmnet3 <- merge(atmnet3, rankids[,c("ALTERhandle", "ALTERid")])
atmnet3 <- atmnet3[order(atmnet3$EGOhandle, atmnet3$ALTERhandle), ]

rtnet1 <- merge(rtnet1, rankids[,c("EGOhandle", "EGOid")])
rtnet1 <- merge(rtnet1, rankids[,c("ALTERhandle", "ALTERid")])
rtnet1 <- rtnet1[order(rtnet1$EGOhandle, rtnet1$ALTERhandle), ]
rtnet2 <- merge(rtnet2, rankids[,c("EGOhandle", "EGOid")])
rtnet2 <- merge(rtnet2, rankids[,c("ALTERhandle", "ALTERid")])
rtnet2 <- rtnet2[order(rtnet2$EGOhandle, rtnet2$ALTERhandle), ]
rtnet3 <- merge(rtnet3, rankids[,c("EGOhandle", "EGOid")])
rtnet3 <- merge(rtnet3, rankids[,c("ALTERhandle", "ALTERid")])
rtnet3 <- rtnet3[order(rtnet3$EGOhandle, rtnet3$ALTERhandle), ]

### For transforming an arclist into a matrix
fnet1 <- as.matrix(fnet1)
fnet2 <- as.matrix(fnet2)
fnet3 <- as.matrix(fnet3)

atmnet1 <- as.matrix(atmnet1)
atmnet2 <- as.matrix(atmnet2)
atmnet3 <- as.matrix(atmnet3)

rtnet1 <- as.matrix(rtnet1)
rtnet2 <- as.matrix(rtnet2)
rtnet3 <- as.matrix(rtnet3)

# create empty adjacency matrix
fnet1_mat <- fnet2_mat <- fnet3_mat <- matrix(0, length(EGOhandles), length(EGOhandles))
atmnet1_mat <- atmnet2_mat <- atmnet3_mat <- matrix(0, length(EGOhandles), length(EGOhandles))
rtnet1_mat <- rtnet2_mat <- rtnet3_mat <- matrix(0, length(EGOhandles), length(EGOhandles))

head(atmnet3)
head(fnet1)
# put edge values in desired places
fnet1_mat[fnet1[, 4:5]] <- fnet1[, 3]
fnet2_mat[fnet2[, 4:5]] <- fnet2[, 3]
fnet3_mat[fnet3[, 4:5]] <- fnet3[, 3]
#insert structural zeros for EGOhandle 106102 (only EGO, t1/t2) & 41401 (only ego, t1/t2)
id <- rankids$EGOid[rankids$EGOhandle==106102]
sum(fnet1_mat[id,])
sum(fnet2_mat[id,])
sum(fnet3_mat[id,])
fnet1_mat[id,] <- 10
fnet2_mat[id,] <- 10	
id <- rankids$EGOid[rankids$EGOhandle==41401]
sum(fnet3_mat[,id])
fnet1_mat[id,] <- 10
fnet2_mat[id,] <- 10
fnet <- sienaNet(array( c( fnet1_mat, fnet2_mat, fnet3_mat),dim = c( length(ids), length(ids), 3 ) ))

##LET OP HIER DUS EGO EN ALTER NOG FOUT. 
atmnet1_mat[atmnet1[, 4:5]] <- atmnet1[, 3]
atmnet2_mat[atmnet2[, 4:5]] <- atmnet2[, 3]
atmnet3_mat[atmnet3[, 4:5]] <- atmnet3[, 3]
id <- rankids$EGOid[rankids$EGOhandle==106102]

sum(atmnet1_mat[id,])
sum(atmnet2_mat[id,])
sum(atmnet3_mat[id,])
#wordt dus niet geatmenioned: dat klopt want zo is ook gezocht. dus deze zijn inderdaad missing. maar had in t3 wel geatmentioned kunnen worden. 
sum(atmnet1_mat[,id])
sum(atmnet2_mat[,id])
sum(atmnet3_mat[,id])
#heeft zelf wel geatmentioned
atmnet1_mat[id,] <- 10
atmnet2_mat[id,] <- 10	
id <- rankids$EGOid[rankids$EGOhandle==41401]
sum(atmnet1_mat[id,])
sum(atmnet2_mat[id,])
sum(atmnet3_mat[id,])
sum(atmnet1_mat[,id])
sum(atmnet2_mat[,id])
sum(atmnet3_mat[,id])
atmnet1_mat[id,] <- 10
atmnet2_mat[id,] <- 10	
#EN NU EINDELIJK EGO/ALTER OMDRAAIEN
atmnet <- sienaNet(array( c(t(atmnet1_mat), t(atmnet2_mat), t(atmnet3_mat)),dim = c( length(ids), length(ids), 3 ) ), allowOnly=FALSE)

rtnet1_mat[rtnet1[, 4:5]] <- rtnet1[, 3]
rtnet2_mat[rtnet2[, 4:5]] <- rtnet2[, 3]
rtnet3_mat[rtnet3[, 4:5]] <- rtnet3[, 3]
id <- rankids$EGOid[rankids$EGOhandle==106102]
sum(rtnet1_mat[id,])
sum(rtnet2_mat[id,])
sum(rtnet3_mat[id,])
sum(rtnet1_mat[,id])
sum(rtnet2_mat[,id])
sum(rtnet3_mat[,id])
#106102 lijkt in rt al wel in wave 2 te zitten
rtnet1_mat[id,] <- 10
#rtnet2_mat[id,] <- 10	
id <- rankids$EGOid[rankids$EGOhandle==41401]
sum(rtnet1_mat[id,])
sum(rtnet2_mat[id,])
sum(rtnet3_mat[id,])
sum(rtnet1_mat[,id])
sum(rtnet2_mat[,id])
sum(rtnet3_mat[,id])
rtnet1_mat[id,] <- 10
#rtnet2_mat[id,] <- 10	
rtnet <- sienaNet(array( c(rtnet1_mat, rtnet2_mat, rtnet3_mat),dim = c( length(ids), length(ids), 3 ) ))

#covariates ego#
rankids
keyf <- merge(key, rankids[,c("EGOhandle", "EGOid")])
keyf
names(keyf)
keyf$Naam

#recoding names
keyf$Naam2 <- c(
  "FA",
  "MA",
  "KA",
  "TvA",
  "MA",
  "HB",
  "SB",
  "VB",
  "RB",
  "MB",
  "AB",
  "HtB",
  "HBS",
  "JvD",
  "ED",
  "PD",
  "RD",
  "CDF",
  "PD",
  "JG",
  "MdG",
  "RG",
  "DG",
  "SVHB",
  "MH",
  "PH",
  "LH",
  "MvH",
  "MK",
  "JK",
  "RK",
  "NK",
  "WK",
  "HK",
  "AK",
  "TK",
  "RL",
  "HL",
  "BM",
  "PvM",
  "AM",
  "HN",
  "CNdH",
  "MvN",
  "PO",
  "FvO",
  "SO",
  "AP",
  "RvR",
  "ER",
  "MR",
  "ER",
  "Rdr",
  "AR",
  "CS",
  "GJS",
  "SS",
  "KvdS",
  "OT",
  "MT",
  "Mvt",
  "SvV",
  "KV",
  "BV",
  "JV",
  "LV",
  "AdV",
  "FW",
  "SvW",
  "GW",
  "BvtW",
  "EZ",
  "HZ",
  "MR",
  "LP",
  "JHP",
  "JD",
  "LA",
  "SD",
  "SD",
  "KD",
  "TB",
  "EB",
  "LM",
  "SK",
  "SB",
  "PK",
  "BvK",
  "CL",
  "FF",
  "MH",
  "IvE",
  "JP",
  "RJ",
  "JvE",
  "MG",
  "RR",
  "AB",
  "AD",
  "TdG",
  "RP",
  "HvdM",
  "AK",
  "CvD",
  "JvdBJ",
  "MvM",
  "DW",
  "BB",
  "SH",
  "AM",
  "DYZ",
  "DK",
  "ZeY",
  "MW",
  "AW",
  "SK",
  "JM",
  "LS",
  "MvR",
  "CvB",
  "EO",
  "KB",
  "TvdL",
  "CE",
  "ZO",
  "BS",
  "SK",
  "BvO",
  "NO",
  "LW",
  "ID",
  "LvT",
  "LvR",
  "FMA",
  "FA",
  "GvD",
  "KvdH",
  "KG",
  "TH",
  "VM",
  "GM",
  "DvW",
  "EM",
  "LdJ",
  "GP",
  "AK",
  "RvA")

keyf$Partij_col <- ifelse(keyf$Partij == "VVD",rgb(234, 133, 27, max = 255) ,NA)
keyf$Partij_col <- ifelse(keyf$Partij == "PVV", rgb(26, 41, 82, max = 255),keyf$Partij_col)
keyf$Partij_col <- ifelse(keyf$Partij == "CDA", rgb(1, 138, 56, max = 255),keyf$Partij_col)
keyf$Partij_col <- ifelse(keyf$Partij == "D66", rgb(5, 165, 57, max = 255),keyf$Partij_col)
keyf$Partij_col <- ifelse(keyf$Partij == "GroenLinks",rgb(95, 175, 50, max = 255) ,keyf$Partij_col)
keyf$Partij_col <- ifelse(keyf$Partij == "SP",rgb(220, 51, 30, max = 255) ,keyf$Partij_col)
keyf$Partij_col <- ifelse(keyf$Partij == "PvdA",rgb(164, 35, 29, max = 255) ,keyf$Partij_col)
keyf$Partij_col <- ifelse(keyf$Partij == "CU", rgb(3, 145, 219, max = 255),keyf$Partij_col)
keyf$Partij_col <- ifelse(keyf$Partij == "PvdDieren",rgb(0, 88, 50, max = 255) ,keyf$Partij_col)
keyf$Partij_col <- ifelse(keyf$Partij == "50Plus", rgb(128, 29, 119, max = 255),keyf$Partij_col)
keyf$Partij_col <- ifelse(keyf$Partij == "SGP", rgb(226, 94, 30, max = 255),keyf$Partij_col)
keyf$Partij_col <- ifelse(keyf$Partij == "DENK",rgb(24, 173, 179, max = 255) ,keyf$Partij_col)
keyf$Partij_col <- ifelse(keyf$Partij == "FvD",rgb(125, 53, 54, max = 255) ,keyf$Partij_col)
keyf$Partij_col



segments <- rep(1:6, each=25)
rows <- rep(c(1,1,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6), 6)
columns <- rep(c(1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,6),6)
seats <- as.data.frame(cbind(segments, rows, columns))
seats$X <- NA
seats$Y <- NA

#not very very neat. 
seats$X <- c(
  8.20, 7.90,
  10.0, 9.80, 9.30,
  11.8, 11.6, 11.3, 10.8,
  13.6, 13.5, 13.2, 12.8, 12.5, 
  15.4, 15.2, 14.9, 14.4, 14.0, 
  17.1, 16.9, 16.7, 16.3, 15.9, 15.4, 
  6.40, 5.30, 
  7.90, 7.10, 6.10, 
  9.50, 8.70, 7.80, 6.80, 
  11.1, 10.4, 9.60, 8.80, 7.90,
  12.7, 11.8, 10.9, 9.90, 8.80, 
  14.1, 13.3, 12.5, 11.6, 10.6, 9.50,
  3.00, 1.40,
  3.90, 2.60, 1.30,
  4.80, 3.70, 2.40, 1.30,
  5.70, 4.70, 3.60, 2.30, 1.20, 
  6.50, 5.40, 4.00, 2.60, 1.30, 
  7.30, 6.30, 5.00, 3.70, 2.60, 1.30,
  -1.30, -2.60, 
  -1.10, -2.40, -3.70, 
  -1.00, -2.30, -3.60, -4.60,
  -1.10, -2.20, -3.30, -4.50, -5.50,
  -1.20, -2.40, -3.70, -5.10, -6.30,
  -1.00, -2.30, -3.60, -4.80, -5.90, -7.20,
  -5.10, -6.10, 
  -5.90, -6.90, -7.70, 
  -6.80, -7.70, -8.60, -9.30,
  -7.70, -8.60, -9.50, -10.4, -11.0,
  -8.60, -9.60, -10.7, -11.5, -12.4, 
  -9.40, -10.3, -11.3, -12.2, -13.1, -13.9,
  -7.70, -8.00, 
  -9.00, -9.50, -9.80,
  -10.6, -11.0, -11.4, -11.5, 
  -12.2, -12.6, -13.0, -13.2, -13.3,
  -13.8, -14.3, -14.6, -15.0, -15.1,
  -15.2, -15.7, -16.1, -16.4, -16.6, -16.9
)

seats$Y <- c(
  -4.30, -2.90,
  -4.40, -3.10, -1.80,
  -4.50, -3.20, -1.90, -0.90,
  -4.50, -3.20, -2.10, -1.00, 0.00,
  -4.40, -3.20, -1.70, -0.40, 0.80, 
  -4.40, -3.20, -1.90, -0.80, 0.60, 1.60,
  -0.40, 0.50,        
  0.30, 1.40, 2.10,
  1.30, 2.20, 2.90, 3.80,
  2.20, 3.00, 4.00, 4.70, 5.40,
  3.10, 4.10, 5.10, 6.10, 6.80,
  3.80, 4.80, 5.80, 6.80, 7.50, 8.30,
  2.10, 2.40,
  3.50, 4.00, 4.20, 
  5.00, 5.50, 5.90, 6.00,
  6.50, 7.00, 7.40, 7.70, 7.80,
  8.20, 8.60, 9.10, 9.30, 9.60,
  9.60, 10.1, 10.5, 10.8, 11.0, 11.2,
  2.40, 2.10, 
  4.20, 4.00, 3.50,
  6.00, 5.90, 5.50, 5.00,
  7.80, 7.70, 7.40, 7.00, 6.50,
  9.60, 9.30, 9.10, 8.60, 8.20, 
  11.2, 11.0, 10.8, 10.5, 10.1, 9.60,
  0.50, -0.40,
  2.10, 1.40, 0.30,
  3.80, 2.90, 2.20, 1.30,
  5.40, 4.70, 4.00, 3.00, 2.20,
  6.80, 6.10, 5.10, 4.10, 3.10,
  8.30, 7.50, 6.80, 5.80, 4.80, 3.80,
  -2.90, -4.30,
  -1.80, -3.10, -4.40,
  -0.90, -1.90, -3.20, -4.50,
  0.00, -1.00, -2.10, -3.20, -4.50, 
  0.80, -0.40, -1.70, -3.20, -4.40,
  1.60, 0.60, -0.80, -1.90, -3.20, -4.40
)


plot(seats$X, seats$Y, xlim=c(-18,18), ylim=c(-18,18))
sum(duplicated(keyf[,c("ZetelSegment20170315", "ZetelRij20170315", "ZetelKolom20170315")]))
keyf <- merge(keyf, seats, by.x=c("ZetelSegment20170315", "ZetelRij20170315", "ZetelKolom20170315"), by.y=c("segments", "rows", "columns"), all.x=T)
keyf <- keyf[order(keyf$EGOid),]
plot(keyf$X, keyf$Y, xlim=c(-18,18), ylim=c(-18,18), col=keyf$Partij_col, pch=19)


m <- cbind(keyf$X, keyf$Y)
distm <- dist(m, diag=T, upper=T)
distm <- as.matrix(distm)
distm

#fysieke afstand tussen MPs in TK
afstand <- coDyadCovar(distm)

head(keyf)
#save(keyf, file="Network data/keyf.RData")

#partij, partijen, coalitie
egohandle <- coCovar(as.numeric(keyf$EGOhandle), centered=FALSE)
egoid <- coCovar(as.numeric(keyf$EGOid), centered=FALSE)
partij <- coCovar(as.numeric(keyf$Partij))
cda <- coCovar(as.numeric(as.numeric(keyf$Partij)==2))
cu <- coCovar(as.numeric(as.numeric(keyf$Partij)==3))
denk <- coCovar(as.numeric(as.numeric(keyf$Partij)==4))
d66 <- coCovar(as.numeric(as.numeric(keyf$Partij)==5))
fvd <- coCovar(as.numeric(as.numeric(keyf$Partij)==6))
gl <- coCovar(as.numeric(as.numeric(keyf$Partij)==8))
pvda <- coCovar(as.numeric(as.numeric(keyf$Partij)==10))
pvdd <- coCovar(as.numeric(as.numeric(keyf$Partij)==11))
pvv <- coCovar(as.numeric(as.numeric(keyf$Partij)==12))
sgp <- coCovar(as.numeric(as.numeric(keyf$Partij)==13))
sp <- coCovar(as.numeric(as.numeric(keyf$Partij)==14))
vvd <- coCovar(as.numeric(as.numeric(keyf$Partij)==16))
plus <- coCovar(as.numeric(as.numeric(keyf$Partij)==17))
coalitie <- coCovar(as.numeric(as.numeric(keyf$Partij)==16 | as.numeric(keyf$Partij)==2 | as.numeric(keyf$Partij)==5 | as.numeric(keyf$Partij)==3))

#EthMinZ
table(keyf$EthMinZ)
ethminz <- coCovar(keyf$EthMinZ)

#vrouw
table(keyf$Geslacht)
levels(keyf$Geslacht)
vrouw <- coCovar(as.numeric(as.numeric(keyf$Geslacht)==2))

#leeftijd
table(2017 - keyf$GebJaar)
lft <- coCovar(2017 - keyf$GebJaar)

#incumbent
table(as.numeric(keyf$Kabinet20161123))
table(as.numeric(keyf$Kamerlid20161123))

kamerlid2016 <- coCovar(keyf$Kamerlid20161123)
kabinet2016 <- coCovar(as.numeric(keyf$Kabinet20161123)-2)

#pleklijst
table(keyf$PlekLijst)
pleklijst <- coCovar(keyf$PlekLijst)
pleklijst1 <- coCovar(as.numeric(keyf$PlekLijst==1))

mydata <- sienaDataCreate(fnet, atmnet, rtnet, afstand, egohandle, egoid, partij, coalitie, 
cda,
cu,
denk,
d66,
fvd,
gl,
pvda,
pvdd,
pvv,
sgp,
sp,
vvd,
plus, 
ethminz,
vrouw,
lft,
kamerlid2016,
kabinet2016,
pleklijst,
pleklijst1
)

ls()

twitter_20190829 <- list()
twitter_20190829 [[1]] <- keyf
twitter_20190829 [[2]] <- mydata
twitter_20190829 [[3]] <- seats

save(twitter_20190829, file="Network data/twitter_20190829.RData")

