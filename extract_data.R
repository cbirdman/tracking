library(XML)
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(zoo)
library(data.table)
source("functions.R")

# gameid<-readline('Enter Game Code: (e.g., 2016061909) ')
# location<-readline('At Work? (Y/N) ')
# gameid<-"2015110308"
# location<-"Y"

# Rename game to make more easily understandable
schedule<-fread("C:/Users/brocatoj/Desktop/2016 nba schedule.csv")
gname<-schedule[svucode==gameid,.(svucode,gamecode)]
gname<-gname$gamecode[1]

# Extract PBP and create data frame
ifelse(location=="N",
       pbp<-paste0("C:/Users/brocatoj/Documents/Basketball/svu/2016/xml/",
                  "NBA_FINALPBP_EXP$",gameid, ".XML"),
       pbp<-paste0("S:/NBA_FINALPBP_EXP$",gameid,".XML"))
pbp<-xmlTreeParse(pbp,useInternalNodes = T)
pbp<-xmlRoot(pbp)
pbp<-as.data.table(do.call(rbind,xpathApply(pbp,"//play",xmlAttrs)))

# Extract optical tracking files

# Raw Data
svu<-data.table()

for(i in c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")){
    if(!file.exists(
        paste0("C:/Users/brocatoj/Documents/Basketball/svu/2016/xml/",
               "NBA_FINAL_SEQUENCE_OPTICAL$",gameid,"_",i,".XML"))&
       !file.exists(paste0("S:/NBA_FINAL_SEQUENCE_OPTICAL$",gameid,"_",i,".XML")))
        break
    ifelse(location=="N",
           q<-paste0("C:/Users/brocatoj/Documents/Basketball/svu/2016/xml/",
                      "NBA_FINAL_SEQUENCE_OPTICAL$",gameid,"_",i,".XML"),
           q<-paste0("S:/NBA_FINAL_SEQUENCE_OPTICAL$",gameid,"_",i,".XML"))
    q<-xmlRoot(xmlTreeParse(q,useInternalNodes = T))
    q<-as.data.table(do.call(rbind,xpathApply(q,"//moment",xmlAttrs)))
    svu<-rbind(svu,q)
    rm(q)
}

svu[,locations:=ifelse(str_count(locations,";")==9,
                       paste0("-1,-1,0,0,0;",locations),locations)]
svu[str_count(locations,";")==10]
svu<-svu %>%
    separate(locations,c("Ball.Tm","Ball","Ball.X","Ball.Y","Ball.Z","HP1.Tm",
                         "HP1","HP1.X","HP1.Y","HP1.Z","HP2.Tm","HP2","HP2.X",
                         "HP2.Y","HP2.Z","HP3.Tm","HP3","HP3.X","HP3.Y","HP3.Z",
                         "HP4.Tm","HP4","HP4.X","HP4.Y","HP4.Z","HP5.Tm","HP5",
                         "HP5.X","HP5.Y","HP5.Z","AP1.Tm","AP1","AP1.X","AP1.Y",
                         "AP1.Z","AP2.Tm","AP2","AP2.X","AP2.Y","AP2.Z","AP3.Tm",
                         "AP3","AP3.X","AP3.Y","AP3.Z","AP4.Tm","AP4","AP4.X",
                         "AP4.Y","AP4.Z","AP5.Tm","AP5","AP5.X","AP5.Y",
                         "AP5.Z"),sep="\\,|\\;") %>%
    select(-Ball.Tm,-Ball,-HP1.Tm,-HP1.Z,-HP2.Tm,-HP2.Z,-HP3.Tm,-HP3.Z,-HP4.Tm,
           -HP4.Z,-HP5.Tm,-HP5.Z,-AP1.Tm,-AP1.Z,-AP2.Tm,-AP2.Z,-AP3.Tm,-AP3.Z,
           -AP4.Tm,-AP4.Z,-AP5.Tm,-AP5.Z)

# Tags
ifelse(location=="N",
       opbp<-paste0("C:/Users/brocatoj/Documents/Basketball/svu/2016/xml/",
                  "NBA_FINAL_SEQUENCE_PBP_OPTICAL$",gameid, ".XML"),
       opbp<-paste0("S:/NBA_FINAL_SEQUENCE_PBP_OPTICAL$",gameid,".XML"))
opbp<-xmlTreeParse(opbp,useInternalNodes = T)
opbp<-xmlRoot(opbp)
opbp<-as.data.table(do.call(rbind,xpathApply(opbp,"//moment",xmlAttrs)))
    # join shot clock to pbp
    op2<-opbp[`pbp-seq-number`!="",.(`pbp-seq-number`,`shot-clock`)]
    setnames(op2,"pbp-seq-number","id")
    pbp<-join(pbp,op2,by="id",match="first")
    rm(op2)
opbp<-opbp %>% select(-`game-clock`,-`shot-clock`)
svu<-left_join(svu,opbp,by="time")
op<-pbp[,.(id,quarter,`detail-description`)]
setnames(op,c("id","detail-description"),c("pbp-seq-number","play"))
op[,play:=ifelse(str_count(play,"Shooting"),1,0)]
svu<-left_join(svu,op,by="pbp-seq-number")
svu$quarter[nrow(svu)]<-
    ifelse(is.na(svu$quarter[nrow(svu)]),
       as.numeric(distinct(svu,quarter)[nrow(distinct(svu,quarter)),]),
       svu$quarter[nrow(svu)])
setDT(svu)
svu[,quarter:=na.locf(quarter,fromLast=T)]
svu<-svu[,.(time,quarter,`game-clock`,`shot-clock`,`pbp-seq-number`,
            Ball.X,Ball.Y,Ball.Z,HP1,HP1.X,HP1.Y,HP2,HP2.X,HP2.Y,
            HP3,HP3.X,HP3.Y,HP4,HP4.X,HP4.Y,HP5,HP5.X,HP5.Y,
            AP1,AP1.X,AP1.Y,AP2,AP2.X,AP2.Y,AP3,AP3.X,AP3.Y,AP4,AP4.X,AP4.Y,
            AP5,AP5.X,AP5.Y,`event-id`,`global-player-id`,play)]
setnames(svu,
  c("game-clock","pbp-seq-number","event-id","time","quarter","global-player-id"),
  c("Clock","PBP.Seq.Number","Event.ID1","Time","Quarter","Player.ID1"))
svu <- svu[, lapply(.SD, as.numeric)]
svu[,play:=ifelse(is.na(play),0,play)]
svu[,Event.ID1:=ifelse(play==1,29,Event.ID1)]
svu<-svu[,1:40,with=F]

# Add necessary columns
    # Add team codes
    tmc<-fread("reg/nba-team-codes.csv")
    tmc<-tmc[1:30,]
    tmc<-tmc[,.(code,svu)]
    setnames(tmc,c("team","team-alias-1"))

setDT(pbp)
# Create column for game
pbp[,game:=substr(gname,1,18)]
# Create column for date
pbp[,date:=as.Date(substr(gname,1,10))]
# Create column for home team
pbp[,home:=substr(gname,16,18)]
# Create column fo away team
pbp[,away:=substr(gname,12,14)]
# Create team code column for each event
pbp<-left_join(pbp,tmc,by="team-alias-1");setDT(pbp)
# Create column indicating whether a possession ended
pbp[,poss:=ifelse(is.na(shift(id,type="lead"))&`event-id`%in%c(1,3,5,6), 1,
           ifelse(`event-id`==3&
                  str_count(shift(`detail-description`,type="lead"),"Shooting")
                   &`player-id-1`==shift(`player-id-3`,type="lead"),0,
                   ifelse(`event-id`%in%c(3,6,7)|
                          `event-id`==5&
                            str_count(shift(`event-description`,type="lead"),"End ")|
                          `event-id`==1&(
                              str_count(`detail-description`,"1 of 1")|
                              str_count(`detail-description`,"2 of 2")|
                              str_count(`detail-description`,"3 of 3")),1,0)))]
    # Create column indicating whether the away team had the possession
pbp[,aposs:=ifelse(`event-id`==6,ifelse(team==home&poss==1,1,0),
            ifelse(team==away&poss==1,1,0))]
# Create column indicating whether the home team had the possession
pbp[,hposs:=ifelse(`event-id`==6,ifelse(team==away&poss==1,1,0),
            ifelse(team==home&poss==1,1,0))]
# Create column indicating the id of each possession for the game
pbp$id<-as.numeric(pbp$id)
pbp[,possid:=1]
pbp <- within(pbp, possid <- pmax(cumsum(poss),1))
pbp[,possid:=ifelse(poss==1,possid,NA)]
pbp$possid[nrow(pbp)]<-tail(na.omit(pbp$possid),1)
pbp[,possid2:=na.locf(possid,fromLast = T)]
pbp[,possid2:=ifelse(id<4,NA,possid2)]
pbp[,possid:=ifelse(is.na(possid),NA,
               paste0(gameid,"q",quarter,ifelse(aposs==1,away,home),possid-1))]
pbp[,possid:=na.locf(possid,fromLast = T)]
pbp[,possid:=ifelse(id<4,NA,possid)]

# Create column indicating whether a CHANCE ended
pbp[,chanceid:=0]
pbp[,chance:=ifelse(shift(`event-id`)%in%c(5,8,9,10,11,12,13,16,17,18),1,0)]
pbp[,chance:=ifelse(`event-description`=="Substitution",shift(chance),chance)]
pbp[,chanceid:=cumsum(chance), by=possid2]
pbp[,possid2:=NULL]
pbp[,chanceid:=paste0(possid,"-",chanceid)]
pbp[,possid:=ifelse(id<4,NA,possid)][,chanceid:=ifelse(id<4,NA,chanceid)]
pbp[,chance:=NULL]

# Create global id column
pbp[,global_id:=rownames(pbp)]
pbp[,global_id:=paste0(gameid,quarter,as.numeric(global_id)-1)]

# Combine on-court with pbp
onc<-svu[!is.na(PBP.Seq.Number),
         .(PBP.Seq.Number,AP1,AP2,AP3,AP4,AP5,HP1,HP2,HP3,HP4,HP5)]
setnames(onc,"PBP.Seq.Number","id")

ap<-fread("reg/allplayers.csv")

for (i in c("AP1","AP2","AP3","AP4","AP5","HP1","HP2","HP3","HP4","HP5")) {
    ap1<-ap %>%
        select(`svu-code`,pos) %>%
        distinct(`svu-code`,.keep_all=T)
    names(ap1)<-c(i, "pos")
    onc <- left_join(onc,ap1,by = i);rm(ap1)
}
newnames<-names(onc[2:11])
onc<-onc[-(2:11)]
names(onc)<-c("id",newnames)

onca<-select(onc,AP1:AP5)
onca<-t(onca)
onca <- apply(onca,2,sort,decreasing=F)
onca<-t(onca)
onch<-select(onc,HP1:HP5)
onch<-t(onch)
onch <- apply(onch,2,sort,decreasing=F)
onch<-t(onch)
setDT(onc)
onc[,AP1:=onca[,1]][,AP2:=onca[,2]][,AP3:=onca[,3]][,AP4:=onca[,4]][,AP5:=onca[,5]]
onc[,HP1:=onch[,1]][,HP2:=onch[,2]][,HP3:=onch[,3]][,HP4:=onch[,4]][,HP5:=onch[,5]]
rm(onca,onch)

for (i in c("AP1","AP2","AP3","AP4","AP5","HP1","HP2","HP3","HP4","HP5")) {
    ap1<-ap %>%
        select(unique_name,pos) %>%
        distinct(unique_name,.keep_all=T)
    names(ap1)<-c("player", i)
    onc <- left_join(onc,ap1,by = i);rm(ap1)
}
onc<-onc[-(2:11)]
names(onc)<-c("id",newnames);rm(newnames)

pbp<-join(pbp,onc,by="id",match="first")
rm(ap,onc,op,i)

setDT(pbp)

for (i in c("AP1","AP2","AP3","AP4","AP5","HP1","HP2","HP3","HP4","HP5")){
    pbp[[i]][nrow(pbp)]<-tail(na.omit(pbp[[i]]),1)
    pbp[[i]]<-na.locf(pbp[[i]],fromLast=T)
}
fwrite(svu,paste0("J:/svu/2016/svu/",gname,"_svu.csv"))
fwrite(pbp,paste0("J:/svu/2016/pbp/",gname,"_pbp.csv"))
# fwrite(svu,"C:/Users/brocatoj/Documents/Basketball/svu/2016/csv/2016-04-23-OKC-DAL_svu.csv")
# fwrite(pbp,"C:/Users/brocatoj/Documents/Basketball/svu/2016/csv/2016-04-23-OKC-DAL_pbp.csv")