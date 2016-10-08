library(plyr)
library(dplyr)
library(stringr)
library(zoo)
library(data.table)
source("functions.R")

# source("svu-setup.R")
# setDT(svu)
# svu<-fread("J:/svu/2016/svu/2016-04-06-HOU-DAL_svu.csv")
# pbp<-fread("J:/svu/2016/pbp/2016-04-06-HOU-DAL_pbp.csv")
svu<-fread("C:/Users/brocatoj/Documents/Basketball/svu/2016/csv/2016-04-23-OKC-DAL_svu.csv")
pbp<-fread("C:/Users/brocatoj/Documents/Basketball/svu/2016/csv/2016-04-23-OKC-DAL_pbp.csv")
#svu<-fread("reg/svu.csv")

# Create an id variable to refer to later
svu[,id:=rownames(svu)]

# Identify the shooter for each possession
svu[,Shooter:=ifelse(id==nrow(svu),0,
                     ifelse(Event.ID1%in%c(3,4,7,1,2),Player.ID1,NA))]
svu[,Shooter:=na.locf(Shooter,fromLast = T)]
data1<-svu

# Create a table for each player to determine who his closest opponent is
datalist<-list()
for(i in c("AP1","AP2","AP3","AP4","AP5","HP1","HP2","HP3","HP4","HP5")){
    ifelse(str_count(i,"A")==1,df<-svu[,c(41,1:23),with=F],
                               df<-svu[,c(41,1:8,24:38),with=F])
    setnames(df,10:24,c("O1","X1","Y1","O2","X2","Y2","O3","X3","Y3","O4","X4",
                       "Y4","O5","X5","Y5"))
    df[,Player:=svu[[i]]]
    df[,X:=svu[[paste0(i,".X")]]]
    df[,Y:=svu[[paste0(i,".Y")]]]
    df[,plr:=i]
    datalist[[i]]<-df
}

# Combine the tables back together and identify the closest defender
svu<-data.table::rbindlist(datalist)
svu[,dist1:=pdist(X,X1,Y,Y1)]
svu[,dist2:=pdist(X,X2,Y,Y2)]
svu[,dist3:=pdist(X,X3,Y,Y3)]
svu[,dist4:=pdist(X,X4,Y,Y4)]
svu[,dist5:=pdist(X,X5,Y,Y5)]
svu[,Closest:=pmin(dist1,dist2,dist3,dist4,dist5)]
svu[,Closest:=ifelse(Closest==dist1,O1,
                     ifelse(Closest==dist2,O2,
                            ifelse(Closest==dist3,O3,
                                   ifelse(Closest==dist4,O4,O5))))]

for(i in c("AP1","AP2","AP3","AP4","AP5","HP1","HP2","HP3","HP4","HP5")){
    df<-svu[plr==i,.(id,Closest)]
    data1<-left_join(data1,df,by="id")
    rm(df)
}

setDT(data1)
setnames(data1,43:52,c("AD1","AD2","AD3","AD4","AD5","HD1","HD2","HD3","HD4","HD5"))
svu<-data1;rm(data1)

# Identify the last passer at all times
svu[,Passer:=ifelse(id==1,1,ifelse(Event.ID1%in%c(22,25),Player.ID1,NA))]
svu[,Passer:=na.locf(Passer)]

# Identify the ballhandler at all times
svu[,BH:=ifelse(id==1,0,ifelse(Event.ID1!=5&Event.ID1!=6&
                            Event.ID1!=21&Event.ID1!=23,0,
              ifelse(Event.ID1%in%c(5,6,23),Player.ID1,NA)))]
svu[,BH:=na.locf(BH)]


# Set new columns to zero
for(i in c("AD1.X","AD2.X","AD3.X","AD4.X","AD5.X",
           "HD1.X","HD2.X","HD3.X","HD4.X","HD5.X",
           "AD1.Y","AD2.Y","AD3.Y","AD4.Y","AD5.Y",
           "HD1.Y","HD2.Y","HD3.Y","HD4.Y","HD5.Y",
           "Defender","BHD","S.X","S.Y","D.X","D.Y","BH.X","BH.Y",
           "BHD.X","BHD.Y","OBH.X","OBH.Y","OBHD.X","OBHD.Y","OBHD")){
    svu[,(i):=0]
}

# Identify Defender of eventual shot and Ballhandler defender at all times
for(i in c("AP1","AP2","AP3","AP4","AP5","HP1","HP2","HP3","HP4","HP5")){
    svu[,Defender:=ifelse(Shooter==svu[[i]],svu[[gsub("P","D",i)]],Defender)]
    svu[,BHD:=ifelse(BH==0,0,ifelse(BH==svu[[i]],svu[[gsub("P","D",i)]],BHD))]
}

# Identify coordinates of shooter, defender, ballhandler, etc.
for(i in c("AP1","AP2","AP3","AP4","AP5","HP1","HP2","HP3","HP4","HP5")){
    svu[,S.X:=ifelse(Shooter==svu[[i]],svu[[paste0(i,".X")]],S.X)]
    svu[,S.Y:=ifelse(Shooter==svu[[i]],svu[[paste0(i,".Y")]],S.Y)]
    svu[,D.X:=ifelse(Defender==svu[[i]],svu[[paste0(i,".X")]],D.X)]
    svu[,D.Y:=ifelse(Defender==svu[[i]],svu[[paste0(i,".Y")]],D.Y)]
    svu[,BH.X:=ifelse(BH==svu[[i]],svu[[paste0(i,".X")]],BH.X)]
    svu[,BH.Y:=ifelse(BH==svu[[i]],svu[[paste0(i,".Y")]],BH.Y)]
    svu[,BHD.X:=ifelse(BHD==svu[[i]],svu[[paste0(i,".X")]],BHD.X)]
    svu[,BHD.Y:=ifelse(BHD==svu[[i]],svu[[paste0(i,".Y")]],BHD.Y)]
    svu[,OBH.X:=ifelse(BH==svu[[i]],shift(svu[[paste0(i,".X")]],25),OBH.X)]
    svu[,OBH.Y:=ifelse(BH==svu[[i]],shift(svu[[paste0(i,".Y")]],25),OBH.Y)]
    svu[,OBHD:=ifelse(BH==0,0,ifelse(BH==svu[[i]],
                                     shift(svu[[gsub("P","D",i)]],25),OBHD))]
}


# Identify coordinates for "OLD" ballhandler defender for use in bbc
for(i in c("AP1","AP2","AP3","AP4","AP5","HP1","HP2","HP3","HP4","HP5")){
    svu[,OBHD.X:=ifelse(OBHD==svu[[i]],svu[[paste0(i,".X")]],OBHD.X)]
    svu[,OBHD.Y:=ifelse(OBHD==svu[[i]],svu[[paste0(i,".Y")]],OBHD.Y)]
}

# Find ball screens and DHOs
svu[,pick:=ifelse(((BHD.X<47&pdist(BHD.X,4,BHD.Y,25)>18&pdist(BHD.X,4,BHD.Y,25)<40)|
                 (BHD.X>=47&pdist(BHD.X,90,BHD.Y,25)>18&pdist(BHD.X,90,BHD.Y,25)<40))&
                 pdist(BHD.X,BH.X,BHD.Y,BH.Y)<10,
                 ifelse(BH==HP1 & (pdist(BHD.X,HP2.X,BHD.Y,HP2.Y)<3|pdist(BHD.X,HP3.X,BHD.Y,HP3.Y)<3|pdist(BHD.X,HP4.X,BHD.Y,HP4.Y)<3|pdist(BHD.X,HP5.X,BHD.Y,HP5.Y)<3),1,
                 ifelse(BH==HP2 & (pdist(BHD.X,HP1.X,BHD.Y,HP1.Y)<3|pdist(BHD.X,HP3.X,BHD.Y,HP3.Y)<3|pdist(BHD.X,HP4.X,BHD.Y,HP4.Y)<3|pdist(BHD.X,HP5.X,BHD.Y,HP5.Y)<3),1,
                 ifelse(BH==HP3 & (pdist(BHD.X,HP1.X,BHD.Y,HP1.Y)<3|pdist(BHD.X,HP2.X,BHD.Y,HP2.Y)<3|pdist(BHD.X,HP4.X,BHD.Y,HP4.Y)<3|pdist(BHD.X,HP5.X,BHD.Y,HP5.Y)<3),1,
                 ifelse(BH==HP4 & (pdist(BHD.X,HP1.X,BHD.Y,HP1.Y)<3|pdist(BHD.X,HP2.X,BHD.Y,HP2.Y)<3|pdist(BHD.X,HP3.X,BHD.Y,HP3.Y)<3|pdist(BHD.X,HP5.X,BHD.Y,HP5.Y)<3),1,
                 ifelse(BH==HP5 & (pdist(BHD.X,HP1.X,BHD.Y,HP1.Y)<3|pdist(BHD.X,HP2.X,BHD.Y,HP2.Y)<3|pdist(BHD.X,HP3.X,BHD.Y,HP3.Y)<3|pdist(BHD.X,HP4.X,BHD.Y,HP4.Y)<3),1,
                 ifelse(BH==AP1 & (pdist(BHD.X,AP2.X,BHD.Y,AP2.Y)<3|pdist(BHD.X,AP3.X,BHD.Y,AP3.Y)<3|pdist(BHD.X,AP4.X,BHD.Y,AP4.Y)<3|pdist(BHD.X,AP5.X,BHD.Y,AP5.Y)<3),1,
                 ifelse(BH==AP2 & (pdist(BHD.X,AP1.X,BHD.Y,AP1.Y)<3|pdist(BHD.X,AP3.X,BHD.Y,AP3.Y)<3|pdist(BHD.X,AP4.X,BHD.Y,AP4.Y)<3|pdist(BHD.X,AP5.X,BHD.Y,AP5.Y)<3),1,
                 ifelse(BH==AP3 & (pdist(BHD.X,AP1.X,BHD.Y,AP1.Y)<3|pdist(BHD.X,AP2.X,BHD.Y,AP2.Y)<3|pdist(BHD.X,AP4.X,BHD.Y,AP4.Y)<3|pdist(BHD.X,AP5.X,BHD.Y,AP5.Y)<3),1,
                 ifelse(BH==AP4 & (pdist(BHD.X,AP1.X,BHD.Y,AP1.Y)<3|pdist(BHD.X,AP2.X,BHD.Y,AP2.Y)<3|pdist(BHD.X,AP3.X,BHD.Y,AP3.Y)<3|pdist(BHD.X,AP5.X,BHD.Y,AP5.Y)<3),1,
                 ifelse(BH==AP5 & (pdist(BHD.X,AP1.X,BHD.Y,AP1.Y)<3|pdist(BHD.X,AP2.X,BHD.Y,AP2.Y)<3|pdist(BHD.X,AP3.X,BHD.Y,AP3.Y)<3|pdist(BHD.X,AP4.X,BHD.Y,AP4.Y)<3),1,0)))))))))),0),]
svu[,pick:=ifelse(lead(pick)==1,0,pick)]
svu[,Event.ID1:=ifelse(is.na(Event.ID1),0,Event.ID1)]
svu[,dho:=ifelse(Event.ID1==23&shift(Event.ID1)==22,1,0)]

# Determine what side of the court the goal is on
svu[,pend:=ifelse(!is.na(PBP.Seq.Number)&Event.ID1!=6,
           ifelse(pdist(shift(BH.X),4,shift(BH.Y),25)<pdist(shift(BH.X),90,shift(BH.Y),25),1,0),NA)]
svu[,pend:=na.locf(pend,fromLast=T)]


# DEFENSE, DEFENSIVE ERRORS, OPPS. CREATED
source("svu_de2.R")

# Identify coordinates for defender of each player
for(i in c("AP1","AP2","AP3","AP4","AP5","HP1","HP2","HP3","HP4","HP5")){
    for(d in c("AD1","AD2","AD3","AD4","AD5","HD1","HD2","HD3","HD4","HD5")){
        svu[,(paste0(d,".X")):=
                ifelse(svu[[i]]==svu[[d]],
                       svu[[paste0(i,".X")]],svu[[paste0(d,".X")]])]
        svu[,(paste0(d,".Y")):=
                ifelse(svu[[i]]==svu[[d]],
                       svu[[paste0(i,".Y")]],svu[[paste0(d,".Y")]])]
    }
}

# GRAVITY, ETC.
source("svu_gravity.R")

# REBOUNDING
source("svu_rebounding2.R")

# # CONVERT TO PBP
svu[,PBP.Seq.Number:=na.locf(PBP.Seq.Number,fromLast=T)]

# 1: OC
oc<-svu[,BHD:=shift(BHD)][tast!=0|(!is.na(oc)&oc!=0),.(id,Quarter,Clock,
        `shot-clock`,PBP.Seq.Number,oc,tast,BHD)]
oc[,game:=pbp$game[1]][,date:=pbp$date[1]]
oc[,season:=ifelse(as.numeric(substr(pbp$game[1],6,7))>6,
                   as.numeric(substr(pbp$game[1],1,4)-1),
                   as.numeric(substr(pbp$game[1],1,4)))]
oc[,player_id:=ifelse(tast==0,oc,tast)][,type:=ifelse(tast==0,"oc","tast")]
setnames(oc,c("frame","period","game_clock","shot_clock","pbp_id","oc","tast",
              "d_player_id","game","date","season","player_id","type"))
pbpposs<-pbp[,.(id,possid,chanceid)]
setnames(pbpposs,"id","pbp_id")
setkey(pbpposs,pbp_id)
setkey(oc,pbp_id)
pbpposs<-unique(pbpposs)
oc<-pbpposs[oc]
oc$id<-seq.int(nrow(oc))
setnames(oc,c("possid","chanceid"),c("poss_id","chance_id"))
oc<-oc[,.(id,season,game,date,pbp_id,poss_id,chance_id,frame,period,game_clock,
          shot_clock,type,player_id,d_player_id)]


# ap<-read.csv("reg/allplayers.csv",stringsAsFactors = F)
# ap<-ap %>%
#     select(svu.code,unique_name) %>%
#     filter(!is.na(svu.code)) %>% rename("un"=unique_name)
# tast<-ap %>% rename("tast"=svu.code)
# oc<-ap %>% rename("oc"=svu.code)
# creation<-svu %>%
#     filter(tast!=0|(!is.na(oc)&oc!=0)) %>%
#     select(PBP.Seq.Number,tast,oc,octype)
# creation<-left_join(creation,tast,by="tast")
# creation<-left_join(creation,oc,by="oc")
# creation<-creation %>%
#     rename("id"=PBP.Seq.Number) %>%
#     mutate(oc=paste(ifelse(tast==0,"",paste0("tast",tast)),
#                     ifelse(oc==0,"",paste0(octype,oc))),
#            oc_name=paste(ifelse(is.na(un.x),"",paste("tast",un.x,sep="_")),
#                     ifelse(is.na(un.y),"",paste(octype,un.y,sep="_")))) %>%
#     select(id,oc,oc_name)
# creation1<-ddply(creation, .(id), summarize, oc=paste(oc,collapse=" "))
# creation2<-ddply(creation, .(id), summarize, oc_name=paste(oc_name,collapse=" "))
# creation<-left_join(creation1,creation2,by="id")
# rm(creation1,creation2,tast,oc)
# pbp<-join(pbp,creation,by="id")
# 
# # 2: GRAVITY
# 
# # 3: SHOT DEFENSE
# defender<-ap %>% rename("Defender"=svu.code)
# def<-svu %>%
#     filter(Event.ID1%in%c(3,4,29)) %>%
#     select(PBP.Seq.Number,deftype,Defender) %>%
#     rename("id"=PBP.Seq.Number)
# def<-left_join(def,defender,by="Defender")
# def<-def %>%
#     mutate(Defender=paste0(deftype,Defender),
#            defender_name=paste(deftype,un,sep="_")) %>%
#     select(id,Defender,defender_name) %>% rename("defender"=Defender)
# pbp<-join(pbp,def,by="id")
# 
# # 4: DEFENSIVE ERRORS
# de<-ap %>% rename("de"=svu.code)
# derrors<-svu %>%
#     filter(de!=0&!is.na(de)) %>%
#     select(PBP.Seq.Number,detype,de) %>%
#     rename("id"=PBP.Seq.Number)
# derrors<-left_join(derrors,de,by="de")
# derrors<-derrors %>%
#     mutate(de=paste0(detype,de),
#            de_name=paste(detype,un,sep="_")) %>%
#     select(id,de,de_name)
# de1<-ddply(derrors, .(id), summarize, de=paste(de,collapse=" "))
# de2<-ddply(derrors, .(id), summarize, de_name=paste(de_name,collapse=" "))
# de<-left_join(de1,de2,by="id")
# rm(derrors,de1,de2)
# pbp<-join(pbp,de,by="id")
# 
# # 4.1: BIT
# bit<-ap%>%rename("ap"=svu.code)
# bit<-svu %>%
#     filter(bit!=0&!is.na(bit)) %>%
#     select(PBP.Seq.Number,bit) %>%
#     rename("id"=PBP.Seq.Number)
# bit<-ddply(bit, .(id), summarize, bit=paste(bit,collapse=" "))
# pbp<-join(pbp,bit,by="id")
# 
# # # 6: REBOUNDING
# reb<-svu %>%
#     filter(box!=""|crash!=""|leak!=""|bw!=""|ora!=0|go!="") %>%
#     select(PBP.Seq.Number,box,crash,leak,bw,ora,go) %>%
#     mutate(reb=paste(box,crash,leak,bw,ora,go)) %>%
#     rename("id"=PBP.Seq.Number) %>%
#     select(id,reb)
# reb<-ddply(reb, .(id), summarize, reb=paste(reb,collapse=" "))
# pbp<-join(pbp,reb,by="id")
# 
# # write.csv(pbp,paste0("C:/Users/brocatoj/Documents/Basketball/svu/2016/pbp/",
# #                      gameid,".csv"),row.names=F)