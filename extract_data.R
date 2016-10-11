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
gameid<-"2016042306"
location<-"N"

# Rename game to make more easily understandable
games<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/games.csv")
games<-games[stats_id==gameid,.(stats_id,id,game)]
game_name<-games$game[1]
game_code<-games$id[1]

# EXTRACT PBP
ifelse(location=="N",
       pbp<-paste0("C:/Users/brocatoj/Documents/Basketball/svu/2016/xml/",
                  "NBA_FINALPBP_EXP$",gameid, ".XML"),
       pbp<-paste0("S:/NBA_FINALPBP_EXP$",gameid,".XML"))
pbp<-xmlTreeParse(pbp,useInternalNodes = T)
pbp<-xmlRoot(pbp)
pbp<-as.data.table(do.call(rbind,xpathApply(pbp,"//play",xmlAttrs)))

# CLEAN PBP
    # Add team codes
    teams<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/teams.csv")
    teams<-teams[,.(abbrev,stats_abbrev)]
    setnames(teams,c("team","team-alias-1"))
    setkey(teams,`team-alias-1`)

    # Create columns for game, date, home team, and away team
    pbp[,game:=game_name]
    pbp[,game_code:=game_code]
    pbp[,date:=as.Date(substr(game_name,1,10))]
    pbp[,home:=substr(game_name,16,18)]
    pbp[,away:=substr(game_name,12,14)]
    pbp[,season:=substr(game_name,1,4)]
    setnames(pbp,"id","stats_id")

    # Create team abbrev column for each event, remove starting lineup
    pbp<-left_join(pbp,teams,by="team-alias-1");setDT(pbp)
    pbp<-pbp[`textual-description`!="Starting Lineup"]

    # Create column indicating whether a possession ended
    pbp[,poss:=ifelse(is.na(shift(stats_id,type="lead"))&`event-id`%in%c(1,3,5,6), 1,
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
    pbp$stats_id<-as.numeric(pbp$stats_id)
    pbp[,possession_id:=1]
    pbp <- within(pbp, possession_id <- pmax(cumsum(poss),1))
    pbp[,possession_id:=ifelse(poss==1,possession_id,NA)]
    pbp$possession_id[nrow(pbp)]<-tail(na.omit(pbp$possession_id),1)
    pbp[,possession_id:=na.locf(possession_id,fromLast = T)]
    pbp[,possession_id:=possession_id-1]
    pbp[,chance_id:=0]
    pbp[,chance:=ifelse(shift(`event-id`)%in%c(5,8,9,10,11,12,13,16,17,18),1,0)]
    pbp[,chance:=ifelse(`event-id`==10,shift(chance),chance)]
    pbp[,chance_id:=cumsum(chance), by=possession_id]
    pbp[,possession_id:=paste0(game_code,quarter,"_",possession_id)]
    pbp[,chance_id:=paste0(possession_id,"_",chance_id)]
    pbp[,chance:=NULL]

    #Simplify PBP frame
    pbp[,id:=rownames(pbp)]
    pbp[,id:=paste0(game_code,quarter,as.numeric(id)-1)]
    pbp[,pbp_game_clock:=round(as.numeric(as.character(`time-minutes`))*60+
                               as.numeric(as.character(`time-seconds`)),0)]
    pbp[,player_id2:=ifelse(`global-player-id-2`=="",
                            `global-player-id-3`,`global-player-id-2`)]
    pbp[,poss:=ifelse(hposs==1,"home",ifelse(aposs==1,"away",NA))]
    pbp[,poss:=na.locf(poss,fromLast = T)]
    pbp[,team:=ifelse(poss=="home",home,away)]
    pbp[,opponent:=ifelse(poss=="home",away,home)]
    pbp<-pbp[,.(game,game_code,season,date,id,stats_id,quarter,pbp_game_clock,
                team,opponent,`position-id`,`global-player-id-1`,player_id2,`event-id`,
                `detail-id`,distance,`x-shot-coord`,`y-shot-coord`,fastbreak,
                `in-paint`,`second-chance`,`off-turnover`,`visitor-fouls`,
                `home-fouls`,possession_id,chance_id,`oncourt-id`,`points-type`)]
    setnames(pbp,c("quarter","oncourt-id","position-id","global-player-id-1",
                   "event-id","detail-id","x-shot-coord","y-shot-coord","in-paint",
                   "second-chance","off-turnover","visitor-fouls","home-fouls"),
                 c("period","oncourt_id","text","player_id1","event_id","detail_id",
                   "shot_x","shot_y","pitp","2nd_pts","pts_off_tov","away_fouls",
                   "home_fouls"))
    events<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/events.csv")
    pbp<-left_join(pbp,events,by="event_id");setDT(pbp)
    pbp[,event:=ifelse(event_id==3,ifelse(`points-type`==3,"3PM","2PM"),event)]
    pbp[,event:=ifelse(event_id==4,ifelse(`points-type`==3,"3PX","2PX"),event)]
    pbp<-pbp[,c(1:13,29,15:27),with=F]

# EXTRACT TRACKING DATA
svu<-data.table()

for(i in c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10",
           "OT1","OT2","OT3","OT4","OT5")){
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
    q[,idx:=rownames(q)][,idx:=as.numeric(idx)-1]
    svu<-rbind(svu,q)
    rm(q)
}

# CLEAN TRACKING DATA
    svu[,locations:=ifelse(str_count(locations,";")==9,
           paste0("-1,-1,0,0,0;",locations),locations)]
    svu[str_count(locations,";")==10]
    svu<-svu %>%
    separate(locations,c("ball_tm","ball","ball_x","ball_y","ball_z","hp1_tm",
                         "hp1","hp1_x","hp1_y","hp1_z","hp2_tm","hp2","hp2_x",
                         "hp2_y","hp2_z","hp3_tm","hp3","hp3_x","hp3_y","hp3_z",
                         "hp4_tm","hp4","hp4_x","hp4_y","hp4_z","hp5_tm","hp5",
                         "hp5_x","hp5_y","hp5_z","ap1_tm","ap1","ap1_x","ap1_y",
                         "ap1_z","ap2_tm","ap2","ap2_x","ap2_y","ap2_z","ap3_tm",
                         "ap3","ap3_x","ap3_y","ap3_z","ap4_tm","ap4","ap4_x",
                         "ap4_y","ap4_z","ap5_tm","ap5","ap5_x","ap5_y",
                         "ap5_z"),sep="\\,|\\;") %>%
    select(-ball_tm,-ball,-hp1_tm,-hp1_z,-hp2_tm,-hp2_z,-hp3_tm,-hp3_z,-hp4_tm,
           -hp4_z,-hp5_tm,-hp5_z,-ap1_tm,-ap1_z,-ap2_tm,-ap2_z,-ap3_tm,-ap3_z,
           -ap4_tm,-ap4_z,-ap5_tm,-ap5_z)
    svu<-svu[,c(38,1:37),with=F]
    setnames(svu,c("time","game-clock","game-event-id","shot-clock"),
                 c("utcTime","gameClock","game_event_id","shot_clock"))

# EXTRACT EXTRA INFO
ifelse(location=="N",
       info<-paste0("C:/Users/brocatoj/Documents/Basketball/svu/2016/xml/",
                  "NBA_FINAL_SEQUENCE_PBP_OPTICAL$",gameid, ".XML"),
       info<-paste0("S:/NBA_FINAL_SEQUENCE_PBP_OPTICAL$",gameid,".XML"))
info<-xmlTreeParse(info,useInternalNodes = T)
info<-xmlRoot(info)
info<-as.data.table(do.call(rbind,xpathApply(info,"//moment",xmlAttrs)))
setnames(info,c("event_id","gameClock","utcTime","p_id","player_id",
                "stats_id","shot_clock"))

# ATTACH EXTRA INFO TO PBP
    info2<-info[stats_id!="",.(stats_id,shot_clock)]
    pbp<-join(pbp,info2,by="stats_id",match="first");rm(info2)

# ATTACH EXTRA INFO TO TRACKING FRAME
    info<-info[,c(1,3:6),with=F]
    svu<-join(svu,info,by="utcTime",match="first")

# ATTACH PBP INFO TO TRACKING FRAME
    pbp2<-pbp[,.(stats_id,period,event,detail_id)]
    pbp2[,play:=ifelse(event=="FOUL"&detail_id==2,1,0)]
    pbp2<-pbp2[,.(stats_id,period,play)]
    svu<-join(svu,pbp2,by="stats_id",match="first")
    svu$period[nrow(svu)]<-
        ifelse(is.na(svu$period[nrow(svu)]),
           as.numeric(distinct(svu,period)[nrow(distinct(svu,period)),]),
           svu$period[nrow(svu)])
    setDT(svu)
    svu[,period:=na.locf(period,fromLast=T)]
    svu<-svu[,c(3,1:2,43,4,6:40,42,44),with=F]
    svu <- svu[, lapply(.SD, as.numeric)]
    svu[,play:=ifelse(is.na(play),0,play)]
    svu[,event_id:=ifelse(play==1,29,event_id)]
    svu<-svu[,1:41,with=F]

# fwrite(svu,paste0("J:/svu/2016/svu/",game_name,"_svu.csv"))
# fwrite(pbp,paste0("J:/svu/2016/pbp/",game_name,"_pbp.csv"))