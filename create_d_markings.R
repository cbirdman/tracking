# DEF TYPES: define
svu[,deftype:=
      # If there is a missed shot or shooting foul
      ifelse(Event.ID1==3|Event.ID1==4|Event.ID1==29,
        # If the shot was immediately after an ORB, it's tip defense
        ifelse(Reduce("|",shift(Event.ID1==5,1:20)),"defti",
        #If the defender is different from who it was a short time ago
        ifelse(Defender!=shift(Defender,25)|Defender!=shift(Defender,15)|
                               Defender!=shift(Defender,10),
            # If it's close to the hoop, it's defhe, if not it's defclhe
            ifelse(((S.X<17&S.Y>10&S.Y<40)|(S.X>77&S.Y>10&S.Y<40)),
                                          "defhe","defclhe"),
        # If the defender moved more than 5 feet from 20 frames ago
        ifelse(pdist(D.X,shift(D.X,20),D.Y,shift(D.Y,20))>5&
              # And defender was 2+ feet away from shooter 20 frames ago
              pdist(shift(S.X,20),shift(D.X,20),shift(S.Y,20),shift(D.Y,20))>2&
              # And the shot isn't close to the hoop, then it's defcl
              !((S.X<17&S.Y>10&S.Y<40)|(S.X>77&S.Y>10&S.Y<40)),"defcl",
        # If there was a screen in the last 75 frames
        ifelse((Reduce("|",shift(pick==1,1:75))|
                # Or there was a pass in the last 60 frames
                Reduce("|",shift(Event.ID1==22|Event.ID1==25,1:60)))&
                # And defender was 2+ feet away from shooter 20 frames ago 
                pdist(shift(S.X,20),shift(D.X,20),shift(S.Y,20),shift(D.Y,20))>=2,
                    # If the shot was close to the hoop, it's defre, else it's defcl
                    ifelse((S.X<17&S.Y>10&S.Y<40)|(S.X>77&S.Y>10&S.Y<40),"defre","defcl"),
        # If the shooter is 10+ feet from the defender
        ifelse(pdist(D.X,S.X,D.Y,S.Y)>10&
              # And the shot is close to the hoop, then it's defna
              ((S.X<17&S.Y>10&S.Y<40)|(S.X>77&S.Y>10&S.Y<40)),"defna",
        # If the shooter is 10+ feet and it's away from the hoop it's defcl,
        # Otherwise that shit is def bruh
        ifelse(pdist(D.X,S.X,D.Y,S.Y)>10,"defcl","def")))))),NA)]

# BB: define
svu[,bb:=
      # If the ballhandler remains continous
      ifelse(BHD==shift(BHD,25)&BH==shift(BH,25)&
        # And BHD remains continuous and it's not the beginning of a quarter
        BHD==shift(BHD,10)&BH==shift(BH,10)&Clock!=720&
        # And the ballhandler moved 4ft closer to hoop in the last second
        (pdist(shift(BH.X,25),ifelse(pend==1,4,90),shift(BH.Y,25),25)-
         pdist(BH.X,ifelse(pend==1,4,90),BH.Y,25))>4&
        # And the ballhandler is closer to the hoop than his defender
        (pdist(BHD.X,ifelse(pend==1,4,90),BHD.Y,25)-
         pdist(BH.X,ifelse(pend==1,4,90),BH.Y,25))>0&
        # And the defender was (nearly) closer to the hoop a second ago
        (pdist(shift(BH.X,25),ifelse(pend==1,4,90),shift(BH.Y,25),25)-
         pdist(shift(BHD.X,25),ifelse(pend==1,4,90),shift(BHD.Y,25),25))>-0.5,
        # Then the defender was blown by. Otherwise he wasn't.
        BHD,0)]


# BB: remove repeats
svu[,bb:=ifelse(Reduce("|",shift(bb!=0,1:50)),0,bb),]

# BIGBB: not guarding him x frames ago, but beat by him in sam way as bb...
svu[,bigbb:=
  # If the ballhandler defender is not continous
  ifelse(pend==1&BHD!=shift(BHD,25)&BH==shift(BH,25)&
    # And BHD remains continuous and it's not the beginning of a quarter
    BHD==shift(BHD,10)&BH==shift(BH,10)&Clock!=720&
    # And the ballhandler moved 4ft closer to hoop in the last second
    (pdist(shift(BH.X,10),ifelse(pend==1,4,90),shift(BH.Y,10),25)-
     pdist(BH.X,ifelse(pend==1,4,90),BH.Y,25))>4&
    # And the ballhandler is closer to the hoop than his defender
    (pdist(BHD.X,ifelse(pend==1,4,90),BHD.Y,25)-
     pdist(BH.X,ifelse(pend==1,4,90),BH.Y,25))>0&
    # And the defender was (nearly) closer to the hoop a second ago
    (pdist(shift(BH.X,10),ifelse(pend==1,4,90),shift(BH.Y,10),25)-
     pdist(shift(BHD.X,10),ifelse(pend==1,4,90),shift(BHD.Y,10),25))>-0.5&
    # And there is a ball screen involved
    (Reduce("|",shift(pick==1,0:75))|
     Reduce("|",shift(pick==1,0:50,type="lead"))),
    # Then the big was blown by. Otherwise he wasn't.
    BHD,0)]

# BIGBB: remove repeats
svu[,bigbb:=ifelse(Reduce("|",shift(bigbb!=0,1:50)),0,bigbb),]

# HN: define
svu[,hn:=ifelse((Reduce("|",shift(pick==1,0:75))|
                     Reduce("|",shift(pick==1,0:50,type="lead")))&bb!=0,bb,0)]

# BBCL: define
svu[,bbcl:=ifelse(Reduce("|",shift(Event.ID1==22|Event.ID1==25,1:75))&
                      BH.X>15&BH.X<79&bb!=0&hn==0,bb,0),]

# BB: remove those categorized as hn, bbcl, or bigbb
svu[,bigbb:=ifelse(is.na(bigbb),0,bigbb)]
svu[,bb:=ifelse(hn!=0|bbcl!=0|Reduce("|",shift(bigbb!=0,1:60))|
                    Reduce("|",shift(bigbb!=0,1:60,type="lead")),0,bb)]

# HN: remove those categorized as bigbb or bbcl
svu[,hn:=ifelse(Reduce("|",shift(bigbb!=0|bbcl!=0,1:60))|
                    Reduce("|",shift(bigbb!=0,1:60,type="lead")),0,hn)]

# BB: Add in bigbb and remove bigbb column
svu[,bb:=ifelse(bigbb!=0,bigbb,bb)]
svu[,bigbb:=NULL]

# BIT: first take from leftover bb
svu[,bit:=ifelse(bbcl!=0&pend==1&BH.X>47,paste0("bit",bbcl),
          ifelse(bbcl!=0&pend!=1&BH.X<=47,paste0("bit",bbcl),0))]
svu[,bbcl:=ifelse(bit!=0,0,bbcl)]

# BIT: define
for(i in c("AP1","AP2","AP3","AP4","AP5","HP1","HP2","HP3","HP4","HP5")){
    # Define "x" to save space
    eks<-svu[[paste0(i,".X")]]
    svu[,bit:=
      # If hoop is on RIGHT and BH gets in front court and BH is away
      ifelse(pend!=1&BH.X>56&BH.X<58&BH%in%c(AP1,AP2,AP3,AP4,AP5)&
        # And BH was on the LEFT 20 frames ago and the loop is on home
        shift(BH.X,20)<47&eks<64&str_count(i,"H"),
        # If an away player beat the home player
        ifelse((((HP1.X>eks)+(HP2.X>eks)+(HP3.X>eks)+(HP4.X>eks)+(HP5.X>eks))-
             ((AP1.X>eks)+(AP2.X>eks)+(AP3.X>eks)+(AP4.X>eks)+(AP5.X>eks)))<0,
        # Then he was beat in transition. Otherwise he wasn't.         
        paste(bit,paste0("bit",svu[[i]])),bit),
      # If hoop is on RIGHT and BH gets in front court and BH is home
      ifelse(pend!=1&BH.X>56&BH.X<58&BH%in%c(HP1,HP2,HP3,HP4,HP5)&
        # And BH was on the LEFT 20 frames ago and the loop is on away
        shift(BH.X,20)<47&eks<64&str_count(i,"A"),
        # If a home player beat the away player
        ifelse((((HP1.X>eks)+(HP2.X>eks)+(HP3.X>eks)+(HP4.X>eks)+(HP5.X>eks))-
                ((AP1.X>eks)+(AP2.X>eks)+(AP3.X>eks)+(AP4.X>eks)+(AP5.X>eks)))>0,
        # Then he was beat in transition. Otherwise he wasn't
        paste(bit,paste0("bit",svu[[i]])),bit),
     # If hoop is on LEFT and BH gets in front court and BH is away
     ifelse(pend==1&BH.X>36&BH.X<38&BH%in%c(AP1,AP2,AP3,AP4,AP5)&
       # And BH was on the RIGHT 20 frames ago and the loop is on home
       shift(BH.X,20)>47&eks>30&str_count(i,"H"),
       # If an away player beat the home player
       ifelse((((HP1.X>eks)+(HP2.X>eks)+(HP3.X>eks)+(HP4.X>eks)+(HP5.X>eks))-
               ((AP1.X>eks)+(AP2.X>eks)+(AP3.X>eks)+(AP4.X>eks)+(AP5.X>eks)))>0,
       # Then he was beat in transition. Otherwise he wasn't.
       paste(bit,paste0("bit",svu[[i]])),bit),
     # IF the hoop is on the LEFT and BH gets i front court and BH is home
     ifelse(pend==1&BH.X>36&BH.X<38&BH%in%c(HP1,HP2,HP3,HP4,HP5)&
      # And BH was on the RIGHT 20 frames ago and the loop is on away
      shift(BH.X,20)>47&eks>30&str_count(i,"A"),
      # If a home player beat the away player
      ifelse((((HP1.X>eks)+(HP2.X>eks)+(HP3.X>eks)+(HP4.X>eks)+(HP5.X>eks))-
              ((AP1.X>eks)+(AP2.X>eks)+(AP3.X>eks)+(AP4.X>eks)+(AP5.X>eks)))<0,
      # Then he was beat in transition. Otherwise he wasn't.
      paste(bit,paste0("bit",svu[[i]])),bit),bit))))]
}

# BIT: remove repeats
svu[,bit:=ifelse(Reduce("|",shift(bit!=0,1:75)),0,bit),]

# BIT: remove those categorized as bb
svu[,bit:=ifelse(Reduce("|",shift(bb!=0,1:60))|
                 Reduce("|",shift(bb!=0,1:60,type="lead")),0,bit)]

# BBC: define
svu[,bbc:=
        # If there wasn't a bb in the last 50 frames
        ifelse(Clock!=720&!Reduce("|",shift(bb!=0,1:50))&BH!=shift(BH,25)&
                   # And BH moved 4+ feet closer to hoop than 1 sec ago and now < 12 ft
                   (pdist(OBH.X,ifelse(pend==1,4,90),OBH.Y,25)-
                    pdist(BH.X,ifelse(pend==1,4,90),BH.Y,25))>4&
                    pdist(BH.X,ifelse(pend==1,4,90),BH.Y,25)<12&
                   # And BH is closer to hoop than BHD of 1 sec ago
                   (pdist(OBHD.X,ifelse(pend==1,4,90),OBHD.Y,25)-
                    pdist(BH.X,ifelse(pend==1,4,90),BH.Y,25))>0&
                   # And there wasn't an ORB or screen in the last x seconds
                   !Reduce("|",shift(Event.ID1==5,0:25))&
                   !Reduce("|",shift(pick==1,0:75))&
                   # And there was a pass in the last second
                   Reduce("|",shift(Event.ID1==22|Event.ID1==25,0:40)),
               # Then the defender was beat by a cut. Otherwise he wasn't.
               OBHD,0)]

# BBC: remove repeats
svu[,bbc:=ifelse(Reduce("|",shift(bbc!=0,1:75)),0,bbc),]

# BBC: no error in next x or last x
svu[,bbc:=ifelse(Reduce("|",shift(bit!=0|hn!=0|bb!=0|bbcl!=0,1:60,type="lead"))|
                     Reduce("|",shift(bit!=0|hn!=0|bb!=0|bbcl!=0,1:60)),0,bbc),]

# TAST: define
svu[,tast:=
  # If there was a pass/ast in the last 60
  ifelse(Reduce("|", shift(Event.ID1%in%c(22,25),1:60))&
    # And the result is a fga or shooting foul
    Event.ID1%in%c(3,4,29)&
    # And either there was a bb in the last sec or the deftype isn't def
    (Reduce("|",shift(bb!=0,1:60))|deftype!="def"),
    # Then the passer is credited with a tast. Otherwise he isn't.
    Passer,0)]

# OC: define
svu[,oc:=
  # If there is an assist within 6 seconds
  ifelse(Reduce("|",shift(tast!=0,1:150,type="lead"))&
    # And there is a pass within a second
    Reduce("|",shift(Event.ID1%in%c(22,25),1:25,type="lead"))&
    # And there isn't a dho within a second
    !Reduce("|",shift(dho==1,1:25,type="lead"))&
    # And there wasn't an orb in the last 2 seconds
    !Reduce("|",shift(Event.ID1==5,1:50))&
    # And the ballhandler is within 40 feet of the baseline
    (pend==1&BH.X<40|pend==0&BH.X>=54)&
    # And the shooter isn't the bh and the bhd is within 10 feet of the bh
    Shooter!=BH&pdist(BH.X,BHD.X,BH.Y,BHD.Y)<10,
    # If the shooter drew a second defender
    ifelse(
        ifelse(BH%in%c(AP1,AP2,AP3,AP4,AP5),
          ((pdist(BH.X,HP1.X,BH.Y,HP1.Y)<8)+(pdist(BH.X,HP2.X,BH.Y,HP2.Y)<8)+
          (pdist(BH.X,HP3.X,BH.Y,HP3.Y)<8)+(pdist(BH.X,HP4.X,BH.Y,HP4.Y)<8)+
          (pdist(BH.X,HP5.X,BH.Y,HP5.Y)<8))>=2,
          ((pdist(BH.X,AP1.X,BH.Y,AP1.Y)<8)+(pdist(BH.X,AP2.X,BH.Y,AP2.Y)<8)+
           (pdist(BH.X,AP3.X,BH.Y,AP3.Y)<8)+(pdist(BH.X,AP4.X,BH.Y,AP4.Y)<8)+
           (pdist(BH.X,AP5.X,BH.Y,AP5.Y)<8))>=2)|
      # Or BH blew by his defender    
      bb!=0|bbc!=0|bit!=0,
      # Then BH is credited with an oc. Otherwise he isn't.
      BH,0),0)]

# OC: remove repeats
svu[,oc:=ifelse(Reduce("|",shift(oc!=0,1:150)),0,oc),]

# OC: types
svu[,octype:=ifelse(oc!=0,
             ifelse(Reduce("|",shift(pick!=0,0:75))|
                    Reduce("|",shift(pick!=0,0:50,type="lead")),"poc","oc"),0)]

# TAST: Change to the pass not the shot
svu[,tast1:=
        ifelse(Event.ID1%in%c(22,25)&
                   Reduce("|", shift(tast!=0,1:60,type="lead"))&
                   !Reduce("|", shift(Event.ID1%in%c(22,25),1:60,type="lead")),
               shift(BH),0)]
svu[,tast:=NULL]
setnames(svu,"tast1","tast")

# DE: define
svu[,de:=
  # If there is an oc
  ifelse(oc!=0,
    # If there isn't a bb,bbcl,hn,bit, or bbc within the last 4 or next 2 secs
    ifelse(Reduce("|",shift(bb!=0|bbcl!=0|hn!=0|bit!=0|bbc!=0,1:120))|
           Reduce("|",shift(bb!=0|hn!=0|bit!=0|bbc!=0,1:60,type="lead")),0,
             # If there isn't a screen in the last 4 or next 2 secs
             ifelse(Reduce("|",shift(pick!=0,1:120))|
                    Reduce("|",shift(pick!=0,1:60,type="lead"))|
                    # Or the bhd is within 4 ft of where he was 1 sec ago
                    pdist(BHD.X,shift(BHD.X,20),BHD.Y,shift(BHD.Y,20))<4,
            # Then it's help needed. Otherwise it's an error
            paste("hn",BHD),paste("de",BHD))),
    # If the defender on the shot is helping
    ifelse(deftype%in%c("defhe","defclhe"),
      # If there isn't already a defensive error       
      ifelse(Reduce("|",shift(bb!=0|bbcl!=0|hn!=0|bit!=0|bbc!=0,1:120))|
             Reduce("|",shift(bb!=0|hn!=0|bit!=0|bbc!=0,1:60,type="lead")),0,
             # If there's a screen
             ifelse(Reduce("|",shift(pick!=0,1:120))|
                    Reduce("|",shift(pick!=0,1:60,type="lead")),
             # Then it's help needed
            paste("hn",BHD),
            # Otherwise it's a defensive error
            ifelse(Defender!=shift(Defender,20),paste("de",shift(Defender,20)),
            ifelse(Defender!=shift(Defender,15),paste("de",shift(Defender,15)),
            ifelse(Defender!=shift(Defender,10),paste("de",shift(Defender,10)),0))))),0))]
#svu[,de:=ifelse(is.na(de)|str_count(de,as.character(Defender)),0,de)]
svu[,de:=ifelse(is.na(de),0,de)]
svu[,hn2:=ifelse(pick==1&Reduce("|",shift(de=="hn 0",1:150,type="lead")),
                 shift(BHD,10),NA)]
svu[,hn2:=ifelse(id==1,0,hn2)]
svu[,hn2:=na.locf(hn2)]
svu[,de:=ifelse(de=="hn 0",paste("hn",hn2),de)]
svu[,hn2:=NULL]


# DE: remove repeats
svu[,de:=ifelse(Reduce("|",shift(!is.na(de)&de!=0,1:150)),0,de)]


# Merge defensive errors
svu[,bit:=ifelse(is.na(bit),0,bit)]
svu[,detype:=substr(de,1,2)]
svu[,de:=substr(de,4,nchar(de))]
svu$de<-as.numeric(svu$de)
svu[,de:=ifelse(is.na(de),0,de)]
svu[,de:=bb+hn+bbcl+bbc+de]
svu[,detype:=ifelse(detype!="0",detype,
                    ifelse(bb!=0,"bb",
                           ifelse(hn!=0,"shn",
                                  ifelse(bbcl!=0,"bbcl",
                                                ifelse(bbc!=0,"bbc",NA)))))]
svu[,detype:=gsub("de","hn",detype)]
svu[,bb:=NULL][,hn:=NULL][,bbcl:=NULL]

# Adjust DE for recovery
svu[,fdefender:=ifelse(id==nrow(svu),0,NA)]
svu[,fdefender:=ifelse(deftype%in%c("defhe","defclhe"),Defender,fdefender)]
svu[,fdefender:=na.locf(fdefender,fromLast=T)]
svu[,fdefender:=ifelse(Reduce("|",shift(deftype%in%c("defhe","defclhe"),0:60,
                                        type="lead")),fdefender,0)]
svu[,deftype:=ifelse(is.na(deftype),0,deftype)]
dtypes<-c("defti","defhe","defclhe","defcl","defre","defna")
svu[,de:=
  #If deftype isn't "def"
  ifelse(Reduce("|",shift(deftype%in%dtypes,0:150,type="lead"))&
    # And there wasn't an ORB in the last 40 frames
    !Reduce("|",shift(Event.ID1==5,1:40))&
    # And the error isn't attributed to the help defender
    fdefender!=de,
    # Then the error stands. Otherwise it doesn't.
    de,0)]

# Forced turnovers
svu[,fort:=0]
for(i in c("AP1","AP2","AP3","AP4","AP5","HP1","HP2","HP3","HP4","HP5")){
    svu[,fort:=ifelse(Event.ID1==7,svu[[gsub("P","D",i)]],fort)]
}

# KIF: define
svu[,kif:=
  # If the distance between the ballhandler and the hoop        
  ifelse((pdist(BH.X,ifelse(pend==1,4,90),BH.Y,25)-
          # is 2+ feet greater than it was a second ago
          pdist(shift(BH.X,25),ifelse(pend==1,4,90),shift(BH.Y,25),25))>2&
          # And the ballhanlder is within 27 feet of the hoop
          pdist(BH.X,ifelse(pend==1,4,90),BH.Y,25)<27&
          # And the bh & bhd are continuous
          BH==shift(BH,25)&BHD==shift(BHD,25),
  BHD,0)]

# KIF: remove replicates
svu[,kif:=ifelse(Reduce("|",shift(kif!=0,1:50)),0,kif),]