# GRAVITY: define
# for(i in c("AP1","AP2","AP3","AP4","AP5","HP1","HP2","HP3","HP4","HP5")){
#     svu[,(paste0("gravity",i)):=
#         ifelse(pend==1&BH.X<30|pend!=1&BH.X>64,
#             ifelse(BH%in%c(AP1,AP2,AP3,AP4,AP5)&str_count(i,"A"),
#               (pmax(pdist(svu[[paste0(i,".X")]],Ball.X,svu[[paste0(i,".Y")]],Ball.Y)+10,1)+
#               pmax((pdist(svu[[paste0(i,".X")]],ifelse(pend==1,4,90),svu[[paste0(i,".Y")]],25))-10,1))/
#               pdist(svu[[paste0(i,".X")]],svu[[paste0(gsub("P","D",i),".X")]],
#                      svu[[paste0(i,".Y")]],svu[[paste0(gsub("P","D",i),".Y")]]),NA),
#             ifelse(BH%in%c(HP1,HP2,HP3,HP4,HP5)&str_count(i,"H"),
#               (pmax(pdist(svu[[paste0(i,".X")]],Ball.X,svu[[paste0(i,".Y")]],Ball.Y)+10,1)+
#                pmax((pdist(svu[[paste0(i,".X")]],ifelse(pend==1,4,90),svu[[paste0(i,".Y")]],25))-10,1))/
#                      pdist(svu[[paste0(i,".X")]],svu[[paste0(gsub("P","D",i),".X")]],
#                            svu[[paste0(i,".Y")]],svu[[paste0(gsub("P","D",i),".Y")]]),NA))]
# }

# PUS: define
svu[,pus:=
      # If there isn't a stoppage in time
      ifelse(Clock!=shift(Clock)&
        # If there wasn't a shot in the last second
        !Reduce("|",shift(Event.ID1%in%c(3,4,8),0:60))&
        # And there isn't a shot in the next second
        !Reduce("|",shift(Event.ID1%in%c(3,4,8),0:60,type="lead"))&
        # And the ball is within 24 feet of the goal
        (pend==1&BH.X<28|pend!=1&BH.X>66)&
        #And the defender is over 8 feet away
        pdist(BH.X,BHD.X,BH.Y,BHD.Y)>12,
        # Then the ballhandler passed up the shot. Otherwise he didn't
        BH,0)]

# PUS: adjust for time
svu[,pus:=ifelse(shift(pus)==pus&shift(pus,2)==pus&shift(pus,3)==pus&
                 shift(pus,4)==pus&shift(pus,5)==pus,pus,0)]

# PUS: remove repeats
svu[,pus:=ifelse(Reduce("|",shift(pus!=0,1:60)),0,pus),]
# svu[,AD1.X:=NULL][,AD2.X:=NULL][,AD3.X:=NULL][,AD4.X:=NULL][,AD5.X:=NULL]
# svu[,HD1.X:=NULL][,HD2.X:=NULL][,HD3.X:=NULL][,HD4.X:=NULL][,HD5.X:=NULL]
# svu[,AD1.Y:=NULL][,AD2.Y:=NULL][,AD3.Y:=NULL][,AD4.Y:=NULL][,AD5.Y:=NULL]
# svu[,HD1.Y:=NULL][,HD2.Y:=NULL][,HD3.Y:=NULL][,HD4.Y:=NULL][,HD5.Y:=NULL]


#PUP: define
svu[,pup:=
  # If there isn't a stoppage in time
  ifelse(Clock!=shift(Clock)&
    # And BH is within 25 feet of hoop
    pdist(BH.X,ifelse(pend==1,4,90),BH.Y,25)<25&
    # And a teammate is open
    (pdist(AP1.X,AD1.X,AP1.Y,AD1.Y)>10&BH!=AP1|
    pdist(AP2.X,AD2.X,AP2.Y,AD2.Y)>10&BH!=AP2|
    pdist(AP3.X,AD3.X,AP3.Y,AD3.Y)>10&BH!=AP3|
    pdist(AP4.X,AD4.X,AP4.Y,AD4.Y)>10&BH!=AP4|
    pdist(AP5.X,AD5.X,AP5.Y,AD5.Y)>10&BH!=AP5),
    # Then BH passed up a pass. Otherwise he didn't.
    BH,0)]

# PUP: only keep occurences that lasted 2+ seconds
svu[,pup:=ifelse(Reduce("&",shift(pup!=0,1:50)),pup,0),]
# PUP: remove repeats
svu[,pup:=ifelse(Reduce("|",shift(pup!=0,1:60)),0,pup),]