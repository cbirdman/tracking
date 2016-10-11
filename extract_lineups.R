#-------------------------------------------------------------------------------
# DIFFERENT FILE????

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