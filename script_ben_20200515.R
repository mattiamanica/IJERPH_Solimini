



library(here)
library(tidyverse)

#script Q ben

dati_Q_Ben <- read.csv(here("data","dati_Q_Ben2.csv"), na.strings="",sep=";",stringsAsFactors = TRUE)
Q_completo_life.cicle.anni.italia <- read.csv(here("data","Q_completo_life cicle anni italia.csv"), sep=";",stringsAsFactors = TRUE)


##house keeping
db<-dati_Q_Ben
db$conosce_zanzaratigre2=factor(db$conosce_zanzaratigre)
levels(db$conosce_zanzaratigre2)[levels(db$conosce_zanzaratigre2)=="1- no"]<-0
levels(db$conosce_zanzaratigre2)[levels(db$conosce_zanzaratigre2)=="4- altro"]<-0
levels(db$conosce_zanzaratigre2)[levels(db$conosce_zanzaratigre2)=="2- si la riconosco"]<-1
levels(db$conosce_zanzaratigre2)[levels(db$conosce_zanzaratigre2)=="si generico"]<-1
levels(db$conosce_zanzaratigre2)[levels(db$conosce_zanzaratigre2)=="3- si ho sentito parlare"]<-0
levels(db$conosce_zanzaratigre2)[levels(db$conosce_zanzaratigre2)=="no"]<-0
levels(db$azioni_evitare)[levels(db$azioni_evitare)=="2- vapo-zampironi"]<-"2- vapo/zampironi"

str(db)
db$paese_etnia2=factor(db$paese_etnia)
#cbind(db,db$paese_etnia2)
#str(db)
#db$paese_etnia2 <-factor(db$paese_etnia2)
levels(db$paese_etnia2)[levels(db$paese_etnia2)=="albania"]<-NA
levels(db$paese_etnia2)[levels(db$paese_etnia2)=="bulgaria"]<-NA
levels(db$paese_etnia2)[levels(db$paese_etnia2)=="romania"]<-NA
levels(db$paese_etnia2)[levels(db$paese_etnia2)=="russia"]<-NA
levels(db$paese_etnia2)[levels(db$paese_etnia2)=="tunisia"]<-NA
levels(db$paese_etnia2)[levels(db$paese_etnia2)=="ucraina"]<-NA
levels(db$paese_etnia2)[levels(db$paese_etnia2)=="altro"]<-NA
levels(db$paese_etnia2)


db$areaverde_vicina2<-  recode(db$areaverde_vicina, '1- no' = "no",'2- si parco '="yes", '3- si campagna'="yes",'4- si tipo verde non specificato'= "yes", '5- altro verde' = "yes",'6- giardino condominio'="yes" )
db$piano2<-  factor(ifelse(db$piano<= 1, "1- ground floor or lower",ifelse(db$piano>0 & db$piano<3,"2- first or second floor", "3- third floor or higher")))
db$areaverde_vicina2<-  recode(db$areaverde_vicina, '1- no' = "no",'2- si parco '="yes", '3- si campagna'="yes",'4- si tipo verde non specificato'= "yes", '5- altro verde' = "yes",'6- giardino condominio'="yes" )
db$area_esterna<-recode(db$area_esterna, '1- nessuno' = "1- none",'2- giardino'="2- garden", '3- balcone'="3- balcony")
db$abitazione_tipo<-recode(db$abitazione_tipo, '1- appartamento' = "1- apartment", '2- casaindip_villa' = "2- indipendent house")
db$punture_disturbo_ben<-recode(db$punture_disturbo, '1- per niente' = 0, '2- poco' = 0, '3- molto'=1)
db$conosce_malattie_ben<-ifelse(db$conosce_malattie_ben=="si",1,0)  
db$preoccupazione_malattie_ben<-ifelse(db$preoccupazione_malattie_ben=="si",1,0)  
db$zanzare_disturbo_ben<-recode(db$zanzare_disturbo, '1- per niente' = 0, '2- piu no che si' = 0, '3- qualche volta'=0, '4- piu si che no'=1,'5- si molto'=1)
db$azioni_evitare_ben<-ifelse(db$azioni_evitare=="1- niente",0,1)


db$sesso_F <-factor(db$sesso_F)
levels(db$sesso_F)[levels(db$sesso_F)=="1"] <- "woman"
levels(db$sesso_F)[levels(db$sesso_F)=="0"] <- "man"
table(db$sesso_F)

#db$bmi<-db$peso/((db$altezza/100)^2)
#summary(db$bmi)

db$livelloeducativo <-factor(db$livelloeducativo)
#str(db$livelloeducativo)
levels(db$livelloeducativo)

db<-db %>% 
  mutate(livelloeducativo2= recode(livelloeducativo, `1- elementari`="1- elementary school", `2- medie`="2- junior high school",`3- superiori`="3- high school", `4- laurea`="4- university degree"))

db$zanzariere_si <-factor(db$zanzariere_si)
levels(db$zanzariere_si)[levels(db$zanzariere_si)=="1"] <- "yes"
levels(db$zanzariere_si)[levels(db$zanzariere_si)=="0"] <- "no"

levels(db$preoccupazione_malattie)[levels(db$preoccupazione_malattie)=="3-a volte"]<-"3- a volte"
db<-db %>% 
  mutate(preoccupazione_malattie= recode(preoccupazione_malattie, `1- decisamente no`="1- never / rarely", `2- piu no che si`="1- never / rarely",`3- a volte`="2- sometimes", `4- piu si che no`="3- very often / always", `5- decisamente si`="3- very often / always" ))

db<- db %>% 
  mutate(zanzare_disturbo=recode(zanzare_disturbo, '1- per niente' = "1- never / rarely", '2- piu no che si' = "1- never / rarely", '3- qualche volta'="2- sometimes", '4- piu si che no'="3- very often / always",'5- si molto'="3- very often / always"))

levels(db$ora_punture)[levels(db$ora_punture)=="5- notte"]<-"3- evening/night"
levels(db$ora_punture)[levels(db$ora_punture)=="4- sera"]<-"3- evening/night"

levels(db$ora_punture)[levels(db$ora_punture)=="2- mattina presto"]<-"2- mattina presto / pomeriggio tardi"
levels(db$ora_punture)[levels(db$ora_punture)=="3- pomeriggio tardi"]<-"2- mattina presto / pomeriggio tardi"

levels(db$azioni_evitare)[levels(db$azioni_evitare)=="7- vapo/zampironi/spray"]<-"2- vapo/zampironi"

names<-c("abitazione_tipo", "area_esterna", "zanzariere_si","areaverde_vicina","zanzare_disturbo","ora_punture","punture_disturbo", "azioni_evitare","preoccupazione_malattie",	"preoccupazione_malattie_ben",	"conosce_zanzaratigre2",	"conosce_ciclo",	"conosce_malattie")

db[,names] <- lapply(db[,names] , factor)
str(db)

db <- db %>% 
  filter(!is.na(eta),!eta<18,!is.na(paese_etnia2) )%>%
  rename(etnic_group=`paese_etnia2`,age=`eta`, sex=sesso_F, edu_level=`livelloeducativo2`, house_type=`abitazione_tipo`, house_floor=`piano2`, house_out_space=`area_esterna`, green_area=`areaverde_vicina2`,
                    house_window_screen=`zanzariere_si`, k_lifecycle=`conosce_ciclo`, k_bitetime=`ora_punture`, 
                    k_disease=`conosce_malattie`, a_diseaseworry=`preoccupazione_malattie`, a_bite_nuisance=`punture_disturbo`, a_mosq_nuisance=`zanzare_disturbo`, p_action=`azioni_evitare`) 


###descrittive
table1<-db %>%
  gather(variable,value,sex, edu_level, house_type, house_floor, house_out_space, green_area,
         house_window_screen) %>% #trasforma in una singola variable con valori value; passaggio necessario
  count(etnic_group,variable,value) %>%
  group_by(etnic_group,variable) %>% #aggrega per etnia e singole variabili
  #summarise (n=n()) %>% #calcola frequenze assolute
  filter(!is.na(value)) %>% #elimina NA
  mutate(freq = round( n / sum(n),2)) %>% #calcola le proporzioni
  subset(select=-c(n))%>%  #elimina la colonna n che non serve
  filter(!is.na(value)) %>% #elimina NA
  spread(etnic_group,freq) #crea rxc table

table1<-data.frame(table1[,1:5])

db %>% 
  select(etnic_group,sex, edu_level, house_type, house_floor, house_out_space, green_area,
         house_window_screen) %>% 
    summarise_all(~chisq.test(.,db$etnic_group)$p.value)

chisq.test(db$sex,db$etnic_group)
chisq.test(db$edu_level,db$etnic_group)
chisq.test(db$house_type,db$etnic_group)
chisq.test(db$house_floor,db$etnic_group)
chisq.test(db$house_out_space,db$etnic_group)
chisq.test(db$green_area,db$etnic_group)
chisq.test(db$house_window_screen,db$etnic_group)


medie<-db %>%
  dplyr::select(age, etnic_group) %>% # select variables to summarise
    group_by(etnic_group)%>%
           summarise_each(list(min = min, 
                      max = max,
                      mean = mean)) 

medie<-medie %>%gather(key = stat, value = value, -etnic_group) %>%spread(etnic_group,value)
medie$value<-medie$stat
medie$variable<-"age"
medie$stat<-NULL
medie<-medie %>% select(variable, value, 'india-kerala', 'india-punjab','italia')
medie<-data.frame(medie)

medie<-medie %>% mutate_if(is.numeric, round, 2)

table1<-rbind(table1,medie) #tabella 1
rm(medie)
write.table(table1,here("output","table1.txt"),sep="|",row.names =F,quote=FALSE)


table2<-db %>% 
  gather(key = question,value = answer, k_bitetime, 
         k_disease, a_diseaseworry, a_bite_nuisance, a_mosq_nuisance, p_action,na.rm=T) %>%
  group_by(etnic_group,question,answer) %>%
     summarise (n=n()) %>%
     mutate(freq = n / sum(n)) 
table2


likdb <- db %>% 
  gather(key = question,value = answer, k_bitetime, 
         k_disease, a_diseaseworry, a_bite_nuisance, a_mosq_nuisance, p_action,na.rm=T) %>%
  group_by(etnic_group,question,answer) %>%
  summarise (n=n()) 
levels(likdb$etnic_group) <- c("Malayalis","Punjabis","Italian")

library(HH)
require(latticeExtra)

# define a custom panel function
myPanelFunc <- function(...){
  panel.likert(...)
  vals <- list(...)
  DF <- data.frame(x=vals$x, y=vals$y, groups=vals$groups)
  
  ### some convoluted calculations here...
  grps <- as.character(DF$groups)
  for(i in 1:length(origNames)){
    grps <- sub(paste0('^',origNames[i]),i,grps)
  }
  
  DF <- DF[order(DF$y,grps),]
  
  DF$correctX <- ave(DF$x,DF$y,FUN=function(x){
    x[x < 0] <- rev(cumsum(rev(x[x < 0]))) - x[x < 0]/2
    x[x > 0] <- cumsum(x[x > 0]) - x[x > 0]/2
    return(x)
  })
  
  subs <- sub(' Positive$','',DF$groups)
  collapse <- subs[-1] == subs[-length(subs)] & DF$y[-1] == DF$y[-length(DF$y)]
  DF$abs <- abs(DF$x)
  DF$abs[c(collapse,FALSE)] <- DF$abs[c(collapse,FALSE)] + DF$abs[c(FALSE,collapse)]
  DF$correctX[c(collapse,FALSE)] <- 0
  DF <- DF[c(TRUE,!collapse),]
  
  DF$perc <- ave(DF$abs,DF$y,FUN=function(x){x/sum(x) * 100})
  ###
  
  panel.text(x=DF$correctX, y=DF$y, label=paste0(round(DF$perc,1),'%'), cex=0.7)
}


bitenuis <-likdb %>% filter(question == "k_disease") %>%
  spread(answer,n)
bitenuis$question <- NULL
names(bitenuis)[2:5] <- c("None","Can transmit diseases\nbut don't know which one",
                          "Dengue, Chikungunya,\nYellow Fever","Malaria")

bitenuis2<- bitenuis[,c(1,2,5,3,4)]
origNames <- colnames(bitenuis2)

AA <- likert(etnic_group ~ .,bitenuis2, as.percent = TRUE,
       xlim=c(-100,100),xlab="Percentage (%)", 
       main = "What diseases may be transmitted by tiger mosquitoes, if any?",
       auto.key=list(between=1, between.columns=2),
       sub = "", panel=myPanelFunc,ylab="")
AA

pdf(here("output","Kdis.pdf"),width = 6,height = 4)
AA
dev.off()

bitenuis <-likdb %>% filter(question == "k_bitetime") %>%
  spread(answer,n)
bitenuis$question <- NULL
names(bitenuis)[2:4] <- c("Don't know","Early morning or late afternoon","Evening or night")
origNames <- colnames(bitenuis)

BB <- likert(etnic_group ~ .,bitenuis, as.percent = TRUE,
       xlim=c(-100,100),xlab="Percentage (%)", 
       main = "What is the preferred biting time of tiger mosquitoes?",
       auto.key=list(between=1, between.columns=2),
       sub = "", panel=myPanelFunc,ylab="")
BB
pdf(here("output","Ktime.pdf"),width = 6,height = 4)
BB
dev.off()

bitenuis <-likdb %>% filter(question == "a_bite_nuisance") %>%
  spread(answer,n)
bitenuis$question <- NULL
names(bitenuis)[2:4] <- c("Not al all","Neutral/somehow","A lot/ extremely")
origNames <- colnames(bitenuis)

DD <-likert(etnic_group ~ .,bitenuis, as.percent = TRUE,
       xlim=c(-100,100),xlab="Percentage (%)",
       main = "How much do you feel disturbed by tiger mosquitoes bites?",
       sub = "", panel=myPanelFunc,ylab="")
pdf(here("output","Abite.pdf"),width = 6,height = 4)
DD
dev.off()

bitenuis <-likdb %>% filter(question == "a_diseaseworry") %>%
  spread(answer,n)
bitenuis$question <- NULL
names(bitenuis)[2:4] <- c("Never/rarely","Sometime","Very often/always")
origNames <- colnames(bitenuis)

CC <- likert(etnic_group ~ .,bitenuis, as.percent = TRUE,
       xlim=c(-100,100),xlab="Percentage (%)",
       main = "How often are you worried by diseases that Aedes mosquitoes may transmit?",
       sub = "", panel=myPanelFunc,ylab="")
pdf(here("output","Adis.pdf"),width = 6,height = 4)
CC
dev.off()


bitenuis <-likdb %>% filter(question == "a_mosq_nuisance") %>%
  spread(answer,n)
bitenuis$question <- NULL
names(bitenuis)[2:4] <- c("Never/rarely","Sometime","Very often/always")
origNames <- colnames(bitenuis)

EE <- likert(etnic_group ~ .,bitenuis, as.percent = TRUE,
       xlim=c(-100,100),xlab="Percentage (%)",
       main = "How often do you feel disturbed at home by tiger mosquitoes?",
       sub = "", panel=myPanelFunc,ylab="")
pdf(here("output","Admosq.pdf"),width = 6,height = 4)
EE
dev.off()

bitenuis <-likdb %>% filter(question == "p_action") %>%
  spread(answer,n)
bitenuis$question <- NULL
names(bitenuis)[2:5] <- c("Nothing","Environmental repellents","personal repellents","Other")
origNames <- colnames(bitenuis)

FF <- likert(etnic_group ~ .,bitenuis, as.percent = TRUE,
       xlim=c(-100,100),ReferenceZero=1.5,xlab="Percentage (%)",
       main = "what do you do to avoid Aedes bites?",
       sub = "", panel=myPanelFunc,ylab="")
pdf(here("output","pact.pdf"),width = 6,height = 4)
FF
dev.off()


# table2<-db %>%
#   gather(variable,value, k_bitetime, 
#          k_disease, a_diseaseworry, a_bite_nuisance, a_mosq_nuisance, p_action,na.rm=T) %>%
#   group_by(etnic_group,variable,value) %>%
#   summarise (n=n()) %>%
#   filter(!is.na(value)) %>%
#   mutate(freq = n / sum(n)) %>%
#   subset(select=-c(n))%>%
#   spread(etnic_group,freq)

table2<-data.frame(table2[,1:5])



Q_completo_life.cicle.anni.italia$paese_etnia2<-recode(Q_completo_life.cicle.anni.italia$tipo_Q,'keralesi_2012'="india.kerala",'lavoratori_LT'="india.punjab",'pop_gen2012'="italia")

table2b<-Q_completo_life.cicle.anni.italia %>%
  group_by(paese_etnia2) %>%
  summarise(unknown = mean(eggslarvae_unknown),
            water   = mean(eggslarvae_water), 
            grounds  = mean(eggslarvae_ground.herbs.trees), 
            walla   = mean(eggslarvae_walls)) %>%
  gather(key = Where, value = perc,-paese_etnia2)
table2b$paese_etnia2 
table2b$Where <- factor(table2b$Where, 
                        levels = c("water","grounds","walla","unknown"),
                        labels  = c("Small water containers,\nstorm drains","Bare ground,\nvegetation","Walls","Don't know"))

levels(table2b$paese_etnia2 )<- c("Malayalis","Punjabis","Italian")


aa <- Q_completo_life.cicle.anni.italia %>%
  mutate(where = case_when(eggslarvae_water == 1 & eggslarvae_unknown==0 & eggslarvae_walls==0 & eggslarvae_ground.herbs.trees==0 ~ 1,
                           TRUE ~ 0) )%>%
  group_by(paese_etnia2) %>%
  summarise(Correct = mean(where)) %>%
  gather(key = Where, value = perc,-paese_etnia2)

levels(aa$paese_etnia2) <- c("Malayalis","Punjabis","Italian")

  
pdf(here("output","Figure1.pdf"),width = 6,height = 3)
ggplot(table2b, aes(x = paese_etnia2, y = perc*100,fill=Where ))+
  ylim(c(0,100))+ylab("Percentage (%)")+xlab("")+
  geom_bar(stat="identity",width=0.75,position = position_dodge(),col="black")+
  geom_bar(data=aa, aes(x = paese_etnia2, y = perc*100,col="% of correct Answer"), 
           stat="identity",width=0.1,position = position_nudge(x = -0.35),fill="red")+
  theme_bw() + coord_flip()+
  scale_fill_brewer(direction = -1)+
  scale_color_manual(name = "",values="red")+
  ggtitle("Where tiger mosquitoes lay eggs and larvae develop?")
dev.off()  


png(here("output","Figure1.png"),width = 6,height = 3,units = "in",res = 600)
ggplot(table2b, aes(x = paese_etnia2, y = perc*100,fill=Where ))+
  ylim(c(0,100))+ylab("Percentage (%)")+xlab("")+
  geom_bar(stat="identity",width=0.75,position = position_dodge(),col="black")+
  geom_bar(data=aa, aes(x = paese_etnia2, y = perc*100,col="% of correct\nanswer"), 
           stat="identity",width=0.1,position = position_nudge(x = -0.35),fill="red")+
  theme_bw() + coord_flip()+
  scale_fill_brewer(direction = -1)+
  scale_color_manual(name = "",values="red")+
ggtitle("Where tiger mosquitoes lay eggs and larvae develop?")
dev.off() 


Q_completo_life.cicle.anni.italia %>%
  mutate(where = case_when(eggslarvae_water == 1 & eggslarvae_unknown==0 & eggslarvae_walls==0 & eggslarvae_ground.herbs.trees==0 ~ 1,
                           TRUE ~ 0) )%>%
  group_by(paese_etnia2) %>%
  summarise(unknown = mean(eggslarvae_unknown),
            water   = mean(eggslarvae_water), 
            grounds  = mean(eggslarvae_ground.herbs.trees), 
            walla   = mean(eggslarvae_walls),
            correct = mean(where)) %>%
  gather(key = Where, value = perc,-paese_etnia2)



Qife.cicle <- Q_completo_life.cicle.anni.italia %>% 
  mutate(where = case_when(eggslarvae_water == 1 & eggslarvae_unknown==0 & eggslarvae_walls==0 & eggslarvae_ground.herbs.trees==0 ~ 1,
                           TRUE ~ 0) )
tapply(Qife.cicle$where,Qife.cicle$paese_etnia2,mean)




###stat
#univariata
#summary(lm(eta ~ paese_etnia2, data =db))

#db %>%
  #filter (abitazione_tipo=="apartment") %>%
#  summarise(pval = chisq.test(abitazione_tipo, paese_etnia2)$p.value)

#db %>%
#  gather (variable, value, paese_etnia2, sesso_F, livelloeducativo, abitazione_tipo) %>%
#  group_by(variable,value, paese_etnia2) %>% 
#  summarise(pval = chisq.test(variable, paese_etnia2)$p.value)

#library(epiDisplay)
db$etnic_group <- relevel(db$etnic_group, ref="italia")
Q_completo_life.cicle.anni.italia$paese_etnia2 <- factor(Q_completo_life.cicle.anni.italia$paese_etnia2)
Q_completo_life.cicle.anni.italia$paese_etnia2 <- relevel(Q_completo_life.cicle.anni.italia$paese_etnia2, ref="italia")

db <- db %>% 
  mutate(k_lifecycle_bin = 
           case_when(k_lifecycle == "si" ~ 1, 
                     is.na(k_lifecycle) ~ NA_real_,          #se NA mette NA
                     TRUE ~0))                      #tutto il resto diventa no)
db <- db %>% 
  mutate(k_bitetime_bin = 
           case_when(k_bitetime == "2- mattina presto / pomeriggio tardi" ~ 1, 
                                         is.na(k_bitetime) ~ NA_real_,          #se NA mette NA
                     TRUE ~0))                      #tutto il resto diventa no)


db <- db %>% 
  mutate(k_disease_bin = 
           case_when(k_disease == "4- VBD" ~ 1, 
                     k_disease == "3- si generico" ~ 1, 
                     is.na(k_disease) ~ NA_real_,          #se NA mette NA
                     TRUE ~ 0))                      #tutto il resto diventa no)


db <- db %>% 
  mutate(a_diseaseworry_bin = 
           case_when(a_diseaseworry == "3- very often / always" ~ 1, 
                     is.na(a_diseaseworry) ~ NA_real_,          #se NA mette NA
                     TRUE ~ 0))                      #tutto il resto diventa no)

db <- db %>% 
  mutate(a_bite_nuisance_bin = 
           case_when(a_bite_nuisance == "3- molto" ~ 1, 
                     is.na(a_bite_nuisance) ~ NA_real_,          #se NA mette NA
                     TRUE ~ 0))                      #tutto il resto diventa no)

db <- db %>% 
  mutate(a_mosq_nuisance_bin = 
           case_when(a_mosq_nuisance == "3- very often / always" ~ 1, 
                  is.na(a_mosq_nuisance) ~ NA_real_,          #se NA mette NA
                     TRUE ~ 0))                      #tutto il resto diventa no)

db <- db %>% 
  mutate(p_action_bin = 
           case_when(p_action == "1- niente" ~ 0, 
                     p_action == "4- non esco"~ 0,
                     is.na(p_action) ~ NA_real_,          #se NA mette NA
                     TRUE ~ 1))                      #tutto il resto diventa no)


dependent.variables <- c("k_bitetime_bin", "k_disease_bin","a_diseaseworry_bin",
                         "a_bite_nuisance_bin","a_mosq_nuisance_bin","p_action_bin")

m1 <- lapply(dependent.variables, function(dvar) 
  glm(eval(paste0(dvar,' ~ etnic_group+sex+age+edu_level')), data = db,family=binomial))

m1.b<-rbind(
cbind(beta=round(exp(coef(m1[[1]])[2:3]),2),round(exp(confint(m1[[1]])[2:3,]),2),outcome=dependent.variables[1]),
cbind(beta=round(exp(coef(m1[[2]])[2:3]),2),round(exp(confint(m1[[2]])[2:3,]),2),outcome=dependent.variables[2]),
cbind(beta=round(exp(coef(m1[[3]])[2:3]),2),round(exp(confint(m1[[3]])[2:3,]),2),outcome=dependent.variables[3]),
cbind(beta=round(exp(coef(m1[[4]])[2:3]),2),round(exp(confint(m1[[4]])[2:3,]),2),outcome=dependent.variables[4]),
cbind(beta=round(exp(coef(m1[[5]])[2:3]),2),round(exp(confint(m1[[5]])[2:3,]),2),outcome=dependent.variables[5]),
cbind(beta=round(exp(coef(m1[[6]])[2:3]),2),round(exp(confint(m1[[6]])[2:3,]),2),outcome=dependent.variables[6])
)


m1.b





Qife.cicle <- Q_completo_life.cicle.anni.italia %>% 
  mutate(where = case_when(eggslarvae_water == 1 & eggslarvae_unknown==0 & eggslarvae_walls==0 & eggslarvae_ground.herbs.trees==0 ~ 1,
                           TRUE ~ 0) )
tapply(Qife.cicle$where,Qife.cicle$paese_etnia2,mean)

m2<-glm(where~paese_etnia2+sesso_F+eta+livelloeducativo, data=Qife.cicle, family=binomial(link="logit"))
summary(m2)

m2.b<-cbind(beta=round(exp(coef(m2)[2:3]),2),round(exp(confint(m2)[2:3,]),2),outcome="eggslarvae_water")

table3<-data.frame(rbind(m1.b,m2.b))
table3.a<-table3[seq(1, nrow(table3), 2),]
colnames(table3.a)<-c("kerala_beta","kerala_lw","kerala_up","outcome")

table3.b<-table3[seq(2, nrow(table3), 2), ]
colnames(table3.b)<-c("punjab_beta","punjab_lw","punjab_up","outcome")
table3.c<-cbind(table3.a[,c("outcome","kerala_beta","kerala_lw","kerala_up")],table3.b[,1:3])
row.names(table3.c)<-NULL

table3<-table3.c 

rm(table3.a,table3.b, table3.c, m1,m1.b, m2, m2.b,  dependent.variables)

table3
write.table(table3,here("output","table3.csv"),sep="|",row.names =F,quote=F)



#####predictors of nuisance
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
theme_set(theme_sjplot())

db$Group <- factor(db$etnic_group,levels = levels(db$etnic_group),
                   labels = c("Italians","Mayalalis","Punjabis"))

set_label(db$a_mosq_nuisance_bin) <- "How much do you feel disturbed at home by tiger mosquito?"
m3 <- glm(a_mosq_nuisance_bin ~ Group+sex+age+edu_level+house_type+house_floor+house_out_space+green_area+house_window_screen, data = db,family=binomial)

summary(m3)
anova(m3, test="LRT")

#png("NuisKAP.png",width = 7,height = 7,units = "in",res = 600)
pdf("Figure3.pdf",width = 7,height = 7)

plot_model(m3,show.values = TRUE, value.offset = .3)

dev.off()

###modelli stratificati per sesso

#male
dependent.variables <- c("k_lifecycle_bin", "k_bitetime_bin", "k_disease_bin","a_diseaseworry_bin",
                         "a_bite_nuisance_bin","a_mosq_nuisance_bin","p_action_bin")

m1 <- lapply(dependent.variables, function(dvar) 
  glm(eval(paste0(dvar,' ~ etnic_group+age+edu_level')), data = subset(db,sex=="man"),family=binomial))

m1.b<-rbind(
  cbind(beta=round(exp(coef(m1[[1]])[2:3]),2),round(exp(confint(m1[[1]])[2:3,]),2),outcome=dependent.variables[1]),
  cbind(beta=round(exp(coef(m1[[2]])[2:3]),2),round(exp(confint(m1[[2]])[2:3,]),2),outcome=dependent.variables[2]),
  cbind(beta=round(exp(coef(m1[[3]])[2:3]),2),round(exp(confint(m1[[3]])[2:3,]),2),outcome=dependent.variables[3]),
  cbind(beta=round(exp(coef(m1[[4]])[2:3]),2),round(exp(confint(m1[[4]])[2:3,]),2),outcome=dependent.variables[4]),
  cbind(beta=round(exp(coef(m1[[5]])[2:3]),2),round(exp(confint(m1[[5]])[2:3,]),2),outcome=dependent.variables[5]),
  cbind(beta=round(exp(coef(m1[[6]])[2:3]),2),round(exp(confint(m1[[6]])[2:3,]),2),outcome=dependent.variables[6]),  
  cbind(beta=round(exp(coef(m1[[7]])[2:3]),2),round(exp(confint(m1[[7]])[2:3,]),2),outcome=dependent.variables[7])
)


table3<-data.frame(m1.b)
table3.a<-table3[seq(1, nrow(table3), 2),]
colnames(table3.a)<-c("kerala_beta","kerala_lw","kerala_up","outcome")

table3.b<-table3[seq(2, nrow(table3), 2), ]
colnames(table3.b)<-c("punjab_beta","punjab_lw","punjab_up","outcome")
table3.c<-cbind(table3.a[,c("outcome","kerala_beta","kerala_lw","kerala_up")],table3.b[,1:3])
row.names(table3.c)<-NULL

table3.male<-table3.c 

#female
#male
dependent.variables <- c("k_lifecycle_bin", "k_bitetime_bin", "k_disease_bin","a_diseaseworry_bin",
                         "a_bite_nuisance_bin","a_mosq_nuisance_bin","p_action_bin")

m1 <- lapply(dependent.variables, function(dvar) 
  glm(eval(paste0(dvar,' ~ etnic_group+age+edu_level')), data = subset(db,sex=="woman"),family=binomial))

m1.b<-rbind(
  cbind(beta=round(exp(coef(m1[[1]])[2:3]),2),round(exp(confint(m1[[1]])[2:3,]),2),outcome=dependent.variables[1]),
  cbind(beta=round(exp(coef(m1[[2]])[2:3]),2),round(exp(confint(m1[[2]])[2:3,]),2),outcome=dependent.variables[2]),
  cbind(beta=round(exp(coef(m1[[3]])[2:3]),2),round(exp(confint(m1[[3]])[2:3,]),2),outcome=dependent.variables[3]),
  cbind(beta=round(exp(coef(m1[[4]])[2:3]),2),round(exp(confint(m1[[4]])[2:3,]),2),outcome=dependent.variables[4]),
  cbind(beta=round(exp(coef(m1[[5]])[2:3]),2),round(exp(confint(m1[[5]])[2:3,]),2),outcome=dependent.variables[5]),
  cbind(beta=round(exp(coef(m1[[6]])[2:3]),2),round(exp(confint(m1[[6]])[2:3,]),2),outcome=dependent.variables[6]),  
  cbind(beta=round(exp(coef(m1[[7]])[2:3]),2),round(exp(confint(m1[[7]])[2:3,]),2),outcome=dependent.variables[7])
)


table3<-data.frame(m1.b)
table3.a<-table3[seq(1, nrow(table3), 2),]
colnames(table3.a)<-c("kerala_beta","kerala_lw","kerala_up","outcome")
row.names(table3.a)<-NULL

table3.female<-table3.a 

rm(table3.a,table3.b, table3.c, m1,m1.b, m2, m2.b,  dependent.variables)

