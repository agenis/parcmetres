############################
# ETUDE OPEN DATA PARIS PARCMETRES
#
# script principal
###########################################

# auteur: marc.agenis@gmail.com
# dernières mise à jour: 29/09/2017

# preparation
#############

setwd("D:/stats/R-symbols&scripts (nov 15)/parcmetres")
source("script - packages.R")
source("script - fonctions.R")
source("mes_fonctions_utiles.R", encoding = 'UTF-8', echo=TRUE)
source("script - data.R")

# ANALYSE
#########

# recuperation des donnees meteo
# LFPB bourget # LFPO Orly # numero col 10 si detailes (11 events), 20 si non detailed
bourget.num <- getWeatherForDate("LFPB", "2014-01-01", "2014-12-31", opt_detailed=F, opt_custom_columns=TRUE, custom_columns=20) # precip numerique
bourget     <- getWeatherForDate("LFPB", "2014-01-01", "2014-12-31", opt_detailed=T, opt_custom_columns=TRUE, custom_columns=11) # quali rain
orly        <- getWeatherForDate("LFPO", "2014-01-01", "2014-12-31", opt_detailed=T, opt_custom_columns=TRUE, custom_columns=11) # quali rain
orly.num    <- getWeatherForDate("LFPO", "2014-01-01", "2014-12-31", opt_detailed=F, opt_custom_columns=TRUE, custom_columns=20) # precip numerique
# save(bourget, orly, bourget.num, orly.num, file="raw.meteo.Rdata")
source("script - packages")
# load(file="raw.meteo.Rdata")

bourget %<>% CleanHourlyPrecip
orly    %<>% CleanHourlyPrecip
meteo <- bourget %>% left_join(orly %>% select(Time, precip), by="Time") %>% mutate(precip=(precip.x|precip.y))
table(meteo$precip.x, meteo$precip.y) # autant de pluie sur l'année mais plus etale à orly et plsu concentré à bourget?
c(sum(bourget.num$Precipitationmm), sum(orly.num$Precipitationmm))
table(bourget.num$Precipitationmm>0, orly.num$Precipitationmm>0)

######" metigate # reservoir montsouris
ms=read.csv("synop_07156_2014_decoded.csv", stringsAsFactors = F, sep=',', dec='.', na.strings = c("", "NaN")) 
names(ms)=c("date", "temp", "rosee", "humid", "winddir", "wind", "pression", "nebulos", "nebulos.bas", "nuage.alt", "nuage.type1", "nuage.type2", "nuage.type3", "precip", "precip3", "precip6", "precip12", "temps.100", "temps.passe", "SSS")
ms$date <- ymd_hms(ms$date)
ms[, 14:17] <- ms[, 14:17] %>% zoo::na.fill(., fill=0)
ms.daily <- ms %>% CreateCalendarVariables(1) %>% group_by(.JMA) %>% summarise(Precipitationmm=sum(precip, na.rm=TRUE))
# rajout de variables précipitation transformées


# orly
ms=read.csv("synop_07149_2014_decoded.csv", stringsAsFactors = F, sep=',', dec='.', na.strings = c("", "NaN")) 
names(ms)=c("date", "temp", "rosee", "humid", "winddir", "wind", "pression", "nebulos", "nebulos.bas", "nuage.alt", "nuage.type1", "nuage.type2", "nuage.type3", "precip", "precip3", "precip6", "precip12", "temps.100", "temps.passe", "SSS")
ms$date <- ymd_hms(ms$date)
ms[, 14:17] <- ms[, 14:17] %>% zoo::na.fill(., fill=0)
ms.daily <- ms %>% CreateCalendarVariables(1) %>% group_by(.JMA) %>% summarise(Precipitationmm=sum(precip, na.rm=TRUE))

# comparaison montsouris VS orly
table(orly$precip, ms$precip>0) %>% prop.table %>% pc2# autant de pluie sur l'année mais plus etale à orly et plsu concentré à bourget?
c(sum(ms.daily$Precipitationmm), sum(orly.num$Precipitationmm))
table(ms.daily$Precipitationmm>0, orly.num$Precipitationmm>0)
# comparaison montsouris VS montsouris.infoclimat
c(sum(ms.daily$Precipitationmm), sum.(ms.infoclimat$precip))
table(ms.daily$Precipitationmm>0, ms.infoclimat$precip>0)
PredError(ms.daily$Precipitationmm, ms.infoclimat$precip %>% zoo::na.fill(0), type="MAE")

#verification si delai de correlation? => non
for (i in -3:3){message(i); table(shift(ms.daily$Precipitationmm>0, i, fill=TRUE), orly.num$Precipitationmm>0) %>% print;}
# on teste sur la moyenne des aéroports contre montsouris?
meteo.day <- data.frame(Precipitationmm = .5*(orly.num$Precipitationmm+bourget.num$Precipitationmm))
table(meteo$precip, ms$precip>0) %>% prop.table %>% pc2# autant de pluie sur l'année mais plus etale à orly et plsu concentré à bourget?
c(sum(meteo.day$Precipitationmm), sum(ms.daily$Precipitationmm))
table(meteo.day$Precipitationmm>0, ms.daily$Precipitationmm>0)
# historique de pluie annuelle a orly selon WU
structure(c(383.76, 421.03, 482.47, 374.36, 482.76), .Names = c("2012", "2013", "2014", "2015", "2016"))
# historique orly selon https://www.infoclimat.fr/climatologie/annee/2014/orly-athis-mons/valeurs/07149.html
structure(c(602, 677, 537, 545, 701, 591, 636, 662, 720, 513, 687), .Names = as.character(2006:2016))
# historique montsouris selon https://www.infoclimat.fr/climatologie/annee/2015/paris-montsouris/valeurs/07156.html
structure(c(622, 690, 559, 540, 634, 504, 609, 573, 700, 499, 703), .Names = as.character(2006:2016))
# si on fait le code source:view-source:https://www.infoclimat.fr/climatologie/annee/2014/paris-montsouris/details/07156.html
# historique paris6 selon https://www.infoclimat.fr/climatologie/annee/2015/paris-montsouris/valeurs/07156.html
structure(c(622, 690, 559, 540, 634, 504, 609, 573, 700, 499, 703), .Names = as.character(2006:2016))


# ajout meteo (long! 5 min)
by.hour <- df2 %>% mutate(date=as.Date(debut), heure=hour(debut)) %>% group_by(date, heure, usager) %>% summarise(transactions=n(), montant=sum.(montant))
by.hour.resid = by.hour %>% filter(usager=="resident") %>% mutate(Time=paste0(date, " ", heure, ":00:00") %>% ymd_hms(tz="CET")) %>% left_join(ms, ., by=c("date"="Time"))
by.hour.resid[, c("transactions", "montant")] <- zoo::na.fill(by.hour.resid[, c("transactions", "montant")], fill=0)
by.hour.rotat = by.hour %>% filter(usager=="rotatif")  %>% mutate(Time=paste0(date, " ", heure, ":00:00") %>% ymd_hms(tz="CET")) %>% left_join(ms, ., by=c("date"="Time"))
by.hour.rotat[, c("transactions", "montant")] <- zoo::na.fill(by.hour.rotat[, c("transactions", "montant")], fill=0)
# save(by.hour.resid, by.hour.rotat, file="by.hour.Rdata")
# load(file="by.hour.Rdata")


to.lm <- by.hour.rotat %>% CreateCalendarVariables(1, detailed=TRUE) %>% 
  select(-precip3, -precip6) %>% mutate(precip.sqrt=sqrt(precip), log.precip = log(precip+1), transactions.log = log(1+transactions)) %>%
  mutate(is.aout=.mois==8, is.sam=.joursem=="Saturday", is.dim=.joursem=="Sunday", transactions.sqrt=sqrt(transactions)) %>%
  mutate(precip.ts=CutShortRain(precip, 1)) %>% mutate(precip.ts.ma5=ma(precip.ts, 5)) %>% mutate(is.precip.ts.ma5=precip.ts.ma5>0, precip.ts.ma3=ma(precip.ts, 3)) %>% mutate(is.precip.ts.ma3=precip.ts.ma3>0) %>%
  mutate(precip.m1 = shift(precip, +1, TRUE), precip.m2 = shift(precip, +2, TRUE)) %>% 
  mutate(is.precip = precip>0, is.precip.m1 = precip.m1>0, is.precip.m2 = precip.m2>0, is.precip.ts=precip.ts>0) %>% 
  mutate(precip.ma3 = ma(precip, 3), precip.ma5 = ma(precip, 5), precip.m1.ma3 = ma(precip.m1, 3)) %>%
  mutate(precip.0.2=ifelse(precip>0.2, precip, 0), precip.0.4=ifelse(precip>0.4, precip, 0)) %>%
  mutate(is.precip.0.2 = precip.0.2>0, is.precip.0.4 = precip.0.4>0, precip.0.2.ma3=ma(precip.0.2, 3), precip.0.2.ma5=ma(precip.0.2, 5), precip.0.4.ma5=ma(precip.0.4, 5)) %>%
  mutate(is.precip.ma3 = precip.ma3>0, is.precip.ma5 = precip.ma5>0, is.precip.0.2.ma3=precip.0.2.ma3>0, is.precip.0.2.ma5=precip.0.2.ma5>0, is.precip.0.4.ma5=precip.0.4.ma5>0)
feries = as.Date(c("2014-01-01", "2014-04-21", "2015-05-01", "2014-05-08", "2014-05-29", "2014-06-09", "2014-07-14", "2014-08-15", "2014-11-01", "2014-11-11", "2014-12-25"))
to.lm['ferie'] <- to.lm$.JMA %in% feries
# pics:
pics = as.Date(c("2014-07-08", "2014-07-09", "2014-03-11", "2014-03-12","2014-03-13", "2014-03-17", "2014-03-27", "2014-05-09", "2014-09-24"))
to.lm['pics'] <- to.lm$.JMA %in% pics

to.lm %<>% mutate(precip.ma5 = na.fill(precip.ma5, fill=0),
                  precip.ts.ma5 = na.fill(precip.ts.ma5, fill=0),
                  is.precip.ma5 = na.fill(is.precip.ma5, fill=FALSE),
                  is.precip.ts.ma5 = na.fill(is.precip.ts.ma5, fill=FALSE))
fit=lm(transactions.sqrt ~ (1 + is.sam + is.dim)*.heure*is.aout + .joursem, data=to.lm); summary(fit);anova(fit)
fit=lm(transactions.log ~ (is.sam + is.dim)*(.heure + is.aout) + pics + ferie*.heure, data=to.lm); summary(fit);anova(fit)
plot.ts(to.lm$transactions.log[4500:5000]); fitted(fit)[4500:5000] %>% lines(col='red')



# residuals without precip
to.lm$fitted.without.precip = residuals(fit)
fit2 =lm(fitted.without.precip ~ precip + precip.m1 + precip.m2 + precip.0.2 + precip.ts.ma5 + is.precip.ts.ma5 +
           precip.ma3 + precip.ma5 + precip.m1.ma3 + precip.0.2.ma3 + precip.0.2.ma5 +
           is.precip + is.precip.m1 + is.precip.m2 + is.precip.ma3 + is.precip.ma5 + is.precip.0.2.ma3 +  is.precip.0.2.ma5,
           data=to.lm)
summary(fit2);anova(fit2)
# en step BIC: lm(formula = fitted.without.precip ~ is.precip.ma5 + is.precip.0.2.ma5, data = to.lm)
# let's try regsubset!!!
fit3 = regsubsets(data=to.lm %>% select(contains("precip")), fitted.without.precip~., nvmax=4)
regsubset4X4(fit3)
out=c(precip             = lm(fitted.without.precip~precip, to.lm)  %>% BIC,
      is.precip          = lm(fitted.without.precip~is.precip, to.lm) %>%  BIC,
      precip.m1          = lm(fitted.without.precip~precip.m1, to.lm) %>%  BIC,
      precip.ts          = lm(fitted.without.precip~precip.ts, to.lm) %>%  BIC,
      precip.0.4         = lm(fitted.without.precip~precip.0.4, to.lm) %>%  BIC,
      precip.ma5         = lm(fitted.without.precip~precip.ma5, to.lm) %>%  BIC,
      precip.ts.ma5      = lm(fitted.without.precip~precip.ts.ma5, to.lm) %>%  BIC,
      is.precip.0.4      = lm(fitted.without.precip~is.precip.0.4, to.lm) %>%  BIC,
      is.precip.ts       = lm(fitted.without.precip~is.precip.ts, to.lm) %>%  BIC,
      is.precip.ma5      = lm(fitted.without.precip~is.precip.ma5, to.lm) %>%  BIC,
      is.precip.ts.ma5   = lm(fitted.without.precip~is.precip.ts.ma5, to.lm) %>%  BIC,
      is.precip.m1   = lm(fitted.without.precip~is.precip.m1, to.lm) %>%  BIC)
fit3 = regsubsets(data=to.lm %>% select(contains("precip")), 
                  fitted.without.precip~precip+precip12 +is.precip+precip.m1+precip.ts+precip.0.4+precip.ma5+precip.ts.ma5+is.precip.0.4+is.precip.ts+is.precip.ma5+is.precip.ts.ma5, intercept=FALSE, nvmax=2)

fit=lm(transactions.sqrt ~ .heure*is.aout + .heure*is.sam*is.aout + .heure*is.dim*is.aout + .joursem_num +
         precip, data=to.lm); BIC(fit)

fit1.0=lm(transactions.sqrt ~ .heure*is.aout + .heure*is.sam*is.aout + .heure*is.dim*is.aout + .joursem_num, data=to.lm)
fit1.1=lm(transactions.sqrt ~ .heure*is.aout + .heure*is.sam*is.aout + .heure*is.dim*is.aout + .joursem_num + precip.ts.ma5, data=to.lm); summary(fit1.1);anova(fit1.1)
anova(fit1.0, fit1.1)
fit=lm(transactions.log ~ (is.sam + is.dim)*(.heure + is.aout) + pics + ferie*.heure, data=to.lm); summary(fit);anova(fit)
fit2.1=lm(transactions.log ~ (is.sam + is.dim)*(.heure + is.aout) + pics + ferie*.heure + precip.ts.ma5, data=to.lm); summary(fit2.1);anova(fit2.1)
anova(fit, fit2.1)
plot.ts(to.lm$transactions.sqrt[2000:2500]); fitted(fit1.1)[2000:2500] %>% lines(col='red')

# evaluer le coefficient de la pluie selon la valeur de la prediction, ici en forte charge (jeudi 11h)
fit1.3=lm(transactions.sqrt ~ .heure*is.aout + .heure*is.sam*is.aout + .heure*is.dim*is.aout + .joursem_num + is.precip.ts.ma5, data=to.lm); summary(fit1.1);anova(fit1.1)
predict(fit1.3, newdata=data.frame(transactions.sqrt=median(fit1.1$model$transactions.sqrt), .heure="11", is.aout=F, is.sam=F, is.dim=F, .joursem_num=5, is.precip.ts.ma5=FALSE))^2
predict(fit1.3, newdata=data.frame(transactions.sqrt=median(fit1.1$model$transactions.sqrt), .heure="11", is.aout=F, is.sam=F, is.dim=F, .joursem_num=5, is.precip.ts.ma5=TRUE))^2
# evaluer le coefficient de la pluie selon la valeur de la prediction, ici en faible charge (dimche 6h )
predict(fit1.3, newdata=data.frame(transactions.sqrt=median(fit1.1$model$transactions.sqrt), .heure="6", is.aout=F, is.sam=F, is.dim=T, .joursem_num=1, is.precip.ts.ma5=FALSE))^2
predict(fit1.3, newdata=data.frame(transactions.sqrt=median(fit1.1$model$transactions.sqrt), .heure="6", is.aout=F, is.sam=F, is.dim=T, .joursem_num=1, is.precip.ts.ma5=TRUE))^2

# graphe des paiements rotatifs en semaine, avec ou sans pluie.
toplot <- expand.grid('heure'=factor(0:23), precip=c(TRUE, FALSE)) %>% mutate(predicted=predict(fit, newdata=.))
ggplot(toplot) + aes(x=heure, y=predicted, fill=precip) + geom_bar(stat="identity", position="dodge")
barplot(coef(fit) %>% tail(24))

# etudier si les gens payent des petits montants plutot le matin ou l'aprem?
# = etude du montant moyen payé en rotatif par heure ou quart d'heure de la journée!
by.horod.day

# pour 5 euros peut on avoir un rapport en latex?

#######
# etude des points particuliers et extrema
by.horod <- df2 %>% arrange(id, debut) %>% group_by(id) %>% summarise(montant=sum.(montant), transactions=n(), debut=head(debut, 1)) %>% 
  left_join(mapdata, by="id") %>% na.omit %>%
  mutate(montant_norm = montant/tarifhor*2.4)
by.horod.fin <- df2 %>% arrange(id, debut) %>% group_by(id) %>% summarise(montant=sum.(montant), transactions=n(), debut=head(debut, 1)) %>% 
  left_join(mapdata, by="id") %>% na.omit %>%
  mutate(montant_norm = montant/tarifhor*2.4)

by.horod %>% arrange(transactions) %>% select(1,17,3,4,8,9) %>% head
by.horod %>% arrange(transactions) %>% select(1,17,3,4,8,9) %>% tail
by.horod %>% arrange(montant) %>% select(1,17,3,4,8,9) %>% tail


# repartition et indice de Gini d'inégalité, heure par heure
by.horod.hour <- df2 %>% rhead(1e6) %>% mutate(heure=hour(debut)) %>% group_by(usager, id, heure) %>% summarise(montant=sum.(montant), transactions=n())
out <- NULL; out2 <- NULL; usager1="rotatif"
for (h in 0:23){ # faire idem avec resident ou rotatif
  out  <- by.horod.hour %>% filter(heure==h, usager=="rotatif")  %>% .$transactions %>% Gini(na.rm=TRUE) %>% c(out,  .)
  out2 <- by.horod.hour %>% filter(heure==h, usager=="resident") %>% .$transactions %>% Gini(na.rm=TRUE) %>% c(out2, .)
}
plot.ts(out, main=paste0("Indice de Gini selon l'heure de la journée (",usager1,")"), ylim=c(0.3, 0.7)); lines(out2, col="red")
# indication de la concentration globale
by.horod %>% .$transactions %>% sort %>% cumsum %>% '/'(tail(., 1))

# animatiosn GIF!
load(file="horodateurs-transactions-de-paiement1.Rdata")
mapdata = read.csv2("horodateurs-mobiliers.csv", stringsAsFactors = FALSE) %>% 
  setNames(c("id1", "id", "alim", "modele", "adresse", "arrdt", "regime", "tarif", "zone", "tarif.hor", "geo_shape", "geo_point_2d"))
gparis=readOGR(dsn=getwd(), layer="horodateurs-mobiliers")
mapdata = data.frame(gparis) %>% rename(id=numhoro) %>% mutate(id=as.character(id))
load("by.horod.day.Rdata")

# pour les plots
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn("montant \n normalisé", colours = myPalette(100), limits=c(1, 100))
th <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(), plot.title = element_text(hjust = 1))

# visualisation des zones tarifaires ou types de parcmetres
ggplot(data=mapdata) + aes(x=coords.x1, y=coords.x2, color=factor(modele)) + 
  geom_point(alpha=0.8, size=2.5) + th
# difference selon les jours pour les residents?
by.joursem = df2 %>% mutate(.joursem=wday(debut)) %>% group_by(.joursem, usager) %>% summarise(montant=sum(montant), transactions =n())
by.joursem = df2 %>% mutate(.joursem=wday(debut, label=TRUE)) %>% 
  group_by(.joursem, usager) %>% summarise(montant=sum(montant), transactions =n())


# preparation animation
mydates <- seq.Date(as.Date("2014-01-01"), as.Date("2014-12-31"), by="day")
# plot journalier des paiements
date.i <- ymd_hms("2014-01-01 00:00:00", tz="GMT")
myhours <- seq.POSIXt(from=date.i, to=date.i+24*3600-1, length.out=144)
img1 <- readPNG("pluie.png") %>% grid::rasterGrob(interpolate = TRUE)
img2 <- readPNG("logometigate.png") %>% grid::rasterGrob(interpolate = TRUE)
# join by horodday and meteo
by.horod.day2 <- by.horod.day %>% left_join(ms.daily, by=c("date"=".JMA")) %>% mutate(is.pluie)
saveGIF(ani.height=400, ani.width=567,  movie.name = "montants_normalises_1TRIMESTRE_0.15_800p600_pluie.gif", interval=0.15, expr=
{for (i in 1:100){
  if (wday(mydates[i]) %in% c(1,7)) next 
  temp <- by.horod.day %>% filter(date==mydates[i]) %>% 
    left_join(mapdata, by="id") %>% na.omit %>%
    mutate(montant_norm = montant/tarifhor*2.4) %>% left_join(ms.daily, by=c("date"=".JMA"))
  g <- ggplot(data=temp) + aes(x=coords.x1, y=coords.x2, color=montant_norm) + 
         geom_point(alpha=0.8, size=2.5) + sc + th +
         ggtitle( paste(wday(mydates[i], label=T, abbr=F), day(mydates[i]), month(mydates[i], label=T, abbr=F), 2014) ) +
        coord_cartesian(xlim=c(2.25, 2.42), ylim=c(48.82, 48.90)) +
        annotate(geom="text", x=2.27, y=48.819, label="marc.agenis@gmail.com", color="gold1") +
         annotate(geom="text", x=2.41, y=48.90, label=toupper(month(mydates[i], TRUE, FALSE)), size=5) +
         annotation_custom(img2, xmin=2.415, xmax=2.43, ymin=48.817, ymax=48.825)
  if (temp$Precipitationmm[1]>0.4) g <- g + annotation_custom(img1, xmin=2.25, xmax=2.27, ymin=48.89, ymax=48.90) +
    annotate(geom="text", x=2.262, y=48.888, label=paste(temp$Precipitationmm[1], "mm"), size=5, col="grey30")
  plot(g)}
})
# plot horaire pour l'année hors aout et WEEK END

# df2 has times in posixCT
df2 <- df; df2$debut=as.POSIXct(df$debut); rm(df)
df2['quart.heures'] <- align.time(df2$debut, n=600) %>% strftime(format="%H:%M:%S") # 3min!
# save(df2, file="df2.Rdata")
# load(file="df2.Rdata")
by.horod.min <- df2 %>% group_by(usager, id, quart.heures) %>% summarise(montant=mean(montant, na.rm=TRUE), transactions=n())
sc <- scale_colour_gradientn("montant \n normalisé", colours = myPalette(100), limits=c(0, 5))
saveGIF(ani.height=600, ani.width=851,  movie.name = "montants_normalises_10min_800p600.gif", interval=0.2, expr=
{for (i in sort(unique(by.horod.min$quart.heures))){
  temp <- by.horod.min %>% filter(quart.heures==i) %>% 
    left_join(mapdata, by="id") %>% na.omit %>%
    mutate(montant_norm = montant/tarifhor*2.4)
  g <- ggplot(data=temp) + aes(x=coords.x1, y=coords.x2, color=montant_norm) + 
    geom_point(alpha=0.8, size=2.5) + sc + th +
    ggtitle( paste("Etat des paiements à", i, "hors WE et mois d'août") ) +
    coord_cartesian(xlim=c(2.25, 2.42), ylim=c(48.82, 48.90))
  plot(g)}
})
# nombre transactions... (ajouter la legende separement sur le gif?)
# attention : il faut filter que les paiements non résident, la semiane hors vacs aout et feriées
by.horod.min <- df2 %>% group_by(usager, id, quart.heures) %>% summarise(montant=mean(montant, na.rm=TRUE), transactions=n())
sc1 <- scale_colour_gradientn("nombre de \n transactions", colours = myPalette(100), limits=c(1, 100))
sc2 <- scale_colour_gradientn("nombre de \n transactions", colours = myPalette(100), limits=c(1, 25))
saveGIF(ani.height=250, ani.width=650,  movie.name = "transactions_log_10min_650p250_biplot.gif", interval=0.2, expr=
{for (i in sort(unique(by.horod.min$quart.heures))){
  temp <- by.horod.min %>% filter(quart.heures==i) %>% 
    left_join(mapdata, by="id") %>% na.omit
  temp$transactions[temp$transactions > 100] = 100
  g1 <- ggplot(data=temp %>% filter(usager=="resident")) + aes(x=coords.x1, y=coords.x2, color=(transactions)) + 
    geom_point(alpha=0.8, size=1.5) + sc2 + th +
    ggtitle("", subtitle="Nombre de transactions de residents" ) +
    coord_cartesian(xlim=c(2.25, 2.42), ylim=c(48.82, 48.90)) + 
    theme(legend.position="none")
  g2 <- ggplot(data=temp %>% filter(usager=="rotatif")) + aes(x=coords.x1, y=coords.x2, color=(transactions)) + 
    geom_point(alpha=0.8, size=1.5) + sc1 + th +
    ggtitle(i, subtitle="Nombre de transactions en rotatif") +
    coord_cartesian(xlim=c(2.25, 2.42), ylim=c(48.82, 48.90))  + 
    theme(legend.position="none")
  grid.arrange(g1, g2, ncol=2, name=i)
 #  grid.draw(cbind(ggplotGrob(g1), ggplotGrob(g2), size="last")) # not working for GIF
  }
})
ggplot(by.horod.min) + aes(x=quart.heures, y=)
a=by.horod.min %>% group_by(quart.heures) %>% summarise(transactions=n())
plot.ts(a$transactions)

# evolution globale
a.resid=df2 %>% mutate(.mois=month(debut), .joursem=wday(debut)) %>% filter(.mois!=8 & .joursem!="Sunday") %>% group_by(quart.heures) %>% filter(usager=="resident") %>% summarise(transactions=n(), montant.moy=mean.(montant))
a.rotat=df2 %>% mutate(.mois=month(debut), .joursem=wday(debut)) %>% filter(.mois!=8 & .is_we==FALSE) %>% group_by(quart.heures) %>% filter(usager=="rotatif") %>% summarise(transactions=n(), montant.moy=mean.(montant))
par(mfrow=c(2,1))
barplot(a.resid$transactions, col="purple")
barplot(a.rotat$transactions, col="orange") # ajouter legende axe horiz.
par(mfrow=c(1,1))
barplot(a.rotat$montant.moy, col="orange") # bizarre! montants plus petits payés
#a creuser en separant par jour et sans le mois d'aout
by.10min.joursem.usager = df2 %>% mutate(.mois=month(debut), .joursem=wday(debut, label=TRUE)) %>% filter(.mois!=8) %>% 
  group_by(usager, quart.heures, .joursem) %>% summarise(transactions=n(), montant.moy=mean.(montant))
# save(by.10min.joursem.usager, file="by.10min.joursem.usager.Rdata")
# load(fie="by.10min.joursem.usager.Rdata)
by.10min.joursem.usager %>% filter(usager=="resident", .joursem=="Sun") %>% ggplot(.) +
  aes(x=quart.heures, y=transactions) + geom_bar(stat="identity", fill="purple") #+ facet_grid(.joursem~.)
by.10min.joursem.usager %>% filter(usager=="rotatif") %>% ggplot(.) +
  aes(x=quart.heures, y=transactions) + geom_bar(stat="identity", fill="orange") + facet_grid(.joursem~.)
by.10min.joursem.usager %>% filter(usager=="rotatif") %>% ggplot(.) +
  aes(x=quart.heures, y=transactions) + geom_bar(stat="identity", fill="orange")
by.10min.joursem.usager %>% filter(usager=="rotatif") %>% ggplot(.) +
  aes(x=quart.heures, y=montant.moy) + geom_bar(stat="identity", fill="orange") + facet_grid(.joursem~.)
by.10min.joursem.usager %>% filter(usager=="resident", !(.joursem %in% c("Sun", "Sat"))) %>% group_by(quart.heures) %>%
  summarise(transactions=sum(transactions)) %>% .$transactions %>% barplot(col="purple")
by.10min.joursem.usager %>% filter(usager=="resident", !(.joursem %in% c("Sun", "Sat"))) %>% group_by(quart.heures) %>%ggplot(.) +
  aes(x=quart.heures, y=transactions) + geom_bar(stat="identity", fill="purple")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

# etudions si nos deux hypothèses "voiture mal garée" et "diner en ville" sont valides avec les montants normalisés..
diner.en.ville <- df2 %>% filter(usager=="rotatif") %>% group_by(id, quart.heures) %>% 
  summarise(transactions=n(), montant.moy=mean.(montant), montant=sum.(montant)) %>% 
  left_join(mapdata, by="id") %>% na.omit %>% mutate(montant_norm = montant/tarifhor*2.4)
diner.en.ville %>% ungroup %>% group_by(quart.heures, tarifhor) %>% mutate(montant.norm=montant.moy/tarifhor*2.4) %>%
  summarise(montant.moy=mean(montant.moy), montant=sum(montant), transactions=sum(transactions), montant.norm=mean(montant.norm)) %>% 
  ggplot(.) + aes(x=quart.heures, y=transactions, fill=factor(tarifhor)) + geom_bar(stat="identity", position="fill") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(breaks = unique(diner.en.ville$quart.heures)[c(T, rep(F, 5))]) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# pour la modélisation: normalité des variables Y
spat.resid$transactions %>% rhead2(1000) %>% shapiro.test
MASS::boxcox(transactions~1, data=spat.resid) # meilleur 0.1
spat.resid$transactions %>% sqrt %>% rhead2(1000) %T>% hist %>% shapiro.test
# pour rotatif
spat.rotat$montant_norm %>% rhead2(1000) %>% shapiro.test
MASS::boxcox(montant_norm~1, data=spat.rotat) # meilleur 0.1
spat.rotat$montant_norm %>% '^'(0.15) %>% rhead2(1000) %T>% hist %>% shapiro.test

# modélisation 
# effet modele horodateur
fit = lm(data=data.frame(spat.rotat), transactions.sqrt ~ zoneres*alim); anova(fit)
ggplot(spat.rotat %>% data.frame)+aes(x=x.km, y=y.km, color=alim) + geom_point()
fit=lm(data=df2 %>% rhead(100000), log(montant)~paiement); anova(fit)
# effet alim solaire + mois année? heure journée? rien à signaler..
set.seed(1); df2.extract <- df2 %>% rhead(100000)
df2.extract %<>% mutate(.hour=as.character(lubridate::hour(debut)), .mois=as.character(lubridate::month(debut))) %>% group_by(.hour,.mois,id) %>% summarise(transactions=n(), montant=mean.(montant)) %>% left_join(mapdata, by="id") %>% na.omit
fit=lm(data=df2.extract, transactions ~ alim * .hour); summary(fit);anova(fit)
# ratio de paiement
by.paiement = df2 %>% group_by(id, paiement) %>% summarise(n=n()) %>% ungroup %>% 
  group_by(id) %>% summarise(taux.CB=RatioPaiement(n)) %>% left_join(mapdata, by="id") %>% na.omit
ggplot(by.paiement %>% data.frame)+aes(x=coords.x1, y=coords.x2, color=taux.CB) + geom_point()
summary(by.paiement$taux.CB); lm(data=by.paiement, taux.CB ~ factor(arrondt)) %>% summary

# meteo plot annuel
ggplot(data=ms.daily) + aes(x=.JMA, y=Precipitationmm) + geom_bar(stat="identity", fill="lightblue")

# analyse géographique (ya du taf...)
# creation d'une matrice de distance, yeah! ROTATIF
load(file="df2.Rdata")
spat.rotat <- df2 %>% filter(usager=="rotatif") %>% group_by(id) %>% 
  summarise(montant=sum(montant, na.rm=TRUE), transactions=n()) %>% 
  left_join(mapdata, by="id") %>% na.omit %>%
  mutate(montant_norm = montant/tarifhor*2.4) %>%
  mutate(x.km=coords.x1*73-min(coords.x1*73), y.km=coords.x2*111-min(coords.x2*111)) %>%
  mutate(montant_norm.sqrt=sqrt(montant_norm), transactions.sqrt=sqrt(transactions))
spat.resid <- df2 %>% filter(usager=="resident") %>% group_by(id) %>% 
  summarise(montant=sum(montant, na.rm=TRUE), transactions=n()) %>% 
  left_join(mapdata, by="id") %>% na.omit %>%
  mutate(montant_norm = montant/tarifhor*2.4) %>% 
  mutate(x.km=coords.x1*73-min(coords.x1*73), y.km=coords.x2*111-min(coords.x2*111)) %>%
  mutate(montant_norm.sqrt=sqrt(montant_norm), transactions.sqrt=sqrt(transactions))
coordinates(spat.rotat) <- c("x.km", "y.km")
coordinates(spat.resid) <- c("x.km", "y.km")
# save(spat.rotat, spat.resid, file="spat.rotat.Rdata")
# load(file="spat.rotat.Rdata")
# conversion degre - km
# 1 degre latitude  (Y, X2) à paris (48.8) = 111km
# 1 degre longitude (X, X1) a paris (2.3) = 73km

# variogramme
spat.rotat %>% variogram(transactions~1, .) %>% plot(type='b')
spat.resid %>% variogram(transactions~1, .) %>% plot(type='b')
# compared to the total distance (in deg.) of Paris it's about 1/8 of the E-O diameter!
mydist2 <- spat.rotat %>% data.frame %>% select(x.km, y.km) %>% dist(method="manhattan") %>% as.matrix
mydist  <- mydist2 %>% '^'(-1) # passage en valeurs de proximité (fonction inverse)
# on coupe la distance maxi à 1.5km, soit un quantile d'environ 5% => 95% des valeurs seront mises à 0
quantile(as.numeric(mydist2), probs=c(0.001,0.01,0.05,0.1,0.2))
mydist[mydist2 > 1.5] <- 0
mydist[is.infinite(mydist)] <- 0
diag(mydist) <- 0; rm(mydist2)
# save(mydist, file="mydist.Rdata")
# load(file="mydist.Rdata")
# load(file="mydist_resid.Rdata")
w <-  spat.rotat %>% knearneigh(k=7) %>% knn2nb #inutile de plotter la matrice elle n'a aucune structure particulière (id parcmètres aléatoires)
w <-  spat.resid %>% knearneigh(k=7) %>% knn2nb #inutile de plotter la matrice elle n'a aucune structure particulière (id parcmètres aléatoires)
plot(w, coordinates(spat.rotat))

# optimisation de K (nombre de voisins)
out = NULL
for (K in c(1:10,15,20,25,30)){
  out <- c(out, moran.test(spat.resid$transactions, nb2listw(spat.resid %>% knearneigh(k=K) %>% knn2nb))$statistic)}

# construction de la carte des voisins
saveGIF(ani.height=1024, ani.width=1280,  movie.name = "graphe_voisinage_0.5_1280p1024.gif", interval=0.5, expr=
{for (i in 7){plot(spat.rotat %>% knearneigh(k=i) %>% knn2nb, coordinates(spat.rotat))  }})

# méthode 1 - distance euclidienne
Moran.I(spat.rotat$transactions, weight=mydist, scaled=F)
# Méthode 2 - distance 7 voisins
moran.test(spat.rotat$transactions, nb2listw(w))
# I local
spat.rotat$localI=localmoran(spat.rotat$transactions.sqrt, nb2listw(w))[,1]
# spat.rotat$localI=log(spat.rotat$localI+3)
(mylimits = range(spat.rotat$localI))
sc <- scale_colour_gradientn("I de moran \n local", colours = myPalette(100), limits=mylimits)
ggplot(data=spat.rotat %>% data.frame) + aes(x=x.km, y=y.km, color=localI) + 
  geom_point(alpha=0.8, size=2.5) + sc + th +
  coord_cartesian(xlim=c(0, 12), ylim=c(0, 9.5))
#scaterplot
moran.plot(spat.rotat$transactions, nb2listw(w))

# residus
ols <- lm(data=spat.resid, transactions.sqrt ~ tarif + alim + factor(arrondt))
spat.resid$ols.residuals = residuals(ols)
ols <- lm(data=spat.rotat, transactions.sqrt ~ tarif + alim + factor(arrondt))
spat.rotat$ols.residuals = residuals(ols)
load(file="mydist.Rdata")
# méthode 1 - distance euclidienne
Moran.I(spat.rotat$ols.residuals, weight=mydist, scaled=F)
# Méthode 2 - distance 7 voisins
moran.test(spat.rotat$ols.residuals, nb2listw(w))
load(file="mydist_resid.Rdata")
Moran.I(spat.resid$ols.residuals, weight=mydist, scaled=F)
moran.test(spat.resid$ols.residuals, nb2listw(spat.resid %>% knearneigh(k=7) %>% knn2nb))


summary(ols);anova(ols)
SAR_ML0 <- lagsarlm(montant_norm~1,                    data=to.lm2, listw=nb2listw(w), type = "lag" )
SAR_ML1 <- lagsarlm(montant_norm~tarif+tarif:is.matin, data=to.lm2, listw=nb2listw(w), type = "lag" )
anova(SAR_ML0, SAR_ML1)

summary(LMtests <- lm.LMtests(ols, listw=nb2listw(w), zero.policy = TRUE, test="all")) 
ols <- lm(data=spat.rotat, montant_norm.sqrt ~ tarif + alim + factor(arrondt))
ols <- lm(data=spat.resid, transactions.sqrt ~ tarif + alim + factor(arrondt))

summary(LMtests <- lm.LMtests(ols, listw=nb2listw(w), zero.policy = TRUE, test="all")) 

#" ca met du temps à fitter"
SEM_ML0 <- errorsarlm(montant_norm~1,     data=extrait, listw=nb2listw(w), etype = "error" )
SEM_ML0 <- errorsarlm(montant_norm~1+factor(arrondt),     data=extrait, listw=nb2listw(w), etype = "error" )
SEM_ML1 <- errorsarlm(transactions.sqrt~tarif+alim+factor(arrondt), data=spat.resid, listw=nb2listw(w), etype = "error" )
SEM_ML0 <- errorsarlm(montant_norm~1+factor(arrondt),     data=extrait, listw=nb2listw(w), etype = "error" )
anova(SEM_ML0, SEM_ML1)
# test moran sur residus
residuals(SEM_ML1)
moran.test(residuals(SEM_ML1), nb2listw(w))

SARMA <- sacsarlm(transactions.sqrt ~ 1, data=spat.rotat, listw=nb2listw(w))

######" metigate
# setwd(paste0(getwd(), "/metigate")) 
# reservoir montsouris
df=read.csv("synop_07156_2014_decoded.csv", stringsAsFactors = F, sep=',', dec='.', na.strings = c("", "NaN")) 
names(df)=c("date", "temp", "rosee", "humid", "winddir", "wind", "pression", "nebulos", "nebulos.bas", "nuage.alt", "nuage.type1", "nuage.type2", "nuage.type3", "precip", "precip3", "precip6", "precip12", "temps.100", "temps.passe", "SSS")
df$date <- ymd_hms(df$date)
df[, 14:17] <- df[, 14:17] %>% zoo::na.fill(., fill=0)


setwd(paste0(getwd(), "/metigate")) #bretigny (?)
dff=read.table("compil_2014_station_SN6.txt", stringsAsFactors = F, sep='\t', dec='.', na.strings = c("", "NaN")) 
names(dff)=c("date", "temp", "wind", "winddir", "precip1", "precip2", "precip3", "solar", "pression", "nebulos", "nebulos.bas", "nebulos.haut", "humid")
dff$precip1 <- c(0, diff(dff$precip1))
dff$precip2 <- c(0, diff(dff$precip2))
dff$precip3 <- c(0, diff(dff$precip3))
dff$date <- ymd_h(dff$date)
dff=head(dff, -2)

c(2.2, 2, 0.2, 2.2, 0, 0, 0, 0.4, 0.4, 0, 0, 0, 0.8, 0.4, 2.4, 
  3.8, 0.8, 1, 0, 1.2, 0, 2.2, 1.8, NA, 0.4, 3.2, 0, 5.4, 1, 0.2, 
  8.6, 3.9, 0, 0, 2.6, 1, 3.2, 1.2, 2, 0, 0, 1, 3.6, 5.6, 2, 0, 
  0, 0, 0.2, 0.2, 4.2, 1.2, 3.6, 0, 0, 2.8, 0, 13.3, 2.4, 0.6, 
  5.2, 1.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0.6, 0.6, 0.6, 0.4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.2, 
  2.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 4.8, 0, 8.9, 
  2.4, 4.8, 2, 0.6, 15.3, 2, 17.6, 2, 0, 0, 0.2, 1.6, 1.2, 3.2, 
  0, 5.8, 3, 3.4, 3.8, 0, 0, 0, 0, 0, 0, 4.6, 24.1, 2, 4.6, 2.2, 
  4.4, 3, 0, 1.2, 0, 0, 0, 0, 0, 7.1, 11.3, NA, 0, 2.6, 17.4, 18.3, 
  11.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0, 11.3, 
  6.3, 0, 0, 0, 0, 2, 0.6, 7.7, 1.4, 24.7, 16.1, 18.6, 0.2, 0.4, 
  13.4, 0, 0, 0, 0, 0, 2.2, 1.2, 0, 0, 0, 0, 0, 0, 7.7, 0, 2, 0, 
  0, 0, 7.5, 5.7, 0, 0, 21.6, 29.1, 5.2, 0.8, 7.3, 3, 0.6, 0, 5.4, 
  0, 0, 0, 0, 0, 0, 0, 3.2, 0.2, 0.2, 12.7, 4.2, 0, 0.6, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 
  2.6, 0.6, 0, 0, 0.6, 0, 0, 0, 0.4, 2, 0, 0.2, 0, 0, 0.2, 0, 7, 
  3.6, 15, 1, 4.3, 0.2, 3.6, 0, 0, 1.2, 5.8, 0.2, 0.2, 0, 0.8, 
  4, 0, 0.2, 0.4, 0.4, 0, 0.2, 0, 0, 0, 0.2, 0, 0.8, 12.9, 0.2, 
  0, 0.8, 2.2, 0, 0.2, 0.2, 0, 0.2, 0, 16.1, 0.2, 9.9, 3.8, 0.4, 
  0, 0, 0.2, 0, 0.2, 0, 5.6, 0, 0, 0, 0, 0.4, 0, 0.4, 0, 0, 0, 
  0, 0.8, 1, 0.4, 0.4, 0.2, 20.8, 6.5, 0, 1.6, 4, 6.3, 0, 5.3, 
  0.2, 0, 0, 0, 1.6, 0.4, 10.1, 0.8, 0, 0, 0, 0)
