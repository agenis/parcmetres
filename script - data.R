##################################
# ETUDE OPEN DATA PARIS PARCMETRES
##################################

# auteur: marc.agenis@gmail.com
# dernières mise à jour: 29/09/2017
# DATA IMPORT

# la base de données principale s'obtient ici:
# https://opendata.paris.fr/explore/dataset/horodateurs-transactions-de-paiement/export/
# et la base mobilier ici:
# http://opendata.paris.fr/explore/dataset/horodateurs-mobiliers/export/
# le stationnemnet en général
# https://www.paris.fr/services-et-infos-pratiques/deplacements-et-stationnement/stationnement/stationnement-residentiel-mode-d-emploi-2078/

# chargement de la base de donnees complete format R
load(file="df2.Rdata")
# load(file="horodateurs-transactions-de-paiement1.Rdata")
mapdata = read.csv2("horodateurs-mobiliers.csv", stringsAsFactors = FALSE) %>% 
  setNames(c("id1", "id", "alim", "modele", "adresse", "arrdt", "regime", "tarif", "zone", "tarif.hor", "geo_shape", "geo_point_2d"))
gparis=readOGR(dsn=getwd(), layer="horodateurs-mobiliers")
mapdata = data.frame(gparis) %>% rename(id=numhoro) %>% mutate(id=as.character(id))

# pour les representations graphiques: parametres par defaut
# pour les plots
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn("montant \n normalisé", colours = myPalette(100), limits=c(1, 100))
th <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(), plot.title = element_text(hjust = 1))


# données transactions: rajout information quart d'heure et recodage format temporel
# Decoupage en quarts d'heure
# df2$debut=as.POSIXct(df$debut); rm(df)
# df2['quart.heures'] <- align.time(df2$debut, n=600) %>% strftime(format="%H:%M:%S") # 3min!

# # recuperation des donnees meteo
# # LFPB bourget # LFPO Orly # numero col 10 si detailes (11 events), 20 si non detailed
# bourget.num <- getWeatherForDate("LFPB", "2014-01-01", "2014-12-31", opt_detailed=F, opt_custom_columns=TRUE, custom_columns=20) # precip numerique
# bourget     <- getWeatherForDate("LFPB", "2014-01-01", "2014-12-31", opt_detailed=T, opt_custom_columns=TRUE, custom_columns=11) # quali rain
# orly        <- getWeatherForDate("LFPO", "2014-01-01", "2014-12-31", opt_detailed=T, opt_custom_columns=TRUE, custom_columns=11) # quali rain
# orly.num    <- getWeatherForDate("LFPO", "2014-01-01", "2014-12-31", opt_detailed=F, opt_custom_columns=TRUE, custom_columns=20) # precip numerique
load(file="raw.meteo.Rdata")
# nettoyage climatique!
bourget %<>% CleanHourlyPrecip
orly    %<>% CleanHourlyPrecip
meteo <- bourget %>% left_join(orly %>% select(Time, precip), by="Time") %>% mutate(precip=(precip.x|precip.y))
# donnees metigate (montsouris)
######" metigate # reservoir montsouris
ms=read.csv("synop_07156_2014_decoded.csv", stringsAsFactors = F, sep=',', dec='.', na.strings = c("", "NaN")) 
names(ms)=c("date", "temp", "rosee", "humid", "winddir", "wind", "pression", "nebulos", "nebulos.bas", "nuage.alt", "nuage.type1", "nuage.type2", "nuage.type3", "precip", "precip3", "precip6", "precip12", "temps.100", "temps.passe", "SSS")
ms$date <- ymd_hms(ms$date)
ms[, 14:17] <- ms[, 14:17] %>% zoo::na.fill(., fill=0)
ms.daily <- ms %>% CreateCalendarVariables(1) %>% group_by(.JMA) %>% summarise(Precipitationmm=sum(precip, na.rm=TRUE))
# metigate orly
or=read.csv("synop_07149_2014_decoded.csv", stringsAsFactors = F, sep=',', dec='.', na.strings = c("", "NaN")) 
names(or)=c("date", "temp", "rosee", "humid", "winddir", "wind", "pression", "nebulos", "nebulos.bas", "nuage.alt", "nuage.type1", "nuage.type2", "nuage.type3", "precip", "precip3", "precip6", "precip12", "temps.100", "temps.passe", "SSS")
or$date <- ymd_hms(or$date)
or[, 14:17] <- or[, 14:17] %>% zoo::na.fill(., fill=0)
or.daily <- or %>% CreateCalendarVariables(1) %>% group_by(.JMA) %>% summarise(Precipitationmm=sum(precip, na.rm=TRUE))
# source infoclimat
ms.infoclimat= data.frame('precip'=c(2.2, 2, 0.2, 2.2, 0, 0, 0, 0.4, 0.4, 0, 0, 0, 0.8, 0.4, 2.4, 
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
)


by.hour <- df2 %>% mutate(date=as.Date(debut), heure=hour(debut)) %>% group_by(date, heure, usager) %>% summarise(transactions=n(), montant=sum.(montant))
by.hour.resid = by.hour %>% filter(usager=="resident") %>% mutate(Time=paste0(date, " ", heure, ":00:00") %>% ymd_hms(tz="CET")) %>% left_join(ms, ., by=c("date"="Time"))
by.hour.resid[, c("transactions", "montant")] <- zoo::na.fill(by.hour.resid[, c("transactions", "montant")], fill=0)
by.hour.rotat = by.hour %>% filter(usager=="rotatif")  %>% mutate(Time=paste0(date, " ", heure, ":00:00") %>% ymd_hms(tz="CET")) %>% left_join(ms, ., by=c("date"="Time"))
by.hour.rotat[, c("transactions", "montant")] <- zoo::na.fill(by.hour.rotat[, c("transactions", "montant")], fill=0)
# load(file="by.hour.Rdata")
to.lm.rotat <- by.hour.rotat %>% CreateCalendarVariables(1, detailed=TRUE) %>% 
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
to.lm.rotat['ferie'] <- to.lm.rotat$.JMA %in% feries
# pics de pollution:
pics = as.Date(c("2014-07-08", "2014-07-09", "2014-03-11", "2014-03-12","2014-03-13", "2014-03-17", "2014-03-27", "2014-05-09", "2014-09-24"))
to.lm.rotat['pics'] <- to.lm.rotat$.JMA %in% pics
# completion des donnees manquantes de precipitation
to.lm.rotat %<>% mutate(precip.ma5 = na.fill(precip.ma5, fill=0),
                  precip.ts.ma5 = na.fill(precip.ts.ma5, fill=0),
                  is.precip.ma5 = na.fill(is.precip.ma5, fill=FALSE),
                  is.precip.ts.ma5 = na.fill(is.precip.ts.ma5, fill=FALSE))
to.lm.resid <- by.hour.resid %>% CreateCalendarVariables(1, detailed=TRUE) %>% 
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
to.lm.resid['ferie'] <- to.lm.resid$.JMA %in% feries
# pics de pollution:
pics = as.Date(c("2014-07-08", "2014-07-09", "2014-03-11", "2014-03-12","2014-03-13", "2014-03-17", "2014-03-27", "2014-05-09", "2014-09-24"))
to.lm.resid['pics'] <- to.lm.resid$.JMA %in% pics
# completion des donnees manquantes de precipitation
to.lm.resid %<>% mutate(precip.ma5 = na.fill(precip.ma5, fill=0),
                  precip.ts.ma5 = na.fill(precip.ts.ma5, fill=0),
                  is.precip.ma5 = na.fill(is.precip.ma5, fill=FALSE),
                  is.precip.ts.ma5 = na.fill(is.precip.ts.ma5, fill=FALSE))


