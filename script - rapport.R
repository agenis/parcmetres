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


# SCRIPT NOTE DE SYNTHESE
#########################

# plot des regions selon le tarif
ggplot(data=mapdata) + aes(x=coords.x1, y=coords.x2, color=tarif) + 
  geom_point(alpha=0.8, size=2.5) + th

# caractéristiques des equipements
glimpse(mapdata)

# nombre de zones 
unique(mapdata$zoneres)

# pourcentage de parcmètres rotatif seul
mapdata$regime %>% table_pc

# propreté des données
sum(is.na(df2)) # in croyable, 0 sur 24millions!

# valeurs de paiemnet bizarres, fractions de jour pour les residents 4% du temps
df2 %>% rhead(1e6) %>% filter(usager=="resident") %>% mutate(is.fraction = (10*montant)%%1 !=0 ) %>% .$is.fraction %>% table_pc

# nombre de parcmetres selon la source
lapply(list(df2, mapdata), . %>% .$id %>% unique %>% length)

# repartition des transactions ou montants entre residents et usagers rotatifs
repart <- df2 %>% group_by(usager) %>% summarize(montant=sum.(montant), transactions=n())
repart[,-1] %>% sapply(prop.table) %>% pc2
repart %>% reshape2::melt() %>% 
  ggplot(.) + aes(x=variable, y=value, fill=usager) + geom_bar(stat="identity", position="stack") +
  xlab("type de mesure") + ylab("montant en € ou nombre de transactions") + coord_flip()

# montant globaux, par mois, corrigés nombre de jours non fériés
feries <- as.Date(c("2014-01-01", "2014-04-21", "2014-05-01", "2014-05-08", "2014-05-29", "2014-06-09", "2014-07-14", "2014-08-15", "2014-11-01", "2014-11-11", "2014-12-25"))
annee.complete <- seq.Date(as.Date("2014-01-01"), as.Date("2014-12-31"), by="day") %>% data.frame('.JMA'=.) %>% CreateCalendarVariables(1)
annee.complete$ferie = (annee.complete$.JMA %in% feries)|(annee.complete$.joursem=="Sunday")
repart2 <- df2 %>% mutate(mois = month(debut)) %>% group_by(mois) %>% summarize(montant=sum.(montant), transactions=n()) %>%
  mutate(montant=montant/(annee.complete %>% group_by(.mois) %>% summarise(coef=sum(!ferie)) %>% .$coef %>% '/'(mean(.))))
ggplot(repart2) + aes(x=mois, y=montant/1e6) + geom_bar(stat="identity", fill="lightblue", alpha=0.8) +
  xlab("mois de l'année 2014") + ylab("montant collecté, M€")
sum(df2$montant)/1e6 #, M€

# repartition par arrondissement
repart3 <- df2 %>% group_by(id) %>% summarize(montant=sum.(montant), transactions=n()) %>%
  left_join(mapdata, by="id") %>% na.omit
# populations par arrondt: https://fr.wikipedia.org/wiki/Arrondissements_de_Paris
pop.ardt=c(17767,22571,36358,29138,62236,43976,58309,40849,61046,96733,154411,144595,184034,138465,238914,171880,170218,201975,186507,198678)
repart3 %>% group_by(arrondt) %>% summarise(montant=sum(montant)) %>% mutate(montant.prop=montant/pop.ardt) %>% arrange(desc(montant.prop))
repart3 %>% group_by(arrondt) %>% summarise(transactions=sum(transactions)) %>% arrange(desc(transactions))

# evolution par tranche de 15 minute des paiements selon l'usager # on echantillonne qu'une partie pour ce graphe peu precis
a.resid=df2 %>% rhead(1e6) %>% mutate(.mois=month(debut), .joursem=wday(debut)) %>% filter(.mois!=8 & .joursem!="Sunday") %>% group_by(quart.heures) %>% filter(usager=="resident") %>% summarise(transactions=n(), montant.moy=mean.(montant))
a.rotat=df2 %>% rhead(1e6) %>% mutate(.mois=month(debut), .joursem=wday(debut)) %>% filter(.mois!=8 & .joursem!="Sunday") %>% group_by(quart.heures) %>% filter(usager=="rotatif") %>% summarise(transactions=n(), montant.moy=mean.(montant))
par(mfrow=c(2,1))
barplot(a.resid$transactions, col="purple")
barplot(a.rotat$transactions, col="orange") # ajouter legende axe horiz.
par(mfrow=c(1,1))

# classement des parcmetres top et flop, on normalise le montant payé
by.horod <- df2 %>% arrange(id, debut) %>% group_by(id) %>% summarise(montant=sum.(montant), transactions=n(), debut=head(debut, 1)) %>% 
  left_join(mapdata, by="id") %>% na.omit %>%
  mutate(montant_norm = montant/tarifhor*2.4)
by.horod %>% arrange(transactions) %>% select(1,17,3,4,8,9) %>% head
by.horod %>% arrange(transactions) %>% select(1,17,3,4,8,9) %>% tail
by.horod %>% arrange(montant)      %>% select(1,17,3,4,8,9) %>% tail

# evolution du montant moyen rotatif dnas la jorunée
barplot(a.rotat$montant.moy, col="orange") 

# Evolution en graphique dynamique GIF des montants normalisés (au tarif haut)
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

# mesure de la concentration des paiements (Gini)
by.horod.hour <- df2 %>% mutate(heure=hour(debut)) %>% group_by(usager, id, heure) %>% summarise(montant=sum.(montant), transactions=n())
out <- NULL; out2 <- NULL; usager1="rotatif"
for (h in 0:23){ # faire idem avec resident ou rotatif
  out  <- by.horod.hour %>% filter(heure==h, usager=="rotatif")  %>% .$transactions %>% Gini(na.rm=TRUE) %>% c(out,  .)
  out2 <- by.horod.hour %>% filter(heure==h, usager=="resident") %>% .$transactions %>% Gini(na.rm=TRUE) %>% c(out2, .)
}
plot.ts(out, main=paste0("Indice de Gini selon l'heure de la journée (",usager1,")"), ylim=c(0.3, 0.7)); lines(out2, col="red")
# indication de la concentration globale
by.horod %>% .$transactions %>% sort %>% cumsum %>% '/'(tail(., 1))

# METEO
#######

# quelques chiffres sur les données internet WeatherUnderground
# precipitations totales? faux?
c(sum(bourget.num$Precipitationmm), sum(orly.num$Precipitationmm)) %T>% print %>% ecart # et ecart en %
# correspondance des heures de l'année avec ou sans pluie orly vs bourget
table(meteo$precip.x, meteo$precip.y) # autant de pluie sur l'année mais plus etale à orly et plsu concentré à bourget?
# correspondance des precipitations journalières
table(bourget.num$Precipitationmm>0, orly.num$Precipitationmm>0)

# plot meteo données metigate
ggplot(data=ms.daily) + aes(x=.JMA, y=Precipitationmm) + geom_bar(stat="identity", fill="lightblue")

# comparaisons avec données infoclimat et orly/montsouris
# comparaison montsouris VS orly
table(orly$precip, ms$precip>0) %>% prop.table %>% pc2# autant de pluie sur l'année mais plus etale à orly et plsu concentré à bourget?
c(sum(ms.daily$Precipitationmm), sum(orly.num$Precipitationmm))
table(ms.daily$Precipitationmm>0, orly.num$Precipitationmm>0)
# comparaison montsouris VS montsouris.infoclimat
c(sum(ms.daily$Precipitationmm), sum.(ms.infoclimat$precip))
table(ms.daily$Precipitationmm>0, ms.infoclimat$precip>0)
PredError(ms.daily$Precipitationmm, ms.infoclimat$precip %>% zoo::na.fill(0), type="MAE")
summary(ms.daily$Precipitationmm)
# on teste sur la moyenne des aéroports contre montsouris?
meteo.day <- data.frame(Precipitationmm = .5*(orly.num$Precipitationmm+bourget.num$Precipitationmm))
table(meteo$precip, ms$precip>0) %>% prop.table %>% pc2# autant de pluie sur l'année mais plus etale à orly et plsu concentré à bourget?
c(sum(meteo.day$Precipitationmm), sum(ms.daily$Precipitationmm))
table(meteo.day$Precipitationmm>0, ms.daily$Precipitationmm>0)
#verification si delai de correlation? => non
for (i in -3:3){message(i); table(shift(ms.daily$Precipitationmm>0, i, fill=TRUE), orly.num$Precipitationmm>0) %>% print;}
# historique de pluie annuelle a orly selon WU
structure(c(383.76, 421.03, 482.47, 374.36, 482.76), .Names = c("2012", "2013", "2014", "2015", "2016"))
# historique orly selon https://www.infoclimat.fr/climatologie/annee/2014/orly-athis-mons/valeurs/07149.html
structure(c(602, 677, 537, 545, 701, 591, 636, 662, 720, 513, 687), .Names = as.character(2006:2016))
# historique montsouris selon https://www.infoclimat.fr/climatologie/annee/2015/paris-montsouris/valeurs/07156.html
structure(c(622, 690, 559, 540, 634, 504, 609, 573, 700, 499, 703), .Names = as.character(2006:2016))
# si on fait le code source:view-source:https://www.infoclimat.fr/climatologie/annee/2014/paris-montsouris/details/07156.html
# historique paris6 selon https://www.infoclimat.fr/climatologie/annee/2015/paris-montsouris/valeurs/07156.html
structure(c(622, 690, 559, 540, 634, 504, 609, 573, 700, 499, 703), .Names = as.character(2006:2016))

# MODELISATION HORAIRE
######################

# creation de deux tables separees pour matrices de distance
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

# pour la modélisation: normalité des variables Y et transformation Box-Cox
spat.resid %>% rhead2(1000) %>% .$transactions %>% shapiro.test
MASS::boxcox(transactions~1, data=spat.resid) # meilleur 0.1
spat.resid %>% rhead2(1000) %>% .$transactions %>% sqrt %T>% hist %>% shapiro.test
# pour rotatif
spat.rotat %>% rhead2(1000) %>% .$transactions %>% shapiro.test
MASS::boxcox(transactions~1, data=spat.rotat) # meilleur 0.1
spat.rotat %>% rhead2(1000) %>% .$transactions %>% sqrt %T>% hist %>% shapiro.test

# modeles lineaires horaires sans meteo # ajout meteo (long! 5 min)
# voir le script data pour la creation du tableau et des variables transformees
# maintenant on peut modéliser!
fit=lm(transactions.sqrt ~ (1 + is.sam + is.dim)*.heure*is.aout + .joursem, data=to.lm.resid); summary(fit);anova(fit)
plot.ts(to.lm.resid$transactions.sqrt[4500:5000]^2); fitted(fit)[4500:5000]^2 %>% lines(col='red')

# avec les jours de pics d epollution
fit=lm(transactions.log ~ (is.sam + is.dim)*(.heure + is.aout) + pics + ferie*.heure, data=to.lm.rotat); summary(fit);anova(fit)
plot.ts(to.lm.rotat$transactions.log[4500:5000]); fitted(fit)[4500:5000] %>% lines(col='red')

# recuperer les residus de ces modeles pour analyser
to.lm <- to.lm.resid # remplacer avec rotat ensuite
to.lm$fitted.without.precip = residuals(fit)
# on fait une preselection avec la force brute
fit3 = regsubsets(data=to.lm %>% select(contains("precip")), fitted.without.precip~., nvmax=4)
regsubset4X4(fit3)
# les BIC monovariés
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
# incorporation dans le modele initial, on verifir par test Fisher si ca améliore bien
fit1.0=lm(transactions.sqrt ~ .heure*is.aout + .heure*is.sam*is.aout + .heure*is.dim*is.aout + .joursem_num, data=to.lm)
fit1.1=lm(transactions.sqrt ~ .heure*is.aout + .heure*is.sam*is.aout + .heure*is.dim*is.aout + .joursem_num + precip.ts.ma5, data=to.lm); summary(fit1.1);anova(fit1.1)
anova(fit1.0, fit1.1)
# idem sur rotatif (changer le data frame plus haut)
fit=lm(transactions.log ~ (is.sam + is.dim)*(.heure + is.aout) + pics + ferie*.heure, data=to.lm); summary(fit);anova(fit)
fit2.1=lm(transactions.log ~ (is.sam + is.dim)*(.heure + is.aout) + pics + ferie*.heure + precip.ts.ma5, data=to.lm); summary(fit2.1);anova(fit2.1)
anova(fit, fit2.1)
plot.ts(to.lm$transactions.sqrt[2000:2500]); fitted(fit1.1)[2000:2500] %>% lines(col='red')

# effet quantifié de la météo?
# evaluer le coefficient de la pluie selon la valeur de la prediction, ici en forte charge (jeudi 11h)
fit1.3=lm(transactions.sqrt ~ .heure*is.aout + .heure*is.sam*is.aout + .heure*is.dim*is.aout + .joursem_num + is.precip.ts.ma5, data=to.lm); summary(fit1.1);anova(fit1.1)
predict(fit1.3, newdata=data.frame(transactions.sqrt=median(fit1.1$model$transactions.sqrt), .heure="11", is.aout=F, is.sam=F, is.dim=F, .joursem_num=5, is.precip.ts.ma5=FALSE))^2
predict(fit1.3, newdata=data.frame(transactions.sqrt=median(fit1.1$model$transactions.sqrt), .heure="11", is.aout=F, is.sam=F, is.dim=F, .joursem_num=5, is.precip.ts.ma5=TRUE))^2
# evaluer le coefficient de la pluie selon la valeur de la prediction, ici en faible charge (dimche 6h )
predict(fit1.3, newdata=data.frame(transactions.sqrt=median(fit1.1$model$transactions.sqrt), .heure="6", is.aout=F, is.sam=F, is.dim=T, .joursem_num=1, is.precip.ts.ma5=FALSE))^2
predict(fit1.3, newdata=data.frame(transactions.sqrt=median(fit1.1$model$transactions.sqrt), .heure="6", is.aout=F, is.sam=F, is.dim=T, .joursem_num=1, is.precip.ts.ma5=TRUE))^2

# pour finir un gif de chaque jour de l'année avec le llogo meteo ...
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

# MODELE SPATIAL
################


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











