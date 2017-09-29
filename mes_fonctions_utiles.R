
##########################################################################
#                F O N C T I O N S    U T I L E S
#                                        by: marc.agenis-nevers@veolia.com
##########################################################################
# derniere mise a jour: 25 janvier 2017

# Execution: tout selectionner puis "Run". Ou faire "Source".

liste <- c("detach.", ".ls.objects", "r", "wd", "import", "wt", "wtn", "rhead", "seqrange", "shift", "rmat", "Show", 
           "pc1", "pc2", "ecart", "is.empty", "fitex", "CV", "+15 fonctions de base avec na.rm=T", 
           "minmax", "sumstats", "almost.equal", "MonthNames", "removeZeros", "removeNAs", "CreateRunGroups", "sign3", "Nullratio", "q_lm", 
           "gg_color_hue", "ggpie", ".ggpie", "ggACF", "TS4X4", "regsubsett4X4", "ggNA", "ggNAadd", 
           "colstr", "where.outliers", "HistoDens", "ggPCA", "ggCA", "lmplots", "lml", "lmlX", "PredError", "CreateCalendarVariables")


# detacher package (entre guillemets) sans renvoyer erreur en cas d'absence du package
detach. = function(pkg) {
  mypos <- match(paste("package", pkg, sep = ":"), search())
  if (!is.na(mypos)) detach(pos=mypos)
}

# Liste tous les objets disponibles dans l'environnement
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# fonction identique a rnorm mais plus rapide a ecrire et avec des arguments par defaut
r = function(n=1, ...) rnorm(n, ...)

# Fonction pour voir si tout les elements d'un vecteur sont differents
all.diff = function(num) {
  if (length(num)<=1) message("attention un seul element dans le vecteur")
  NROW(unique(num))==NROW(num)
}

# Fontion pour ouvrir dans une fenetre le repertoire de travail
wd <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

# Fonction pour importer un csv classique point virgule, decimale (systeme francais). Peut importer automatiquement du bureau.
import = function(file, desktop=FALSE, ...) { # entre guillemets
  path <- file
  if (desktop) {
    path <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop/", file)
  }
  return(read.csv2(path, sep=";", dec=",", header=TRUE, ...)) 
}

# Fonction pour exporter au format csv un data.frame
wt = function(tableau, ...) {
  write.table(tableau, file="output.csv", dec=",", sep=";", col.names=NA, ...)
}

# Fonction pour exporter au format excel un 'N'ouveau data.frame (pour eviter l'overwriting
# d'un precedent ou l'erreur s'il est deja ouvert par exemple)
wtn = function(tableau) {
  filename = paste0("output", round(runif(1)*1E6), ".csv")
  write.table(tableau, file=filename, dec=",", sep=";", col.names=NA)
}

# Fonction analogue de "head" ou "glimpse", avec lignes aleatoires (et seed si besoin)
# mais marche que sur les data.frames
rhead = function(data, rows=7, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  data[base::sample(NROW(data), rows), ]
}

# fonction qui cree une sequence entre les bornes max d'un vecteur
# cree un nombre de valeurs desire (defaut 100) ou par pas de 1 (specifier nb=1)
seqrange=function(x, nb=100, integers=FALSE) {
  if (!integers) {
  out <- seq(from=min(x), to=max(x), length.out=ifelse(nb==1, diff(range(x))+1, nb))
  } else {
  out <- round(min(x)):round(max(x))
  }
  return(out)
}

# fonction qui cree un vecteur retarde (ou differencie) dans un sens ou dans l'autre
# fill argument pour remplir les trous crees avec la premiere ou derniere valeur
shift = function(x, lag, fill=FALSE) {
  require(dplyr)
  switch(sign(lag)/2+1.5, 
         lead( x, n=abs(lag), default=switch(fill+1, NA, tail(x, 1))  ), 
         lag(  x, n=abs(lag), default=switch(fill+1, NA, head(x, 1))  )
  )
}


# cree une matrice aleatoire carree de dimension choisie, peuplee par une distribution gaussienne. 
# Option df=TRUE pour creer un data.frame
rmat = function(cote=10, df=FALSE, seed=NULL, ...) {
  if (!is.null(seed)) {set.seed(seed)}
  temp <- matrix(rnorm(cote^2, ...), ncol=cote)
  if (df) {
    temp <- as.data.frame(temp)
  }
  return(temp)
}

# Fonction pour afficher une matrice sous forme d'image, **dans le bon sens!**
Show = function(df, ...) {image(t(df[nrow(df):1,]), ...)}

# Fonction pour multiplier par 100 et garder une decimale (pour les resultats en pourcentage dnas les rapports)
pc1 = function(vector) {
  return(round(vector*100, 1))
}

# Fonction pour multiplier par 100 et garder DEUX decimales (pour les resultats en pourcentage dnas les rapports)
pc2 = function(vector) {
  return(round(vector*100, 2))
}

# Fonction pour calculer l'ecart entre deux nombres ou vecteurs, le premier etant la reference
# l'argument "pc" renvoie le resultat en pourcentage
# cette fonction peut prendre un vecteur de deux elements en input et renvoie l'ecart
# entre le premier et le second
ecart = function(x1, x2=NULL, pc=TRUE){
  if (is.null(x2)){
    message("first two elements used")
    res=(x1[2] - x1[1])/x1[1]*100^pc
  } else {
    res=(x2 - x1)/x1*100^pc 
  }
  return(res)
}

# Fonction pour tester si un objet est vide (vecteur ou data.frame)
is.empty = function (input) {
  df <- data.frame(input)
  (is.null(df) || nrow(df) == 0 || ncol(df) == 0 || NROW(df) == 0)
}

# fonction pour generer rapidement un modele lineaire simple, ou bivarie
# il apparait dans l'environnement sous le nom "fit."
fitex = function(covariates=1){
  if (covariates<=1) {fit. <- lm(mpg~hp, mtcars)} else {fit. <- lm(mpg~hp+wt, mtcars)}
  fit.<<- fit.
  return(fit.)
}

# Coefficient de variation
CV = function(x, ...) {sd(x, ...)/mean(x, ...)}

# Indicateurs statistiques classiques sans na.rm=TRUE
mean.   = function(x) mean(x, na.rm=T)
sum.    = function(x) sum(x, na.rm=T)
median. = function(x) median(x, na.rm=T)
IQR.    = function(x) IQR(x, na.rm=T)
sd.     = function(x) sd(x, na.rm=T)
var.    = function(x) var(x, na.rm=T)
range.  = function(x) range(x, na.rm=T)
prod.   = function(x) prod(x, na.rm=T)
CV.     = function(x) CV(x, na.rm=T)
min.    = function(x) min(x, na.rm=T)
max.    = function(x) max(x, na.rm=T)
pmin.   = function(x) pmin(x, na.rm=T)
pmax.   = function(x) pmax(x, na.rm=T)
weighted.mean.= function(x) weighted.mean(x, na.rm=T)

# fonction pour normaliser min/max un data.frame (R->[0;1]), attention toutes colonnes doivent etre numeriques
minmax <- function(data, ...) {
  .minmax = function(x) (x-min(x, ...))/(max(x, ...)-min(x, ...))
  # find constant columns, replaces with O.5:
  constant <- which(apply(data, 2, function(u) {min(u, ...)==max(u, ...)}))
  if(is.vector(data)) {
    res <- .minmax(data)
  } else {
    res <- apply(data, 2, .minmax)
  }
  res[, constant] <- 0.5
  return(res)
}

# Resume statistique sur n'importe quel data.Frame avec au moins une colonne (et avec l'option na.rm=T)
sumstats=function(x) {
  mean.k=function(x) {if (is.numeric(x)) round(mean.(x), digits = 2)
    else "N*N"}
  median.k=function(x) {  if (is.numeric(x)) round(median.(x), digits = 2)
    else "N*N"}
  sd.k=function(x) {  if (is.numeric(x)) round(sd.(x), digits = 2)
    else "N*N"}
  cv.k=function(x) {  if (is.numeric(x)) round(CV.(x), digits = 2)
    else "N*N"}
  min.k=function(x) {  if (is.numeric(x)) round(min.(x), digits = 2)
    else "N*N"}
  max.k=function(x) {  if (is.numeric(x)) round(max.(x), digits = 2)
    else "N*N"}
  sumtable <- cbind(as.matrix(colSums(!is.na(x))), sapply(x,mean.k), sapply(x,median.k), sapply(x,sd.k),  sapply(x,cv.k), sapply(x,min.k), sapply(x,max.k))
  sumtable <- as.data.frame(sumtable);  names(sumtable) <- c("N.obs","Moy","Med","sd","CV", "min","max")
  return(sumtable)
}

# fonction pour verifier si deux nombres sont quasiument egaux (a la precision machine pres)
# typiquement pour verifier l'egalite entre un nombre et le meme nombre issu d'un format charactere
almost.equal <- function (x, y, tolerance=.Machine$double.eps^0.5,
                          na.value=TRUE)
{
  answer <- rep(na.value, length(x))
  test <- !is.na(x)
  answer[test] <- abs(x[test] - y) < tolerance
  answer
}

# Fonction qui donne les noms des mois en francais ou anglais (sans)
MonthNames = function(langue=1){
  switch(langue, 
         c("janvier", "fevrier", "mars", "avril", "mai", "juin", "juillet", "aout", "septembre", "octobre", "novembre", "decembre"),
         c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
}

# Fonction pour enlever les colonnes avec trop de zeros parmi les numeriques. 
# Proportion limite: entre zero et un.
RemoveZeros = function(data, proportion=0.00){
  temp <- data
  NullRatio = function(x) {length(x[x==0])/length(x)}
  to_keep <- which(apply(temp, 2, NullRatio) <= proportion)
  message("colonnes supprimees: ", paste0(names(temp)[-to_keep], collapse=", "))
  return(temp[, to_keep])
}

# Fonction pour enlever les colonnes avec trop de NAs parmi les numeriques. 
# Proportion limite: entre zero et un.
RemoveNAs = function(data, proportion=1.00){
  temp <- data
  to_keep <- which(apply(temp, 2, function(x) sum(!complete.cases(x))/NROW(x)) <= proportion)
  message("colonnes supprimees: ", paste0(names(temp)[-to_keep], collapse=", "))
  return(temp[, to_keep])
}

# Fonction qui cree un vecteur d'appartenance a un groupe, a partir de la position
# des limites de groupes (extremites comprises). ON peut preciser des noms de niveaux.
CreateRunGroups = function(limits, factor=F, ...) { # warning: the limits must have 0 first, and length(X) as last
  breaks <- diff(limits)
  RunGroup <- unlist(sapply(c(1:length(breaks)), function(i){
    rep(i,breaks[i])
  }))
  if (factor) {RunGroup <- factor(RunGroup, ...)}
  return(RunGroup)
}

# fonction qui formatte les nombres pour X chiffres significatifs
# Formatter les nombres à X chiffres significatifs, par defaut 3, en point ou virgule
sign3 = function(vector, chiffres=3, virgule=FALSE) {
  temp = formatC(signif(vector,digits=chiffres), digits=chiffres, format="fg")
  if (virgule) {temp <- gsub("\\.", ",", temp) }
  return(temp)  
}

# pourcentage de valeurs nulles dans une colonne
NullRatio = function(x) {
  length(na.omit(x)[na.omit(x)==0])/length(x)
  }
NullRatio. = function(x) sapply(x, NullRatio)

#############################
# FONCTIONS DE PLOT (ggplot2)

# quick plot de points et une droite avec IC
q_lm = function(data, y, x, ...) { # pas de guillemets
  require(ggplot2)
  X <- deparse(substitute(x))
  Y <- deparse(substitute(y))
  fit <- lm(data[[Y]] ~ data[[X]])
  g   <- ggplot(data) + aes_string(y=Y, x=X) + geom_point(alpha=0.6, size=4, ...) + geom_smooth(method="lm")
  print(anova(fit))
  print(paste0(rep("-", 50), collapse=""))
  print(summary(fit))
  return(g)
}

# emuler les couleurs ggplot, renvoie un vecteur de N couleurs
gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Creation de graphiques camembert (a partir d'un tableau de contingence)
ggpie = function (dat, by, totals, legend="ma variable", title="graphique camembert", palette=3) {
  require(ggplot2)
  g <<- ggplot(dat, aes_string(x=factor(1), y=totals, fill=by)) +
    geom_bar(stat='identity', color='black') +
    guides(fill=guide_legend(override.aes=list(colour=NA))) + # removes black borders from legend
    coord_polar(theta='y') +
    theme(axis.ticks=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_text(colour='black'),
          axis.title=element_blank()) +
    scale_fill_brewer(palette=palette, name=paste(legend))  +
    ggtitle(paste(title)) +
    scale_y_continuous(breaks=cumsum(dat[[totals]]) - dat[[totals]] / 2, labels=dat[[by]])
  print(g)
}

# Creation de graphiques camembert (a partir d'un tableau de donnees a deux colonnes)
.ggpie = function(data, variable, ...){
  require(ggplot2)
  ggpie <- function (dat, by, totals, legend="ma variable", title="graphique camembert", palette=3) {
    g <<- ggplot(dat, aes_string(x=factor(1), y=totals, fill=by)) +
      geom_bar(stat='identity', color='black') +
      guides(fill=guide_legend(override.aes=list(colour=NA))) + # removes black borders from legend
      coord_polar(theta='y') +
      theme(axis.ticks=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x=element_text(colour='black'),
            axis.title=element_blank()) +
      scale_fill_brewer(palette=palette, name=paste(legend))  +
      ggtitle(paste(title)) +
      scale_y_continuous(breaks=cumsum(dat[[totals]]) - dat[[totals]] / 2, labels=dat[[by]])
    print(g)
  }
  dat <- data.frame(table(data[variable]))
  ggpie(dat, 'Var1', 'Freq',...)
}

# Graphes ACP et PACF
ggACF = function(x){
  require(ggplot2)
  require(gridExtra)
  TS <- x
  bacf    <- acf(TS, plot = FALSE)
  bacfdf <<- with(bacf, data.frame(lag, acf, n.used))
  bacfdf  <- with(bacf, data.frame(lag, acf, n.used))
  q1      <- ggplot(data = bacfdf, aes(x = lag, y = acf)) + 
    geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_ribbon(aes(x=lag, ymin=-qnorm(0.975)/sqrt(bacfdf[1,3]), ymax=qnorm(0.975)/sqrt(bacfdf[1,3])), fill="red", alpha=0.2) +
    ggtitle("Auto-correlogramme") + xlab("decalage temporel (rouge: IC95%)") + ylab("correlation") + ylim(-1,1)
  bacf    <- pacf(TS, plot = FALSE)
  bacfdf <<- with(bacf, data.frame(lag, acf, n.used))
  bacfdf  <- with(bacf, data.frame(lag, acf, n.used))
  q2      <- ggplot(data = bacfdf, aes(x = lag, y = acf)) + 
    geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_ribbon(aes(x=lag, ymin=-qnorm(0.975)/sqrt(bacfdf[1,3]), ymax=qnorm(0.975)/sqrt(bacfdf[1,3])), fill="red", alpha=0.2) +
    ggtitle("Auto-correlogramme PARTIEL") + xlab("decalage temporel (rouge: IC95%)") + ylab("correlation") + ylim(-1,1)
  grid.arrange(q1, q2, ncol=2)
}

# Plot 4x4 pour une serie temporelle, la serie doit avoir une saisonnalite et plusieurs annees
TS4X4 = function(time_serie, ...){
  require(ggplot2)
  require(gridExtra)
  df <- time_serie
  df <- data.frame('y'=as.numeric(df), 'season'=cycle(df), 'year'=trunc(time(df)))
  df[c('moy.season', 'moy.year')] <- cbind(ave(df$y, df$season, FUN=mean), ave(df$y, df$year, FUN=mean))
  # construction des 4 plots
  p1 <- ggplot(data=df) + geom_line(aes(x=seq_along(y), y=y)) + xlab("Time") + xlab("serie complete")
  p2 <- ggplot(data=df, aes(x=year, y=y)) + geom_line() + facet_grid(~season) + geom_line(aes(y=moy.season)) + xlab("Evolution de chaque saison avec sa moyenne")
  p3 <- ggplot(data=df, aes(x=season, y=y)) + geom_line(aes(color=factor(year))) + theme(legend.position = "none") + xlab("Evolution de chaque annee selon la saison")
  p4 <- ggplot(data=df) + geom_boxplot(aes(x=factor(year), y=y)) + xlab("boxplot par annee")
  grid.arrange(p1, p3, p2, p4, ...)
}

# selection de variable exhaustive et rendu graphique  complet
# necessite un modele cree avec la fonction regsubsets du package leaps
regsubset4X4=function(modele) {
  require(leaps)
  require(dplyr)
  fit <- summary(modele)
  par(mfrow=c(2, 2))
  # graphe 1
  plot(fit$rss, xlab="nombre de variables", ylab="RSS", type="l")
  points(which.min(fit$rss), fit$rss[which.min(fit$rss)], col="blue", cex=2, pch=20)
  plot(fit$adjr2, xlab="nombre de variables", ylab="R²-adj", type="l")
  points(which.max(fit$adjr2), fit$adjr2[which.max(fit$adjr2)], col="blue", cex=2, pch=20)
  plot(fit$cp, xlab="nombre de variables", ylab="CP", type="l") 
  points(which.min(fit$cp), fit$cp[which.min(fit$cp)], col="blue", cex=2, pch=20)
  plot(fit$bic, xlab="nombre de variables", ylab="BIC", type="l") 
  points(which.min(fit$bic), fit$bic[which.min(fit$bic)], col="blue", cex=2, pch=20)
  # graphe 2
  for (scale in c("r2", "adjr2", "Cp", "bic")) {plot(modele, scale=scale)}
  par(mfrow=c(1, 1))
  formula <- paste0("fluo", " ~ ", paste0(fit$obj$x[tail(fit$which, 1)][-1], " +", collapse=" ")) %>%
    gsub('.{2}$', '',.)
  return(formula)
}

# programme de visualisation et comptage des NA dans un data.frame.
# Alpha: ajuster la transparence des points s'il y en beaucoup.
ggNA = function(data, alpha=0.5){
  require(ggplot2)
  DF <- data
  if (!is.matrix(data)) DF <- as.matrix(DF)
  to.plot <- cbind.data.frame('y'=rep(1:nrow(DF), each=ncol(DF)), 
                              'x'=as.logical(t(is.na(DF)))*rep(1:ncol(DF), nrow(DF)))
  size <- 20 / log( prod(dim(DF)) )  # size of point depend on size of table
  g <- ggplot(data=to.plot) + aes(x,y) +
    geom_point(size=size, color="red", alpha=alpha) +
    scale_y_reverse() + xlim(1,ncol(DF)) +
    ggtitle("location of NAs in the data frame") +
    xlab("columns") + ylab("lines")
  pc <- round(sum(is.na(DF))/prod(dim(DF))*100, 2) # % NA
  print(paste("percentage of NA data: ", pc))
  return(g)
}

# fonction pour ajouter aleatoirement des NA a un tableau. 
# 'quantite' est soit le nombre de cases NA ou la proportion souhaitee
# si la proportion est elevee il y en aura moins car on retombe sur des NA deja crees
ggNAadd = function(data, quantite, plot=T){
  require(ggplot2)
  temp <- data
  quantite2 <- ifelse(quantite<1, round(prod(dim(data))*quantite), quantite)
  if (quantite2 >= prod(dim(data))) stop("trop de NA")
  for (i in 1:quantite2) temp[sample.int(nrow(temp), 1), sample.int(ncol(temp), 1)] <- NA
  if (plot==T) print(ggNA(temp))
  return(temp)
}  

# Fonction pour representer graphiquement la structure d'un tableau ou vecteur.
colstr = function(input, size.max=500, export=FALSE) {
  require(ggplot2)
  require(RColorBrewer)
  require(reshape2)
  data      <- as.data.frame(input)
  if (NCOL(data) == 1) {
    data    <- cbind(data, data)
    message("warning: input data is a vector")
  }
  miror     <- data # construction du miroir (test data.frame avant)
  wholeNA   <- which(sapply(miror, function(x) all(is.na(x))))
  whole0    <- which(sapply(miror, function(x) all(x==0)))
  numeric   <- which(sapply(data, is.numeric))
  character <- which(sapply(data, is.character))
  factor    <- which(sapply(data, is.factor))
  # transformation des caracteres en code 
  miror[character] <- 12 # code caractere
  # transformation des facteurs
  miror[factor] <- 11 # code caractere
  # normalisation minmax sur les donnees numeriques, par colonne:
  if (!is.empty(numeric)) {miror[numeric] <- minmax(miror[numeric], na.rm=T)}
  miror[numeric] <- data.frame(lapply(miror[numeric], function(x) cut(x, breaks=9, labels=1:9))) # 9 classes numeriques
  miror <- data.frame(lapply(miror, as.numeric))
  # conversion des NA
  miror[is.na(data)] <- 10
  miror[whole0]    <- 13
  # construction du vecteur de colorisation
  mypalette <- c(brewer.pal(n=9, name="Blues"), "red", "green", "purple", "grey")
  colnames <- c(paste0((1:9)*10, "%"), "NA", "factor (lvls)", "character", "zero")
  # ggplot
  couper <- nrow(miror) > size.max
  if (couper) miror <- head(miror, size.max)
  NROW.cut <- min(size.max, NROW(input))
  # plot
  g <- ggplot(data=melt(as.matrix(unname(miror)))) + 
    geom_tile(aes(x=Var2, y=Var1, fill=factor(value, levels=1:13))) +
    scale_fill_manual("legende", values=mypalette, labels=colnames, drop=FALSE) +
    ggtitle(paste("representation symbolique de", deparse(substitute(input)), paste(dim(input), collapse="X"), ifelse(couper, "(tronque)", ""))) +
    xlab("colonnes du tableau") + ylab("lignes du tableau") +
    geom_point(data=data.frame(x=0, y=1:NROW.cut), aes(x,y), alpha=1-all(row.names(miror)==seq(1, NROW.cut))) +
    scale_y_reverse()
  if (!is.empty(factor)) {
    g <- g + geom_text(data=data.frame(x     = factor, 
                                       y     = round(runif(length(factor), 2, NROW(miror)-2)), 
                                       label = paste0("(", sapply(data[factor], function(x) length(levels(x))), ")")),
                       aes(x=x, y=y, label=label))
  }
  if (export) {png("colstr_output.png"); print(g); dev.off()}
  return(g)
}

# Fonction qui retourne la position du plus fort outlier (grubbs) colonne par colonne
where.outliers = function(data){
  require(outliers)
  where <- data.frame(x=numeric(0), y=numeric(0))
  for (i in 1:ncol(data)){
    if (is.numeric(data[[i]])) {
      if (grubbs.test(data[[i]])$p.value < 0.05) {
        temp <- which(data[[i]]==as.numeric(strsplit(grubbs.test(data[[i]])$alternative," ")[[1]][3]))
        where   <- rbind(where, c(i, temp))
      }
    }
  }
  names(where)=c('x', 'y')
  return(where)
}

# Fonction pour plotter un histogramme de frequence avec courbe de densite ajustee, option titre et largeur
HistoDens = function(x, intervalles=NULL, titre="histogramme et densite lissee") {
  require(ggplot2)
  g <- qplot(x, geom='blank') + 
    geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') +  
    geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth=intervalles) +                        
    scale_colour_manual(name = 'densite', values="red", label="lissage") +
    xlab("intervalles de la variable") + ylab("frequence & densite") + ggtitle(titre)
  return(g)
}

# La fonction 'ggPCA' permet de tracer des ACP
# avec une variable qualitative illustrative et des ellipses de groupe, et en utilisant ggplot2 
ggPCA = function(DATA, quali=0, axes=12, ncomp=5, makenames=1, title="ACP, data", col=0, plots=c(1:4)){
  require(ggplot2)
  require(FactoMineR)
  require(ellipse)
  require(gridExtra)
  require(ggdendro)
  circle <- function(center = c(0, 0), npoints = 100) {
    r = 1
    tt = seq(0, 2 * pi, length = npoints)
    xx = center[1] + r * cos(tt)
    yy = center[1] + r * sin(tt)
    return(data.frame(x = xx, y = yy))} # necessaire a la suivante
  data <- droplevels(data.frame(DATA))
  if (quali==0) {data <- cbind(data, 'nofactor'=""); quali='nofactor'}
  data[quali] <- as.factor(data[,quali]) # on transforme en facteur la variable choisie
  quali.index <- which(colnames(data)==quali)  # numero de la colonne quali
  nb.levels   <- length(levels(data[, quali])) #nombre de niveaux du facteur
  # verification des couleurs du facteur
  if (col==0) {col=colors()[round(runif(nb.levels, 24, 137))]}
  if (length(col)>nb.levels) {col=col[1:nb.levels]}
  if (length(col)<nb.levels) {col=c(col, colors()[round(runif(nb.levels-length(col), 24, 137))])}
  if (nb.levels==1) {col=colors()[99]}
  CP1 <- as.numeric(substr(axes, 1, 1)) # abscisse
  CP2 <- as.numeric(substr(axes, 2, 2)) # ordonnee
  data.PCA <- data[, sapply(data, is.numeric)] # tableau pour PCA avec quanti seul.
  data.PCA <<- data[, sapply(data, is.numeric)] # tableau pour PCA avec quanti seul.
  if (makenames==1) {row.names(data.PCA)=paste(seq_along(data[, quali]), data[, quali])}
  # PCA core
  res <- PCA(data.PCA, scale.unit=TRUE, ncp=ncomp, graph = F)
  res <<- PCA(data.PCA, scale.unit=TRUE, ncp=ncomp, graph = F) #disponible dehors
  # plot of individuals
  scores <- cbind.data.frame('facteur'=data[, quali], Dim.1=res$ind$coord[, CP1], Dim.2=res$ind$coord[, CP2])
  ellipses.coord <- as.data.frame(coord.ellipse(scores, bary=TRUE))
  scores['names'] <- row.names(data)
  plot1 <- ggplot()+
    geom_hline(yintercept=0, colour="gray65") +
    geom_vline(xintercept = 0, colour = "gray65") +
    geom_point(data=ellipses.coord, aes(x=res.Dim.1, y=res.Dim.2, color=res.facteur), size=1) +
    geom_text(data=scores, aes(x=Dim.1, y=Dim.2, label=names, colour = facteur), alpha = 0.8, size = 4) +
    ggtitle(paste(title, "- plot des individus")) + 
    xlab(paste("Dim.", CP1,"=", round(res$eig[CP1, 2]), "%")) +
    ylab(paste("Dim.", CP2,"=", round(res$eig[CP2, 2]), "%")) +
    scale_color_manual(values=col)
  #Plot des variables  
  corcir = circle(c(0, 0), npoints = 100)
  # create data frame with correlations between variables and PCs
  correlations = cbind.data.frame(Dim.1=res$var$coord[, CP1], Dim.2=res$var$coord[, CP2])
  # data frame with arrows coordinates
  arrows = data.frame(x1 = rep(0, nrow(correlations)), y1 = rep(0, nrow(correlations)), x2 = correlations$Dim.1, y2 = correlations$Dim.2)
  correlations['names'] <- row.names(correlations)
  #ggplot
  plot2 <- ggplot() + 
    geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
    geom_hline(yintercept = 0, colour = "gray65") + 
    geom_vline(xintercept = 0, colour = "gray65") + 
    geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "darkgoldenrod1") + 
    geom_text(data = correlations,size=3, aes(x = Dim.1, y = Dim.2, label = names)) + 
    xlim(-1.05, 1.05) + 
    ylim(-1.05, 1.05) + 
    xlab(paste("Dim.", CP1, "=", round(res$eig[CP1, 2]), "%")) +
    ylab(paste("Dim.", CP2, "=", round(res$eig[CP2, 2]), "%")) +
    ggtitle(paste(title, "- cercle des variables"))
  #master plots
  var <- res$eig[c(1:ncomp),]; names(var) <- make.names(names(var))
  var$comp = as.factor(seq_along(row.names(var)))
  var2 <- var; var2$comp <- as.numeric(var$comp); var2 <- rbind(c(0,0,0,0), var2); var2$comp <- var2$comp+0.5
  plot3 <- ggplot(data=var) + ggtitle("contribution des axes") +
    geom_bar(aes(x=comp, y=percentage.of.variance, fill=percentage.of.variance), stat="identity") +
    geom_line(data=var2, aes(x=comp, y=cumulative.percentage.of.variance), size=3, color="darkgoldenrod1") +
    xlab("numero de la composante")+ylab("% variance totale") + theme(legend.title=element_blank())
  plot4 <- ggdendrogram(hclust(dist(data.PCA)), rotate=T, size=4, theme_dendro=F)+ggtitle("dendrogramme")
  do.call("grid.arrange", c(list(plot3, plot1, plot4, plot2)[plots]))
} 

# ggCA (analyse des correspondances, 2 variables quali)
# entrer un data.frame ou une table de contingence a 2 dimensions
# un data.frame doit comporter au moins deux variables facteurs.
# en cas de disjonction complete, on obtient une erreur.
# axes=12 signifie qu'on represente l'axe 1 et 2.
# le resultat de l'AC est stocke dans la variable "res"
ggCA = function(input, axes=12) {
  require(FactoMineR)
  require(ggplot2)
  # format des donnees
  if ( all(apply(input, 2, is.numeric)) ) {
    factors <- c("colonnes", "lignes")
    data <- input
    message("the initial data.frame was interpreted as a contingency table")
  }
  if ( sum(sapply(input, is.factor))>0 ) {
    factors <- names(input[, sapply(input, is.factor)][, c(1,2)])
    data <- table(input[, sapply(input, is.factor)][, c(1,2)])
    message("only first 2 quali variables processed")
  }
  # axes
  CP1 <- as.numeric(substr(axes, 1, 1)) # abscisse
  CP2 <- as.numeric(substr(axes, 2, 2)) # ordonnee
  # coeur
  res    <- CA(data, graph=FALSE)
  res   <<- res
  # reshape (en ligne la premer facteur, en colonne le second)
  ToPlot <- data.frame(name = c(row.names(res$col$coord), row.names(res$row$coord)),
                       dim1 = c(res$col$coord[, CP1], res$row$coord[, CP1]), 
                       dim2 = c(res$col$coord[, CP2], res$row$coord[, CP2]),
                       type = c(rep(2, length(res$col$coord[, 1])),
                                rep(1, length(res$row$coord[, 1]))),
                       size = c(colSums(data), rowSums(data)))
  xpos   <- with(ToPlot, dim1+diff(range(ToPlot$dim1))/8*(2*type-3))
  ypos   <- with(ToPlot, dim2+diff(range(ToPlot$dim2))/50)
  ToPlot <- cbind(ToPlot, xpos, ypos)
  # plot
  g <- ggplot(data=ToPlot) + 
    geom_hline(yintercept=0, alpha=0.3, linetype=5) + geom_vline(xintercept=0, alpha=0.3, linetype=5) +
    geom_point(aes(x=dim1, y=dim2, size=size, color=factor(type)), alpha=0.5, shape=15) +
    geom_text(aes(x=xpos, y=ypos, label=name, color=factor(type)), size=3) +
    scale_size_continuous("effectif", range=c(1,15)) +
    scale_color_manual("type", values=c("red", "blue"), labels=factors) +
    ggtitle(label=paste("Analyse des correspondances: ", deparse(substitute(input)))) +
    xlab(paste0("axe no", CP1, " a ", round(res$eig[CP1, 2]), "%")) +
    ylab(paste0("axe no", CP2, " a ", round(res$eig[CP2, 2]), "%"))
  # fin
  return(g)
}


# fonction plot avec methode pour objet de classe lm
lmplots = function(model){ 
  
  # packages
  require(ggplot2)
  require(gridExtra)
  
  df <- model$model
  p <- length(model$coef)
  
  # nouvelles variables d'analyse de la regression
  df['predict'] = predict(model, newdata=df)
  df['resid']   = residuals(model)
  df['rstud']   = rstudent(model)
  df['cook']    = cooks.distance(model)
  df['levier']  = lm.influence(model)$hat
  df['indice']  = seq_along(df[,1])
  
  # plot STUDENT selon Yhat
  outliers <- which(abs(df$rstud) > 2)
  to.text  <- df[outliers, ] ; to.text['name'] <- row.names(to.text)
  g1 <- ggplot(data=df) + aes(x=predict, y=rstud) +
    geom_point(size=4, alpha=0.5) + 
    geom_hline(yintercept=2, col="red") +
    geom_hline(yintercept=-2, col="red") +
    geom_point(data=to.text, col="brown1", size=4) +
    geom_text(data=to.text, aes(x=predict, y=rstud+0.4, label=name), col='red', size=4) +
    ggtitle("residus studentises, selon le Y predit")
  
  # plot RESID selon indice
  g2 <- ggplot(data=df) + aes(x=indice, y=resid) +
    geom_point(size=4, alpha=0.5) +
    geom_smooth(col="blue", alpha=0.8,se=FALSE, method='loess') +
    geom_hline(yintercept=0, col="black", "alpha=0.7") +
    ggtitle("residus, dans l'ordre d'apparition")
  
  # plot histrogramme des residus
  HistoDens = function(x, intervalles=NULL, titre="histogramme et densite lissee") {
    g <- qplot(x, geom='blank') + 
      geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') +  
      geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth=intervalles) +                        
      scale_colour_manual(name = 'densite', values="red", label="lissage") +
      xlab("intervalles de la variable") + ylab("frequence & densite") + ggtitle(titre) + theme(legend.position = "none")
    return(g)  }
  g3 <- HistoDens(df$resid, intervalles=diff(range(df$resid/10)), titre="histogramme et densite des residus")
  
  # plot distances de COOK
  cookliers <- which(df$cook > 3*p/NROW(df))
  to.text  <- df[cookliers, ] ; to.text['name'] <- row.names(to.text)
  g4 <- ggplot(data=df) + 
    aes(x=indice, xend=indice, y=0, yend=cook) +
    geom_segment(col='black') +
    geom_hline(yintercept=0.5, col='brown1') +
    geom_hline(yintercept=2*p/NROW(df), col='brown2') +
    geom_hline(yintercept=03*p/NROW(df), col='brown3') +
    ggtitle("distance de cook des residus, par ordre d'apparition") + ylab("cook")
  if (NROW(to.text)>0) {g4 <- g4 + geom_segment(data=to.text, col="brown1") + geom_text(data=to.text, aes(x=indice, y=cook+0.4, label=indice), col='red', size=4)}
  
  grid.arrange(g1, g2, g3, g4)
  
  # return variables
  out <- list('shapiro.p-value'=shapiro.test(df$resid)$p.value,
              'rstudent.outliers'=outliers,
              'cook.outliers.2pn'=cookliers,
              'test.zero.mean'=mean(df$resid),
              'test.sd.student'=sd(df$rstud))
  return(out)
}

# la fonction lml trace la reression d'un modele lineaire simple univarie
# trace egalement la droite de regression, les deux intervalles de confiance et preiction
# ainsi que le leverage de chaque point, seuille a 0.06, et une courbe loess ajustee.
lml = function(model, seuil=0.06, titre="plot des donnees, droite reg, et levier, int.conf&pred"){
  require(ggplot2)
  if (length(model$coef) > 2) {stop("modele multivarie")}
  df <- model$model
  y  <- names(attributes(model$terms)$dataClasses)[1]
  x  <- names(attributes(model$terms)$dataClasses)[2]
  df['levier']  = lm.influence(model)$hat
  df['indice']  = 1:NROW(df)
  df <- cbind(df, predict(model, newdata=df, interval=c("confidence")),
              predict(model, newdata=df, interval=c("prediction"))[, -1])
  names(df)[6:9] <- c("lwr.conf", "upr.conf", "lwr.pred", "upr.pred")
  outleviers <- which(df$levier > seuil)
  # plot
  g <- ggplot(data=df) + aes_string(x=x, y=y) +
    geom_ribbon(aes(ymin=lwr.pred, ymax=upr.pred), fill=colors()[461], alpha=0.1) +
    geom_ribbon(aes(ymin=lwr.conf, ymax=upr.conf), fill=colors()[461], alpha=0.2) +
    geom_smooth(method="loess", se=FALSE, linetype="twodash", col="red", size=0.7) +
    geom_point(aes(size=levier), alpha=0.5) +
    geom_smooth(method="lm", se=FALSE) +
    ggtitle(titre) +
    geom_point(data=df[outleviers, ], aes(size=levier), col="red") +
    scale_size(range = c(2, 8))
  print(g)
  return(list('outliers.levier'=outleviers))
}

# possibilite d'appliquer lml a tout un data.frame
# il fait les regressions sur les variables X une par une
lmlX = function(Y, X, ask=T){
  require(ggplot2)
  par(ask=ask)
  for (i in 1:ncol(X)){
    x <- data.frame(X)[, i]
    lml(model=lm(Y~x), titre=paste("variable X = ", names(X)[i]))
  }
  par(ask=F)
}

# calcule plusieurs types d'erreur de prediction
PredError = function(actual, predicted, type="RMSE", season, ...){
  temp <- na.omit(data.frame(actual, predicted))
  # use the {...} to specify na.rm=TRUE or weightings of means and sums
  # sometimes we force to NA the observations where only one of the vectors has NA.
  # measures return single value except squared error
  if (type %in% c("SE", "SD")){
    # Squared Error
    output <- (actual - predicted)^2
  } else if (type %in% c("MSE", "MSD")){
    # Mean Squared Error
    output <- mean( (actual - predicted)^2, ... )
  } else if (type=="RMSE"){
    # Root Mean Squared Error
    output <- sqrt(   mean( (actual - predicted)^2, ... )   )
  } else if (type=="CV-RMSE"){
    # Coef Variaiton of the RMSE
    output <- sqrt(   mean( (actual - predicted)^2, ... )   )/mean(actual, ...)
  } else if (type=="MBE"){
    # Mean Biased Error
    output <- mean( (predicted - actual), ... )
  } else if (type %in% c("MAE", "MAD")){
    # Mean Absolute Error
    output <- mean( abs(actual - predicted), ... )
  } else if (type=="MCE"){
    # Mean Cubic Error
    output <- mean( (actual - predicted)^3, ... )
  } else if (type=="MACE"){
    # Mean Absolute Cubic Error
    output <- mean(   abs( (actual - predicted)^3 ), ...   )
  } else if (type=="MPE"){
    # Mean Percentage error
    output <- mean( (actual - predicted)/actual, ... )
  } else if (type=="MAPE"){
    # Mean Absolute Percentage error
    output <- mean(   abs( (actual - predicted)/actual ), ...   )
  } else if (type %in% c("sMAPE", "SMAPE", "sMAPE1", "SMAPE1")){
    # Symetric Mean Absolute Percentage Error 1
    output <- 2*mean(   abs( actual - predicted )/( abs(actual)+abs(predicted) )   )
  } else if (type %in% c("sMAPE2", "SMAPE2")){
    # Symetric Mean Absolute Percentage Error 2 (with bias direction)
    actual <- temp$actual; predicted <- temp$predicted
    output <- 2*sum( abs(actual - predicted), ... )/sum(actual + predicted, ...)
  } else if (type %in% c("MAD/MEAN", "MAD-MEAN ratio", "MADMEAN")){
    # Mean Absolute Deviation / Mean
    actual <- temp$actual; predicted <- temp$predicted
    output <- sum( abs(actual - predicted), ... )/sum(actual, ...)
  } else if (type %in% c("MASE", "MASE1")){
    # Mean Absolute Scaled Error (NON SEASONNAL)
    if (as.logical(sum(is.na(c(actual, predicted))))) stop("this method does not allow missing values")
    output <- (NROW(actual)-1)/NROW(actual)*sum(abs(actual-predicted))/sum(abs(diff(actual)) )
  } else if (type %in% c("MASE2", "seasonnal MASE", "sMASE", "SMASE")){
    # Mean Absolute Scaled Error (SEASONNAL)
    if (as.logical(sum(is.na(c(actual, predicted))))) stop("this method does not allow missing values")
    output <- (NROW(actual)-season)/NROW(actual)*sum(abs(actual-predicted))/sum(abs(diff(actual, season)) )
  } else if (type=="GMRAE"){
    # Geometric Mean Relative Absolute Error
    stop("not yet implemented")
  } else if (type %in% c("MdAE", "MDAE")){
    # Median Absolute Error
    output <- median( abs(actual - predicted), ... )
  } else if (type %in% c("MdAPE", "MDAPE", "MdRAE", "MDRAE")){
    # Median Absolute Percentage Error
    output <- median(   abs( (actual - predicted)/actual ), ...   )
  } else if (type=="RMSLE"){
    # Root Mean Squared Logarithmic Error
    output <- sqrt(      mean(   ( log(actual+1) - log(predicted+1) )^2, ...   )      )
  } else {
    stop("unknown type of error asked")
  }
  print(paste("type of error choosen: ", type))
  return(output)
}

# fonction qui ajoute a un tableau des colonnes de variables calendaires pratiques pour la modelisation
# temporelle. Detailed FALSE donne les formats pratiques par defaut, numerique ou facteur selon le cas,
# detailed TRUE donne l'un et l'autre.
CreateCalendarVariables = function(df, id_column=NULL, detailed=FALSE) {
  require(lubridate)
  require(dplyr)
  if (  !("data.frame" %in% class(df))  ){
    message("la donnee en entree a ete convertie en data.frame")
    df <- data.frame(df)
  }
  if (is.null(id_column)) stop("L'argument id_column est obligatoire")
  temp <- df[, id_column]
  if (  !(class(temp)[1] %in% c("Date", "POSIXct", "POSIXt", "POSIXlt"))  ){
    stop("la colonne de dates n'est pas de classe appropriee")
  }
  df['.annee']      <- year(temp)
  df['.trimestre']  <- quarter(temp)
  df['.mois']       <- month(temp)
  df['.semaine']    <- week(temp)
  df['.JMA']        <- as.Date(temp)
  df['.jouran']     <- yday(temp)
  df['.jourmois']   <- mday(temp)
  df['.joursem']    <- wday(temp, label=T, abbr=FALSE) %>% factor(., levels=levels(.)[c(2,3,4,5,6,7,1)])
  df['.is_we']      <- df$.joursem %in% c("Saturday", "Sunday")
  if(class(temp)[1] != "Date"){
    df['.heure']    <- factor(hour(temp))
  }
  if(detailed==TRUE){
    df['.annee_fact']      <- factor(df$.annee)
    df['.trimestre_fact']  <- factor(df$.trimestre)
    df['.mois_fact']       <- factor(df$.mois)
    df['.semaine_fact']    <- factor(df$.semaine)
    df['.jouran_fact']     <- factor(df$.jouran)
    df['.jourmois_fact']   <- factor(df$.jourmois)
    df['.joursem_num']     <- as.numeric(df$.joursem)
    if(class(temp)[1] != "Date"){
      df['.heure_num']    <- as.numeric(df$.heure)
    }
  }
  return(df)
}
# A faire: ajouter la fonction la possibilite de remplir les dates vides par NA?


############################################
#              FIN

message("liste des fonctions chargees:"); print(liste); rm(liste)
