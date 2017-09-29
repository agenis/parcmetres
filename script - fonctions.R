#############################
# ETUDE OPEN DATA PARIS PARCMETRES
###########################################

# auteur: marc.agenis@gmail.com
# dernières mise à jour: 29/09/2017
# FONCTIONS

# nettoyage meteo
CleanHourlyPrecip = function(df){
  df <- df[!duplicated(df$Time), ]
  df %<>% filter(minute(Time)==0)
  return(     seq.POSIXt(from=head(df$Time, 1), to=tail(df$Time, 1), by="hour") %>% 
                data.frame(Time=.) %>% 
                left_join(df, by="Time") %>% 
                CreateCalendarVariables(1) %>% 
                mutate(precip=Events %in% c("Rain", "Snow", "Rain-Thunderstorm", "Rain-Snow"))     )
}

# avoir une table en pourcent
table_pc = . %>% table %>% prop.table %>% pc2

# transformation de variable meteo
CutShortRain = function(x, mymin=1){
  # on doit mettre en entrée un vecteur soit booleen soit numeric
  if (is.numeric(x)) {
    x.logical <- x>0
  }else{
    x.logical <- x
  }
  x.logical[x.logical] <- NA
  x.logical <- zoo::na.spline(x.logical, maxgap = mymin, na.rm=FALSE)
  x[which(!is.na(x.logical))] <- 0
  return(x)
}

# utile
rhead2 = function(data, rows=2) {data[base::sample(NROW(data), rows), ]}

# ratio de paiement par type, fonction one-shot
# ratio de paiement
RatioPaiement = function(x) {x[1]/(x[1]+x[2])}

