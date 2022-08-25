#caricamento dati
setwd(choose.dir(default = "", caption = "WHERE ARE MICRODATA?"))
direttorio = getwd()
fileInput = paste (direttorio, "/Tornadoes_SPC_1950to2015.csv", sep="")
Tornadoes_df <- read.delim2 (fileInput,  header=T, sep=",",  quote="",  na.strings = ".")


###Preprocessing
df_to_use <- Tornadoes_df #copia per non modificare la tabella originale
df_to_use$slat <- as.double(df_to_use$slat) #converto 'slat' in double
df_to_use$slon <- as.double(df_to_use$slon) #converto 'slon' in double
df_to_use$len <-as.double(df_to_use$len) #converto 'len' in double

df_to_use$len <- round((df_to_use$len * 1609.34)/1000,3)#converto miglia in kilometri
df_to_use$wid <- round((df_to_use$wid *0.9144)/1000,3) #converto yard in kilometri

df_to_use <- df_to_use[,c("yr","mo","dy","st","stf","mag","inj","fat","slat","slon","len","wid")] #estrazione delle colonne da studiare




####Statistica descrittiva univariata

###Summary variabili quantitative (Indici di posizione)
summary(df_to_use[,c("yr","mo","dy")]) #info su momento di accadimento del fenomeno
summary(df_to_use[,c("mag","inj","fat","slat","slon","len","wid")]) #info su caratteristiche fenomeno
par(mfrow=c(2,2))
boxplot(df_to_use$yr, names= "Year", main = "Year", col = "yellow",border = "black",horizontal = TRUE)
boxplot(df_to_use$mo, names = "Month", main= "Month",col = "orange",border = "black",horizontal = TRUE)
boxplot(df_to_use$dy, names = "Day", main = "Day", col = "red",border = "black",horizontal = TRUE)
par(mfrow=c(2,2))
boxplot(df_to_use$mag, names = "Magnitude", main = "Magnitude", col = "red",border = "black",horizontal=TRUE)
boxplot(df_to_use$len, names = "Length", main = "Length", col = "orange",border = "black",xlab='Kilometers',horizontal=TRUE)
boxplot(df_to_use$wid, names = "Width", main = "Width", col = "yellow",border = "black",xlab='Kilometers',horizontal=TRUE)
par(mfrow=c(1,2))
boxplot(df_to_use$inj, names = "Injured", main= "Injured",col = "red",border = "black",horizontal=TRUE)
boxplot(df_to_use$fat, names = "Fatality", main= "Fatality",col = "red",border = "black",horizontal=TRUE)
par(mfrow=c(1,2))
boxplot(df_to_use$slat, names = "Starting latitude", main= "Starting latitude",col = "red",border = "black",horizontal=FALSE)
boxplot(df_to_use$slon, names = "Starting longitude", main= "Starting longitude",col = "orange",border = "black",horizontal=TRUE)



###Indici di dispersione
##Varianza
sigma2_2 <- function(x)
{                       
  mean((x-mean(x))^2)
}

##Coefficiente di variazione
cv <- function(x)
{
  sqrt(sigma2_2(x))/abs(mean(x))
}

#yr,mo,dy
var_yr <- sigma2_2(df_to_use$yr)
sd_yr <- sd(df_to_use$yr)
cv_yr <- sd_yr/39   #valore medio standardizzato associando un numero che parte da 1 per ogni anno

var_mo <- sigma2_2(df_to_use$mo)
sd_mo <- sd(df_to_use$mo)
cv_mo <- cv(df_to_use$mo)

var_dy <- sigma2_2(df_to_use$dy)
sd_dy <- sd(df_to_use$dy)
cv_dy <- cv(df_to_use$dy)


var_yr
sd_yr
cv_yr
var_mo
sd_mo
cv_mo
var_dy
sd_dy
cv_dy


#mag,inj,fat,slat,slon,len,wid
var_mag <- sigma2_2(df_to_use$mag)
sd_mag <- sd(df_to_use$mag)
cv_mag <- cv(df_to_use$mag)

var_inj <- sigma2_2(df_to_use$inj)
sd_inj <- sd(df_to_use$inj)
cv_inj <- cv(df_to_use$inj)

var_fat <- sigma2_2(df_to_use$fat)
sd_fat <- sd(df_to_use$fat)
cv_fat <- cv(df_to_use$fat)

var_slat <- sigma2_2(df_to_use$slat)
sd_slat <- sd(df_to_use$slat)
cv_slat <- cv(df_to_use$slat)

var_slon <- sigma2_2(df_to_use$slon)
sd_slon <- sd(df_to_use$slon)
cv_slon <- cv(df_to_use$slon)

var_len <- sigma2_2(df_to_use$len)
sd_len <- sd(df_to_use$len)
cv_len <- cv(df_to_use$len)

var_wid <- sigma2_2(df_to_use$wid)
sd_wid <- sd(df_to_use$wid)
cv_wid <- cv(df_to_use$wid)


var_mag 
sd_mag 
cv_mag
var_inj 
sd_inj 
cv_inj 
var_fat 
sd_fat 
cv_fat 
var_slat  
sd_slat 
cv_slat 
var_slon 
sd_slon 
cv_slon 
var_len 
sd_len 
cv_len 
var_wid 
sd_wid 
cv_wid 
#i valori ottenuti per il cv di 'inj','fat' non sono attendibili perchè la media è approsimabile allo zero.


###Grafici variabili quantitative
##Discrete

#Frequenze assolute
f_yr <- table(df_to_use$yr)
f_mo <- table(df_to_use$mo)
f_dy <- table(df_to_use$dy)

f_mag <- table(df_to_use$mag)
f_inj <- table(df_to_use$inj)
f_fat <- table(df_to_use$fat)
f_slat <- table(df_to_use$slat)
f_slon <- table(df_to_use$slon)
f_len <- table(df_to_use$len)
f_wid <- table(df_to_use$wid)

#Plot
par(mfrow=c(2,2))
plot(f_yr,main= "Number of tornadoes by year",xlab='Year',ylab='Tornadoes',col = "black")
plot(f_mo,main="Number of tornadoes by month",xlab='Month',ylab='Tornadoes',col='black')
plot(f_dy,main="Number of tornadoes by day",xlab='Day',ylab='Tornadoes',col='black')

par(mfrow=c(2,2))
plot(f_mag,main="Number of tornadoes by magnitude",xlab='Magnitude',ylab='Tornadoes',col='black')
plot(f_inj,main="Number of tornadoes by injured",xlab='Injured',ylab='Tornadoes',col='black')
plot(f_fat,main="Number of tornadoes by fatality",xlab='Fataliy',ylab='Tornadoes',col='black')

#Barplot
par(mfrow=c(1,2))
barplot(f_slat,main="Number of tornadoes by start. latitude",xlab='Latitude coordinate',ylab='Tornadoes',col='black')
barplot(f_slon,main="Number of tornadoes by start. longitude",xlab='Longitude coordinate',ylab='Tornadoes',col='black')

par(mfrow=c(1,2))
barplot(f_len,main="Number of tornadoes by length",xlab='Kilometers',ylab='Tornadoes',col='black')
barplot(f_wid,main="Number of tornadoes by width",xlab='Kilometers',ylab='Tornadoes',col='black')





##Grafico variabile qualitativa
#Nominale

#'Stato' come unità statistica
tornado_for_state <- table(df_to_use$st)
tornado_for_state <- as.data.frame(tornado_for_state)
mean_for_state <- mean(tornado_for_state$Freq)
tornado_for_state_ordered <- tornado_for_state[order(tornado_for_state$Freq),]

#Studio delle unità statistiche
tornado_for_state_ordered$Var1[which(tornado_for_state_ordered$Freq < mean_for_state)] 
tornado_for_state_ordered$Var1[which(tornado_for_state_ordered$Freq >=mean_for_state & tornado_for_state_ordered$Freq < 2* mean_for_state) ]
tornado_for_state_ordered$Var1[which(tornado_for_state_ordered$Freq >= 2* mean_for_state)] 

#Preparazione del grafico
#Colori
color_under_mean <-c('yellow','yellow','yellow','yellow',
                     'yellow','yellow','yellow','yellow',
                     'yellow','yellow','yellow','yellow',
                     'yellow','yellow','yellow','yellow',
                     'yellow','yellow','yellow','yellow',
                     'yellow','yellow','yellow','yellow',
                     'yellow','yellow','yellow','yellow',
                     'yellow','yellow','yellow','yellow')

color_between_double_mean <- c('orange','orange','orange','orange',
                               'orange','orange','orange','orange',
                               'orange','orange','orange','orange',
                               'orange')

color_superior_double_mean <- c('red','red','red','red',
                                'red','red','red')

  
  
#labels  
states <-c("DC","AK","RI","PR","HI","VT","DE","NV","NH","CT",
           "OR","WA","UT","ME","WV","NJ","MA","ID","AZ","MD",
           "MT","NY","CA","NM","WY","VA","PA","KY","SC","MI",
           "OH","TN","NC","WI","IN","GA","ND","MN","AR","SD",
           "LA","AL","MS","CO","MO","IL","IA","NE","FL","OK", 
           "KS","TX")



#Barplot
par(mfrow=c(1,1))
barplot(tornado_for_state_ordered$Freq,main='Number of tornadoes by State',xlab='State',ylab='Tornadoes',names.arg=states,col=c(color_under_mean,color_between_double_mean,color_superior_double_mean))
legend("topleft", 
       legend = c("State with Freq. < Mean for State, Mean=1156", "Mean for State <= State with Freq. < 2*Mean for State","State with Freq. >= 2*Mean for State"), # testo della legenda
       pch = 22,                          # tipo di punto
       col = c("black","black","black"),  # bordo dei punti
       pt.bg = c("yellow","orange","red"))# riempimento dei punti
       

####Statistica descrittiva bivariata

###Magnitudine vs data-tempo
#Preparazione al grafico
colors <- heat.colors(6)
colors <- rev(colors)
#Boxplot
par(mfrow=c(2,2))
boxplot(df_to_use$yr ~ df_to_use$mag,main="Magnitude by year",col=colors,horizontal = T,xlab='Year',ylab='Magnitude')
boxplot(df_to_use$mo ~ df_to_use$mag,main="Magnitude by moth",col=colors,horizontal = T,xlab='Month',ylab='Magnitude')
boxplot(df_to_use$dy ~ df_to_use$mag,main="Magnitude by day",col=colors,horizontal = T,xlab='Day',ylab='Magnitude')



###Magnitudine vs stati
##Preparazione al grafico(Ordinamento valori dell'asse x del boxplot)
df_to_use$st <- factor(df_to_use$st , levels=c("AK", "DC", "NV", "OR","PR","ME","AZ","CA","CO","FL","HI",
                                               "ID","KS","MN", "MT","ND","NE","NM","SD","TX","UT",
                                               "WA", "WY","RI", "DE", "GA","IA","IL","LA","MD","MO",
                                               "NC","NJ", "NY", "OH", "PA","SC","VA","WV","VT","CT",
                                               "MA","NH","AR","AL", "IN", "KY","MI","MS","OK",
                                               "TN","WI"))

#Colori
color_state_group_1 <- c("white","white","white","white","white")

color_state_group_2 <- c("#FEFEE9")

color_state_group_3 <-c("yellow","yellow","yellow","yellow","yellow",
                        "yellow","yellow","yellow","yellow","yellow",
                        "yellow","yellow","yellow","yellow","yellow",
                        "yellow","yellow","yellow","yellow","yellow",
                        "yellow","yellow","yellow","yellow","yellow",
                        "yellow","yellow","yellow","yellow","yellow",
                        "yellow","yellow","yellow","yellow")

color_state_group_4 <- c("orange","orange","orange")

color_state_group_5 <-c("#FF681F")

color_state_group_6 <-c("red","red","red","red",
                        "red","red","red","red")



##Boxplot
par(mfrow=c(1,1))
boxplot(df_to_use$mag ~ df_to_use$st,main="Magnitude by State",col=c(color_state_group_1,
                                                                     color_state_group_2,
                                                                     color_state_group_3,
                                                                     color_state_group_4,
                                                                     color_state_group_5,
                                                                     color_state_group_6),horizontal = F,xlab='State',ylab='Magnitude')
legend("topleft", 
       legend = c("State with 0 Mag.","State with 0-1 Mag.", "State with 0-2 Mag.",
                  "State with 0-3 Mag.","State with 0-4 Mag.","State with 0-5 Mag"),    # testo della legenda
       pch = 22,                                                  # tipo di punto
       col = c("black","black","black","black","black","black"),  # bordo dei punti
       pt.bg = c("black","white","yellow","orange","#FF681F","red"), # riempimento dei punti
       cex=0.59)


###Magnitudine vs mortalità
par(mfrow=c(1,2))
boxplot(df_to_use$inj ~ df_to_use$mag,main="Magnitude by injured",col=colors,xlab='Magnitude',ylab='Injured')
boxplot(df_to_use$fat ~ df_to_use$mag,main="Magnitude by fatality",col=colors,xlab='Magnitude',ylab='Fatality')

###Magnitudine vs coordinate di inzio
par(mfrow=c(1,2))
boxplot(df_to_use$slat ~ df_to_use$mag,main="Magnitude by starting lat.",col=colors,xlab='Magnitude',ylab='Starting. latitude')
boxplot(df_to_use$slon ~ df_to_use$mag,main="Magnitude by starting long.",col=colors,xlab='Magnitude',ylab='Starting longitude',horizontal = T)
median(df_to_use$slat)
median(df_to_use$slon)

###Magnitudine vs dimensioni
par(mfrow=c(1,2))
boxplot(df_to_use$len ~ df_to_use$mag,main="Magnitude by tornado's length",col=colors,xlab='Magnitude',ylab='Length')
boxplot(df_to_use$wid ~ df_to_use$mag,main="Magnitude by tornado's width",col=colors,xlab='Magnitude',ylab='Width')



#### Analisi delle dipendenze 

##Test Chiquadro
chisq.test(table(df_to_use$mag,df_to_use$yr)) #info su accadimento fenomeno
chisq.test(table(df_to_use$mag,df_to_use$mo))
chisq.test(table(df_to_use$mag,df_to_use$dy))
chisq.test(table(df_to_use$mag,df_to_use$st))

chisq.test(table(df_to_use$mag,df_to_use$inj)) # caratteristiche fenomeno
chisq.test(table(df_to_use$mag,df_to_use$fat))
chisq.test(table(df_to_use$mag,df_to_use$slat))
chisq.test(table(df_to_use$mag,df_to_use$slon))
chisq.test(table(df_to_use$mag,df_to_use$len))
chisq.test(table(df_to_use$mag,df_to_use$wid))

##Eta^2
install.packages("labstatR")
library("labstatR")
par(mfrow=c(2,2))
eta(df_to_use$yr,df_to_use$mag) #quanto la potenza dipende dall'anno?
eta(df_to_use$mo,df_to_use$mag) #quanto la potenza dipende dal mese?
eta(df_to_use$dy,df_to_use$mag) #quanto la potenza dipende dal giorno?
eta(df_to_use$stf,df_to_use$mag)#quanto la potenza dipende dallo stato? 

eta(df_to_use$inj,df_to_use$mag)#quanto la potenza dipende dal numero di feriti?
eta(df_to_use$fat,df_to_use$mag)#quanto la potenza dipende dal numero di morti?
eta(df_to_use$slat,df_to_use$mag)#quanto la potenza dipende dalla latitudine di inizio?
eta(df_to_use$slon,df_to_use$mag)#quanto la potenza dipende dalla longitudine di inizio?
par(mfrow=c(1,2))
eta(df_to_use$len,df_to_use$mag)#quanto la potenza dipende dal lunghezza del percorso?
eta(df_to_use$wid,df_to_use$mag)#quanto la potenza dipende dalla larghezza?

##Correlazione
matrix_cor <- cor(df_to_use[,c("yr","mo","dy","mag","inj","fat","slat","slon","len","wid")])
matrix_cor <- as.data.frame(matrix_cor)



####Campionamento per il modello statistico della regressione
###Funzione per campione stratificato
stratified <- function(df, group, size, select = NULL, 
                       replace = FALSE, bothSets = FALSE) {
  if (is.null(select)) {
    df <- df
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(df)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select),
                   function(x) df[[x]] %in% select[[x]])
    df <- df[rowSums(temp) == length(select), ]
  }
  df.interaction <- interaction(df[group], drop = TRUE)
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)
  
  if (isTRUE(bothSets)) {
    set2 <- df[!rownames(df) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}

##Campione
set.seed(133659)
stratificato <- stratified(df_to_use,"mag",size = c(232,168,74,20,10,7))


##Significatività del campione
#Media della magnitudine della popolazione
mean_mag_on_population <-mean(df_to_use$mag)
mean_mag_on_population
#T-test intervallo di confidenza
n = length(stratificato$mag)
alpha = 0.05 
t_alfa_2= qt(1-alpha/2, df=n-1) 
c(-t_alfa_2, t_alfa_2) 

#Intervallo di confidenza per magnitudine
xbar = mean(stratificato$mag)            
s = sqrt(sigma2_2(stratificato$mag))       
xbar
s
n
ic_min <- xbar-t_alfa_2*s/sqrt(n)
ic_max <- xbar+t_alfa_2*s/sqrt(n)
ic_min
ic_max

#Verifica della significatività del campione
mean_mag_on_population>ic_min & mean_mag_on_population<ic_max

###Modello statistico: Regressione lineare
linearMod_1 <- lm(mag ~ yr+len+wid+slon, data=stratificato) 
summary(linearMod_1)
mod_summary <- summary(linearMod_1)  
mod_summary$adj.r.squared


##Previsione con il modello
previsional1 <- matrix(c(1968,128.264,0.805,-98.37),nrow=1,ncol=4,byrow=T)
previsional1 <- as.data.frame(previsional1)
colnames(previsional1) <- c("yr","len","wid","slon")
predict(object = linearMod_1 ,newdata =previsional1)

previsional2 <- matrix(c(2021,128.264,0.805,-98.37),nrow=1,ncol=4,byrow=T)
previsional2 <- as.data.frame(previsional2)
colnames(previsional2) <- c("yr","len","wid","slon")
predict(object = linearMod_1 ,newdata =previsional2)




