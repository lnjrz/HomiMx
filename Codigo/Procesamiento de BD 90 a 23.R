# --- Instalar paqueteria requerida, definir directorio y parametros encuestas ----
#Paqueteria a instalar
rm(list=ls())
library(purrr)
packages<-c("foreign", "openxlsx","dplyr","purrr","lubridate")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
lapply(packages, require, character.only=TRUE)
# --- Directorio de trabajo
dir <- "C:/Users/alan5/OneDrive/Documentos/INEGI/Registros Administrativos"
setwd(dir)
# --- Extraccion de homicidios 1990 a 1997 ----
for (i in 0:7) {
  eval(parse(text = paste0("file1<-'defunciones_base_datos_199",i,"_dbf/DEFUN9",i,".dbf'")))
  DEF<-read.dbf(file1)
  DEF$CVE_HOM <- as.character(DEF$CAUSA_DEF)
  DEF$HOM <- ifelse(DEF$CVE_HOM%in%c('9620','961X','9622','9621','9629','963X','964X',
                                       '9650','9651','9652','9653','9654','9655','9656',
                                       '9657','9658','9659','9680','9683','966X','9682',
                                       '9681','9600','9670','9671','9679','9601','9684',
                                       '9688','9689'),1,0)
  HOM <- DEF %>% 
    filter(HOM == 1)
  
  if(i%in%0){
    BD90_97 <- HOM  
  }else{
    BD90_97 <- rbind(BD90_97,HOM)
  }
}

# --- Extraccion de homicidios 1998 y 1999 ----
for (i in c(98:99)) {
  eval(parse(text = paste0("file1<-'defunciones_base_datos_19",i,"_dbf/DEFUN",i,".dbf'")))
  DEF<-read.dbf(file1)
  DEF$CVE_HOM <- substr(DEF$CAUSA_DEF,1,3)
  DEF$HOM <- ifelse(DEF$CVE_HOM%in%c('X93','X94','X95','X99','Y00','X91','X92','X85','X86','X87',
                                     'X88','X89','X90','X96','X97','X98','Y01','Y02','Y03','Y04',
                                     'Y05','Y06','Y07','Y08','Y09'),1,0)
  HOM <- DEF %>% 
    filter(HOM == 1)
  
  if(i%in%98){
    BD98_99 <- HOM}else{
      BD98_99 <- rbind(BD98_99,HOM)
    }
}
# --- Extraccion de homicidios de 2000 a 2001 ----
for (i in c(paste0('0',0:1))) {
  eval(parse(text = paste0("file1<-'defunciones_base_datos_20",i,"_dbf/DEFUN",i,".dbf'")))
  DEF<-read.dbf(file1)
  DEF$CVE_HOM <- substr(DEF$CAUSA_DEF,1,3)
  DEF$HOM <- ifelse(DEF$CVE_HOM%in%c('X93','X94','X95','X99','Y00','X91','X92','X85','X86','X87',
                                         'X88','X89','X90','X96','X97','X98','Y01','Y02','Y03','Y04',
                                         'Y05','Y06','Y07','Y08','Y09'),1,0)
  HOM <- DEF %>% 
    filter(HOM == 1)
  
  if(i%in%"00"){
    BD00_01 <- HOM}else{
      BD00_01 <- rbind(BD00_01,HOM)
  }
}
# --- Extraccion de homicidios de 2002 a 2003 ----
for (i in c(paste0('0',2:3))) {
  eval(parse(text = paste0("file1<-'defunciones_base_datos_20",i,"_dbf/DEFUN",i,".dbf'")))
  DEF<-read.dbf(file1)
  DEF$CVE_HOM <- substr(DEF$CAUSA_DEF,1,3)
  DEF$HOM <- ifelse(DEF$CVE_HOM%in%c('X93','X94','X95','X99','Y00','X91','X92','X85','X86','X87',
                                     'X88','X89','X90','X96','X97','X98','Y01','Y02','Y03','Y04',
                                     'Y05','Y06','Y07','Y08','Y09'),1,0)
  HOM <- DEF %>% 
    filter(HOM == 1)
  
  if(i%in%"02"){
    BD02_03 <- HOM}else{
      BD02_03 <- rbind(BD02_03,HOM)
    }
}
# --- Extraccion de homicidios de 2004 a 2011 ----
for (i in c(paste0('0',4:9),10:11)) {
  eval(parse(text = paste0("file1<-'defunciones_base_datos_20",i,"_dbf/DEFUN",i,".dbf'")))
  DEF<-read.dbf(file1)
  DEF$CVE_HOM <- substr(DEF$CAUSA_DEF,1,3)
  DEF$HOM <- ifelse(DEF$CVE_HOM%in%c('X93','X94','X95','X99','Y00','X91','X92','X85','X86','X87',
                                     'X88','X89','X90','X96','X97','X98','Y01','Y02','Y03','Y04',
                                     'Y05','Y06','Y07','Y08','Y09'),1,0)
  HOM <- DEF %>% 
    filter(HOM == 1)
  
  if(i%in%"04"){
    BD04_11 <- HOM}else{
      BD04_11 <- rbind(BD04_11,HOM)
    }
}
# --- Extraccion de homicidios de 2012 ----
for (i in c(12)) {
  eval(parse(text = paste0("file1<-'defunciones_base_datos_20",i,"_dbf/DEFUN",i,".dbf'")))
  DEF<-read.dbf(file1)
  DEF$CVE_HOM <- substr(DEF$CAUSA_DEF,1,3)
  DEF$HOM <- ifelse(DEF$CVE_HOM%in%c('X93','X94','X95','X99','Y00','X91','X92','X85','X86','X87',
                                     'X88','X89','X90','X96','X97','X98','Y01','Y02','Y03','Y04',
                                     'Y05','Y06','Y07','Y08','Y09'),1,0)
  HOM <- DEF %>% 
    filter(HOM == 1)
  
  if(i%in%12){
    BD12 <- HOM}else{
      BD12 <- rbind(BD12,HOM)
    }
}
# --- Extraccion de homicidios de 2013 - 2021 ----
for (i in c(13:21)) {
  eval(parse(text = paste0("file1<-'defunciones_base_datos_20",i,"_dbf/DEFUN",i,".dbf'")))
  DEF<-read.dbf(file1)
  DEF$CVE_HOM <- substr(DEF$CAUSA_DEF,1,3)
  DEF$HOM <- ifelse(DEF$CVE_HOM%in%c('X93','X94','X95','X99','Y00','X91','X92','X85','X86','X87',
                                     'X88','X89','X90','X96','X97','X98','Y01','Y02','Y03','Y04',
                                     'Y05','Y06','Y07','Y08','Y09'),1,0)
  HOM <- DEF %>% 
    filter(HOM == 1)
  
  if(i%in%13){
    BD13_21 <- HOM}else{
      BD13_21 <- rbind(BD13_21,HOM)
    }
}
# --- Extraccion de homicidios de 2022 ----
for (i in c(22)) {
  eval(parse(text = paste0("file1<-'defunciones_base_datos_20",i,"_dbf/DEFUN",i,".dbf'")))
  DEF<-read.dbf(file1)
  DEF$CVE_HOM <- substr(DEF$CAUSA_DEF,1,3)
  DEF$HOM <- ifelse(DEF$CVE_HOM%in%c('X93','X94','X95','X99','Y00','X91','X92','X85','X86','X87',
                                     'X88','X89','X90','X96','X97','X98','Y01','Y02','Y03','Y04',
                                     'Y05','Y06','Y07','Y08','Y09'),1,0)
  HOM <- DEF %>% 
    filter(HOM == 1)
  
  if(i%in%22){
    BD22 <- HOM}else{
      BD22 <- rbind(BD22,HOM)
    }
}
# --- Extraccion de homicidios de 2022 ----
for (i in c(23)) {
  eval(parse(text = paste0("file1<-'defunciones_base_datos_20",i,"_dbf/DEFUN",i,".dbf'")))
  DEF<-read.dbf(file1)
  DEF$CVE_HOM <- substr(DEF$CAUSA_DEF,1,3)
  DEF$HOM <- ifelse(DEF$CVE_HOM%in%c('X93','X94','X95','X99','Y00','X91','X92','X85','X86','X87',
                                     'X88','X89','X90','X96','X97','X98','Y01','Y02','Y03','Y04',
                                     'Y05','Y06','Y07','Y08','Y09'),1,0)
  HOM <- DEF %>% 
    filter(HOM == 1)
  
  if(i%in%23){
    BD23 <- HOM}else{
      BD23 <- rbind(BD23,HOM)
    }
}
# Comparacion de nombres de los distintos peridodos
lista_nombres <- list(
  BD90_97 = names(BD90_97),
  BD98_99 = names(BD98_99),
  BD00_01 = names(BD00_01),
  BD02_03 = names(BD02_03),
  BD04_11 = names(BD04_11),
  BD12    = names(BD12),
  BD13_21 = names(BD13_21),
  BD22    = names(BD22),
  BD23    = names(BD23)
)
# Encuentra la longitud maxima
max_length <- max(sapply(lista_nombres, length))
# Rellena con NA para que todos tengan la misma longitud
lista_nombres_padded <- map(lista_nombres, ~{ length(.x) <- max_length; .x })
# Convierte en data frame
NOMBRES_COL <- as.data.frame(lista_nombres_padded)
# --- Seleccion de variables de la BD ----

VAR90_97 <- c("ENT_REGIS","MUN_REGIS","ENT_RESID","MUN_RESID","ENT_OCURR","MUN_OCURR","CVE_HOM","SEXO","EDAD","ANIO_OCUR","MES_OCURR",
              "ANIO_REGIS","MES_REGIS","OCUPACION","ESCOLARIDA","EDO_CIVIL","LUGAR_OCUR")
VAR98_23 <- c("ENT_REGIS","MUN_REGIS","ENT_RESID","MUN_RESID","ENT_OCURR","MUN_OCURR","CVE_HOM","SEXO","EDAD","ANIO_OCUR","MES_OCURR",
               "ANIO_REGIS","MES_REGIS","OCUPACION","ESCOLARIDA","EDO_CIVIL","LUGAR_OCUR","DIA_OCURR","HORAS","MINUTOS")


BDTOT <- BD90_97[,c(VAR90_97)]
BDTOT$DIA_OCURR <- 99
BDTOT$HORAS <- 99
BDTOT$MINUTOS <- 99

BDTOT <- rbind(BDTOT,BD98_99[,c(VAR98_23)],BD00_01[,c(VAR98_23)],BD02_03[,c(VAR98_23)],
               BD04_11[,c(VAR98_23)],BD12[,c(VAR98_23)],BD13_21[,c(VAR98_23)],BD22[,c(VAR98_23)],
               BD23[,c(VAR98_23)])
# --- Homologacion de ENT_REGIS
table(BDTOT$ENT_REGIS)
BDTOT$ENT_REGIS <- as.numeric(as.character(BDTOT$ENT_REGIS))
BDTOT$ENT_REGIS <- ifelse(BDTOT$ENT_REGIS<10,paste0("0",BDTOT$ENT_REGIS),paste0(BDTOT$ENT_REGIS))
# --- Homologacion de MUN_REGIS
table(BDTOT$MUN_REGIS)
BDTOT$MUN_REGIS <- as.numeric(as.character(BDTOT$MUN_REGIS))
BDTOT$MUN_REGIS <- ifelse(BDTOT$MUN_REGIS%in%1:9,paste0("00",BDTOT$MUN_REGIS),
                          ifelse(BDTOT$MUN_REGIS%in%10:99,paste0("0",BDTOT$MUN_REGIS),paste0(BDTOT$MUN_REGIS)))
BDTOT$CVEGEO_REGIS <- paste0(BDTOT$ENT_REGIS,BDTOT$MUN_REGIS)# Generar una variable unica de municipio de registros
# --- Homologacion de ENT_RESID
BDTOT$ENT_RESID <- as.numeric(as.character(BDTOT$ENT_RESID))
BDTOT$ENT_RESID <- ifelse(BDTOT$ENT_RESID<10,paste0("0",BDTOT$ENT_RESID),paste0(BDTOT$ENT_RESID))
# --- Homologacion de MUN_RESID
BDTOT$MUN_RESID <- as.numeric(as.character(BDTOT$MUN_RESID))
BDTOT$MUN_RESID <- ifelse(BDTOT$MUN_RESID%in%1:9,paste0("00",BDTOT$MUN_RESID),
                          ifelse(BDTOT$MUN_RESID%in%10:99,paste0("0",BDTOT$MUN_RESID),paste0(BDTOT$MUN_RESID)))
# --- Homologacion de ENT_OCURR
BDTOT$ENT_OCURR <- as.numeric(as.character(BDTOT$ENT_OCURR))
BDTOT$ENT_OCURR <- ifelse(BDTOT$ENT_OCURR<10,paste0("0",BDTOT$ENT_OCURR),paste0(BDTOT$ENT_OCURR))
# --- Homologacion de MUN_OCURR
BDTOT$MUN_OCURR <- as.numeric(as.character(BDTOT$MUN_OCURR))
BDTOT$MUN_OCURR <- ifelse(BDTOT$MUN_OCURR%in%1:9,paste0("00",BDTOT$MUN_OCURR),
                          ifelse(BDTOT$MUN_OCURR%in%10:99,paste0("0",BDTOT$MUN_OCURR),paste0(BDTOT$MUN_OCURR)))
# --- Homologacion de CVE_HOM: Es generada una variable adicional con la reclasificacion de los homicidios en 4 grupos
# --- Disparo de arma de fuego/Objeto cortante o punzante/Ahorcamiento, estrangulamiento y sofocacion
BDTOT$REC_CAU <- BDTOT$CVE_HOM
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9620','X85',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'961X','X86',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9622','X88',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9621','X89',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9629','X90',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'963X','X91',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'964X','X92',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9650','X93',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%c('9651','9652','9653'),'X94',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9654','X95',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%c('9655','9656','9657','9658','9659'),'X96',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9680','X97',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9683','X98',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'966X','X99',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9682','Y00',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9681','Y01',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%c('9600','9670','9671','9679'),'Y04',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9601','Y05',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9684','Y06',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9688','Y08',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%'9689','Y09',BDTOT$REC_CAU)
BDTOT$REC_CAU <- ifelse(BDTOT$REC_CAU%in%c('X93','X94','X95'),1,# Disparo con arma de fuego
                        ifelse(BDTOT$REC_CAU%in%c('X99','Y00'),2,# Objeto cortante o punzante
                               ifelse(BDTOT$REC_CAU%in%c('X91','X92'),3,# Ahorcamiento, estrangulamiento
                                      ifelse(BDTOT$REC_CAU%in%c('X85','X86','X87','X88',
                                                                'X89','X90','X96','X97',
                                                                'X98','Y01','Y02','Y03',
                                                                'Y04','Y05','Y06','Y07',
                                                                'Y08'),4,# Otros medios
                                             ifelse(BDTOT$REC_CAU%in%'Y09',5,0)))))# Medios no especificados
# --- Hologacion de variable edad
table(BDTOT$EDAD)
BDTOT$EDAD <- as.numeric(as.character(BDTOT$EDAD))
BDTOT$EDAD <- ifelse(BDTOT$EDAD<4000,0,BDTOT$EDAD)
BDTOT$EDAD <- ifelse(BDTOT$EDAD%in%4000:4120,substr(BDTOT$EDAD,3,4),BDTOT$EDAD)
BDTOT$EDAD <- ifelse(BDTOT$EDAD%in%4998,999,BDTOT$EDAD)
# --- Homologacion de variable ANIO_OCUR para homologar a 4 digitos
BDTOT$ANIO_OCUR <- ifelse(BDTOT$ANIO_OCUR%in%0:9,paste0("190",BDTOT$ANIO_OCUR),
                          ifelse(BDTOT$ANIO_OCUR%in%10:99,paste0("19",BDTOT$ANIO_OCUR),BDTOT$ANIO_OCUR))
# --- Homologacion de variable ANIO_REGIS para homologar a 4 digitos
BDTOT$ANIO_REGIS <- ifelse(BDTOT$ANIO_REGIS%in%0:9,paste0("190",BDTOT$ANIO_REGIS),
                          ifelse(BDTOT$ANIO_REGIS%in%10:99,paste0("19",BDTOT$ANIO_REGIS),BDTOT$ANIO_REGIS))
# --- Homologacion de variable OCUPACION
table(BDTOT$OCUPACION)
BDTOT$ID_OCU <- paste0(BDTOT$ANIO_REGIS,".",BDTOT$OCUPACION)
REC_OCU <- read.csv("C:/Users/alan5/OneDrive/Documentos/JAJG/Proyectos/Package/HomiMx/Generados/REC_OCU.csv",colClasses = c("ID_OCU"="character","CVE"="character"))
names(REC_OCU)[5] <- "CAT_OCU"
BDTOT <- left_join(BDTOT, REC_OCU[,c("ID_OCU","CAT_OCU")]) 
#1	Agricultor
#2	Artesanos
#3	Empleados, auxiliares o ayudantes
#4	No trabajaba
#5	Comerciantes
#6	Operadores o conductores
#7	Directores o coordinadores
#8	Profesionista o técnicos
#9	No especificado
#10	No aplica
#11	Seguridad
barplot(table(BDTOT[BDTOT$CAT%in%11 & BDTOT$ANIO_OCUR%in%1990:2023,]$ANIO_OCUR))
# --- Homologacion de variable escolaridad
names(BDTOT)
BDTOT$ID_EDU <- paste0(BDTOT$ANIO_REGIS,".",BDTOT$ESCOLARIDA)
EDU <- read.csv("C:/Users/alan5/OneDrive/Documentos/JAJG/Proyectos/Package/HomiMx/Generados/EDUCACION.csv",colClasses = c("ID_EDU"="character","EDU"="character"))
names(EDU)[6] <- "CAT_ESC"
BDTOT<- left_join(BDTOT,EDU[,c("ID_EDU","CAT_ESC")])
#1	Sin educación
#2	Basica
#3	Media
#4	Superior
#5	No aplica
#6	No especificado
barplot(table(BDTOT[BDTOT$CAT_ESC%in%2 & BDTOT$ANIO_OCUR%in%1990:2023,]$ANIO_OCUR))
# --- Homologacion de variable de estado civil
table(BDTOT$EDO_CIVIL)
CIV <- read.csv("C:/Users/alan5/OneDrive/Documentos/JAJG/Proyectos/Package/HomiMx/Generados/EDOCIVIL.csv",colClasses = c("ID_CIVIL"="character"))
BDTOT$ID_CIVIL <- paste0(BDTOT$ANIO_REGIS,".",BDTOT$EDO_CIVIL)
BDTOT <- left_join(BDTOT,CIV[,c("ID_CIVIL","CAT_CIVIL")])
#1	Casado(a)
#2	Divorciado(a)
#3	No aplica
#4	No especificado
#5	Separado(a)
#6	Soltero(a)
#7	Unión libre
#8	Viudo(a)
table(BDTOT$CAT_CIVIL)
# --- homologacion de variable lugar de ocurrencia
names(BDTOT)
table(BDTOT$LUGAR_OCUR)
BDTOT$ID_LUG <- paste0(BDTOT$ANIO_REGIS,".",BDTOT$LUGAR_OCUR)
LUG <- read.csv("C:/Users/alan5/OneDrive/Documentos/JAJG/Proyectos/Package/HomiMx/Generados/LUGAROCURR.csv",colClasses = c("ID_LUG"="character"))
BDTOT <- left_join(BDTOT,LUG[,c("ID_LUG","CAT_LUG")])
#1	Espacio institucional
#2	Espacio laboral
#3	Espacio recreativo o público
#4	Hogar
#5	Vía pública
#6	Otro
#7	No especificado
#8	No aplica
table(BDTOT$CAT_LUG)
# --- Creacion variable fecha para obtener dia de la semana de ocurrencia
BDTOT <- BDTOT %>% 
  mutate(
    ANIO = ifelse(ANIO_OCUR == 9999,NA, ANIO_OCUR),
    MES = ifelse(MES_OCURR == 99, NA, MES_OCURR),
    DIA = ifelse(DIA_OCURR == 99, NA, DIA_OCURR),
    FECHA_OCURR = make_date(year = ANIO, month= MES, day = DIA),
    DIA_SEMANA = weekdays(FECHA_OCURR)
    )
table(is.na(BDTOT$DIA_SEMANA))

saveRDS(BDTOT,"C:/Users/alan5/OneDrive/Documentos/JAJG/Proyectos/Package/HomiMx/data/HOMI9023.rds")

