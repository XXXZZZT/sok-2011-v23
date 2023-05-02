# Help file for assignment 1 - sok-2011 2022
library(dplyr)
library(plyr)
library(ggplot2)
library(WDI)
library(tidyverse)
library(countrycode)
library(psych)
library(Hmisc)

getwd()


Sys.setlocale(locale = "no_NO")


# WDIsearch(string = "search item", field = "name", short = TRUE, cache = NULL)

#   NY.GDP.PCAP.PP.KD (gdppc) = BNP per innbygger (PPP) 
#   NY.ADJ.NNAT.GN.ZS (nsy) = Sparing som andel av BNI (netto)
#   SP.POP.TOTL (poptot) = BefolkningsstC8rrelse
#   JI.TLF.TOTL (lf) = StC8rrelse pC% arbeidskraften
#   SP.POP.GROW (p) = Vekstrate i befolkningen
#   BAR.SCHL.15UP (educ) = Gjennomsnittlig antall C%r i skole (befolkning 15+)
#   SPI.D3.4.EDUC = kvalitet pC% skole. Mulig alternativ til gjennomsnittlig antall C%r i skole
#   NE.GDI.FTOT.KD.ZG (gi) = Crlig vekstrate i investeringer
#   NE.EXP.GNFS.KD.ZG (gx) = Crlig vekstrate i eksport
#   NY.ADJ.DRES.GN.ZS (nry) = Crlig reduksjonsrate i naturressurser

WDIsearch(string = "growth", field = "name", short = TRUE, cache = NULL)


# 1. BNP per innbyggere (alle C%r) og initial nivC% pC% BNP per innbyggere. WDI-variabel =  "NY.GDP.PCAP.PP.KD". 
# Velg startC%r = 2000 og sluttC%r = 2019

df_gdp0<-WDI(
  country = "all",
  indicator = c('gdppc'="NY.GDP.PCAP.PP.KD"),  
  start = 2000,
  end = 2019,
  extra = TRUE, # det C% sette "extra = TRUE" fC8rer til at vi laster inn ekstra informasjon som vi kan benytte seinere (f.eks. variabelen "region")
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_gdp <- subset(df_gdp0, select = c(country, region, income, iso2c, iso3c, year, gdppc) ) %>%  arrange(iso3c, year) # velg ut relevante variabler
df_gdp <-  df_gdp %>% mutate_all(na_if,"") # Vi C8nsker C% ta vekk land som ikke har en iso3c kode. Dessverre er manglende observasjoner for "iso3c" (landkode) kodet som "blanks" isteden for "missing". Denne koden korrigerer dette.
df_gdp <- df_gdp[complete.cases( df_gdp$gdppc, df_gdp$iso3c),] # Ta vekk observasjoner som mangler data pC% gdppc og iso3c. 
df_gdp = df_gdp  %>%  
  mutate(year = as.numeric(year)) # Se til at year er en numerisk variabel. 

# Noen land har flere observasjoner for samme C%r (f.eks afghanistan C%r 2010). Vi C8nsker C% ha C)n observasjon per land og C%r. 
df_gdp <- df_gdp[!duplicated(df_gdp[c("iso3c", "year", max("gdppc"))]), ]  %>%  arrange(iso3c, year) # Ta vekk duplikater for land og C%r, behold observasjonen med stC8rst gdppc (denne regelen kan diskuteres)

# Lag et datasett med Y0 (nivC% pC% BNP per innbyggere i C%r 2000)
df_gdp2000  <- df_gdp %>%  arrange(iso3c, year) %>% group_by(iso3c) %>% #Behold den fC8rste observasjonen for BNP per innbyggere (Y0)
  slice(1) %>%
  ungroup()
df_gdp2000 = subset(df_gdp2000, select = -c(year) ) # Slett unC8dvendige variabler
df_gdp2000 <-   plyr:: rename(df_gdp2000,c("gdppc" = "gdppc0")) # Gi variabeln et nytt navn slik at vi kan identifisere den i datasetet. 

df_gdp <- left_join(df_gdp,df_gdp2000, by=c("country", "iso2c", "iso3c", "region", "income")) # Sett sammen data for BNP per innbygger alle C%r, med BNP per innbygger C%r 2000.

#View(df_gdp)

# 2. Humankapital (gjennomsnittlig antall C%r i skole blant befolkningen eldre enn 15 C%r). WDI-variabel = BAR.SCHL.15UP 
df_educ0<-WDI(
  country = "all",
  indicator = c('educ'="BAR.SCHL.15UP"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_educ <- subset(df_educ0, select = c(country, region, income, iso2c, iso3c, year, educ) ) %>%  arrange(iso3c, year) #Behold nC8dvendige variabler
df_educ <- df_educ[complete.cases(df_educ$educ),] %>%  arrange(iso3c, year) # Slett observasjoner med manglende data

df_educ = df_educ %>%  
  arrange(iso3c, year) %>%  # Sorter etter Iso-kode og C%r. 
  mutate(educ = as.numeric(educ, na.rm = TRUE)) %>% # Se til at variabelen er numerisk
  ddply("iso3c",transform,
        avg_educ=mean(educ, na.rm = TRUE))  # Beregne gjennomsnittlig C%r i skole for tidsperioden 2000 - 2019 for hvert land, basert pC% tilgjenglig data (vil vC&re 2000.2005,2010)

df_educ <- subset(df_educ, select = c(country, region, income, iso2c, iso3c, avg_educ)) # Her tar jeg vekk variabelen "year". Jeg gjC8r dette fordi vi bare har en observasjon pC% utdanning per land. Vi C8nsker C% bruke denne verdi for alle C%r. 
df_educ <- df_educ[!duplicated(df_educ[c("iso3c")]), ]  %>%  arrange(iso3c) # Ta vekk duplikater for hvert land.
#View (df_educ)

# 3. Gjennomsnittlig sparing for perioden 2000-2015 (lagg fordi det kan ta litt tid for sparing C% bli til investering)
df_nsy0<-WDI(
  country = "all",
  indicator = c( 'nsy'="NY.ADJ.NNAT.GN.ZS"),  
  start = 2000,
  end = 2015,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_nsy <- subset(df_nsy0, select = c(country, region, income, iso2c, iso3c, year, nsy) ) %>%  arrange(iso3c, year) #Behold nC8dvendige variabler
df_nsy <- df_nsy[complete.cases(df_nsy$nsy),] %>%  arrange(iso3c, year) # Slett observasjoner med manglende data


df_nsy = df_nsy %>%  
  arrange(iso3c, year) %>%  # Sorter etter Iso-kode og C%r. 
  mutate(nsy = as.numeric(nsy, na.rm = TRUE)) %>% # Se til at variabelen er numerisk
  ddply("iso3c",transform,
        avg_nsy=mean(nsy, na.rm = TRUE))  # Beregne gjennomsnittlig C%r i skole for tidsperioden 2000 - 2019 for hvert land, basert pC% tilgjenglig data (vil vC&re 2000.2005,2010)

df_nsy <- subset(df_nsy, select = c(country, region, income, iso2c, iso3c, avg_nsy)) # Her tar jeg vekk variabelen "year". Jeg gjC8r dette fordi vi bare har en observasjon pC% utdanning per land. Vi C8nsker C% bruke denne verdi for alle C%r. 
df_nsy <- df_nsy[!duplicated(df_nsy[c("iso3c")]), ]  %>%  arrange(iso3c) # Ta vekk duplikater for hvert land.

#View(df_nsy)

# 4. Vekst i arbeidskraften (n)
df_lf0<-WDI(
  country = "all",
  indicator = c('lf'="JI.TLF.TOTL"),  # lf = labor force
  start = 2000,
  end = 2019,
  extra = TRUE, 
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_lf <- subset(df_lf0, select = c(country, region, income, iso2c, year, lf) ) %>%  arrange(iso2c, year) # velg ut relevante variabler
df_lf <-   plyr:: rename(df_lf,c("iso2c" = "iso3c")) # variabelen som identifiserer land med kode er feil i datasetet. Dette korrigerer dette
df_lf <-  df_lf %>% mutate_all(na_if,"") 
df_lf [df_lf == 0]<-NA
df_lf <- df_lf[complete.cases(df_lf$iso3c, df_lf$lf),] # Ta vekk observasjoner som mangler data pC% lf og iso3c. 
df_lf = df_lf  %>%  
  mutate(year = as.numeric(year)) # Se til at year er en numerisk variabel. 

df_lf <- df_lf[!duplicated(df_lf[c("iso3c", "year")]), ]  %>%  arrange(iso3c, year) # Ta vekk duplikater for land og C%r

# Ta fram vekstraten i arbeidskraften (n). Vi har ikke data for hvert C%r i alle land. 
# For C% beregne gjennomsnittlig C%rlig vekst mC% vi lage en variabel som mC%ler antallet tidsperioder mellom hver observasjon.
df_n = df_lf %>%  
  arrange(iso3c, year) %>%  # Sorter pC% C%r og land
  ddply("iso3c",transform,
        t=c(NA,diff(year)),
        lf_growth=c(NA,diff(log(lf)))) #Vekstrate uten hensyn til tidsintervall

df_n <- df_n[complete.cases(df_n$t, df_n$lf_growth),] # Ta vekk observasjoner som mangler data pC% t

#NC% kan vi ta fram C%rlig vekstrate
df_n = df_n %>%  
        mutate(t = as.numeric(t)) %>%   
        mutate(lf_growth = as.numeric(lf_growth))
df_n <- transform(df_n, n =lf_growth/t)

# gjennomsnittlig vekstrate i arbeidskraften for hvert land
df_n <- df_n %>% # 
          ddply("iso3c",transform,
                avg_n=mean(n, na.rm = TRUE)) #Gjennomsnittlig C%rlig vekstrate i arbeidskraften

df_n <- subset(df_n, select = c(iso3c, avg_n) )
df_n <- df_n[!duplicated(df_n["iso3c"]), ]  %>%  arrange(iso3c) # Ta vekk duplikater for land

View(df_n)


# 5. Lag et datasett som inneholder BNP data, utdanningsdata, sparing, og lfp data

df <- left_join(df_gdp, df_educ, by=c("country", "iso2c", "iso3c", "region", "income"))
df <- left_join(df, df_nsy, by=c("country", "iso2c", "iso3c", "region", "income"))
df <- left_join(df, df_n, by="iso3c")
df <- subset(df, select = c(country, region, income, iso2c, iso3c, year, gdppc, gdppc0, avg_educ, avg_nsy, avg_n)) # Behold nC8dvendige variabler

# Mange observasjoner representerer aggregerte regioner. Vi C8nsker C% ta vekk disse. Det finnes helt sikkert en bedre mC%te C% gjC8re dette pC%. Dette er den mC%ten jeg kom pC%.
df <- df  %>%  filter(iso2c!='1A' & iso2c !='1W' & iso2c != '4E' & iso2c != '7E' & iso2c !='8S'
                                         & iso2c !='B8' & iso2c !='EU' & iso2c !='F1' & iso2c !='OE' & iso2c !='S1' & iso2c !='S2' & iso2c !="S3" 
                                         & iso2c !='S4' & iso2c !='T2' & iso2c !='T3' & iso2c !='T4' & iso2c !='T5' & iso2c !='T6' & iso2c !='T7' 
                                         & iso2c !='V1' & iso2c !='V2' & iso2c !='V3' & iso2c !='V4' & iso2c !='XC' & iso2c !='XD' & iso2c !='XE' 
                                         & iso2c !='XF' & iso2c !='XG' & iso2c !='XH' & iso2c !='XI' & iso2c !='XJ' & iso2c !='XL' & iso2c !='XM' 
                                         & iso2c !='XN' & iso2c !='XO' & iso2c !='XP' & iso2c !='XQ' & iso2c !='XT' & iso2c !='XU' & iso2c !='Z4' 
                                         & iso2c !='Z7' & iso2c !='ZF'& iso2c !='ZG'  & iso2c !='ZH' & iso2c !='ZI'  & iso2c !='ZJ'  & iso2c !='ZQ'  
                                         & iso2c !='ZT'  & iso2c !='Z7')  %>% arrange(iso3c, year) 
#View(df)

# 6. Lag et datasett for resterende variabler.

df_rest0<-WDI(
  country = "all",
  indicator = c('poptot'="SP.POP.TOTL", 'gi'="NE.GDI.FTOT.KD.ZG", 'gx'="NE.EXP.GNFS.KD.ZG", 'nry'="NY.ADJ.DRES.GN.ZS", 'p'="SP.POP.GROW" ),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_rest0<-df_rest0 %>% mutate_all(na_if,"")
df_rest <- df_rest0[complete.cases( df_rest0$iso3c),]  %>%  arrange(iso2c) 


# Ta vekk observasjoner som ikke representerer land.
df_rest <- df_rest  %>%  filter(iso2c!='1A' & iso2c !='1W' & iso2c != '4E' & iso2c != '7E' & iso2c !='8S'
                                  & iso2c !='B8' & iso2c !='EU' & iso2c !='F1' & iso2c !='OE' & iso2c !='S1' & iso2c !='S2' & iso2c !="S3" 
                                  & iso2c !='S4' & iso2c !='T2' & iso2c !='T3' & iso2c !='T4' & iso2c !='T5' & iso2c !='T6' & iso2c !='T7' 
                                  & iso2c !='V1' & iso2c !='V2' & iso2c !='V3' & iso2c !='V4' & iso2c !='XC' & iso2c !='XD' & iso2c !='XE' 
                                  & iso2c !='XF' & iso2c !='XG' & iso2c !='XH' & iso2c !='XI' & iso2c !='XJ' & iso2c !='XL' & iso2c !='XM' 
                                  & iso2c !='XN' & iso2c !='XO' & iso2c !='XP' & iso2c !='XQ' & iso2c !='XT' & iso2c !='XU' & iso2c !='Z4' 
                                  & iso2c !='Z7' & iso2c !='ZF'& iso2c !='ZG'  & iso2c !='ZH' & iso2c !='ZI'  & iso2c !='ZJ'  & iso2c !='ZQ'  
                                  & iso2c !='ZT'  & iso2c !='Z7')  %>% arrange(iso3c, year) 

df_rest <- subset(df_rest, select = c("country", "region", "income", "iso3c", "iso2c", "year", "poptot", "p", "nry", "gi", "gx"))
df_all <- left_join(df, df_rest, by=c("country", "region", "income", "iso2c", "iso3c", "year"))

# View(df_all)

# Lag en rekkefC8lge til variablene slik at det er enklere C% fC% en oversikt over datamaterialet.
col_order <- c("country",  "region", "income", "iso3c", "iso2c", "year", "gdppc", "gdppc0", "poptot", "p", "avg_n", "avg_nsy", "nry", "gi", "gx", "avg_educ")
df_all <- df_all[, col_order]

#View(df_all)

# Ta fram vekstraten og gjennomsnitt for resterende variabler
df_growth0 = df_all %>%  
  arrange(iso3c, year) %>%  # Sorter pC% C%r og land
  ddply("iso3c",transform,
        gdpgrowth=c(NA,diff(log(gdppc)))*100) %>%   # Crlig vekstrate i gdppc for hvert land
  mutate(gdpgrowth = as.numeric(gdpgrowth, na.rm = TRUE)) %>% # 
  ddply("iso3c",transform,
        avg_gdpgrowth=mean(gdpgrowth, na.rm = TRUE), #Gjennomsnittlig C%rlig vekstrate i BNP per innbygger for hvert land i perioden
        avg_gi=mean(gi, na.rm = TRUE), # Gjennomsnittlig C%rlig vekstrate i investeringer for hvert land  i perioden
        avg_nry=mean(nry, na.rm = TRUE),  # Gjennomsnittlig C%rlig vekstrate (negativ) i naturressurser for hvert land  i perioden
        avg_gx=mean(gx, na.rm = TRUE),  # Gjennomsnittlig C%rlig vekstrate i eksport for hvert land  i perioden
        avg_p=mean(p, na.rm = TRUE))  # Gjennomsnittlig C%rlig vekstrate i befolkningen for hvert land  i perioden
        
View(df_growth0)
df_growth0 <-  df_growth0 %>% mutate_all(na_if,"") 
df_growth <- df_growth0[complete.cases( df_growth0$country, df_growth0$income, df_growth0$iso3c, df_growth0$avg_gdpgrowth, df_growth0$gdppc0, df_growth0$avg_n, df_growth0$avg_p, df_growth0$avg_nsy, df_growth0$avg_nry,df_growth0$avg_gi, df_growth0$avg_gx, df_growth0$avg_educ),] # Ta vekk land som mangler data 


df_growth <- subset(df_growth, select = c("country",  "region", "income", "iso3c", "iso2c","year", "poptot", "gdppc", "gdppc0", "avg_gdpgrowth", "avg_n", "avg_p", "avg_nsy", "avg_nry", "avg_gi", "avg_gx", "avg_educ"))

# Lag datasettet du vil bruke til analysen din
df_growth2019  <- df_growth %>%  arrange(iso3c, year) %>% group_by(iso3c) %>% 
  slice(n()) %>% # Behold den SISTE observasjonen for hvert land
  ungroup()
head(df_growth2019)

# Lag en variabel som er logaritmen av BNP per innbygger (enklere tolkning og presser sammen fordelingen)
df_growth2019$dppc <-as.numeric(df_growth2019$gdppc)
df_growth2019$ln_gdppc<-log(df_growth2019$gdppc) 
df_growth2019$ln_gdppc0<-log(df_growth2019$gdppc0) 

# View(df_growth2019)


# Deskriptiv analyse
library(vtable)
library(car)
library(knitr)
library(utf8)


#   NY.GDP.PCAP.PP.KD (gdppc) = BNP per innbygger (PPP) 
#   NY.ADJ.NNAT.GN.ZS (nsy) = Sparing som andel av BNI (netto)
#   SP.POP.TOTL (poptot) = BefolkningsstC8rrelse
#   JI.TLF.TOTL (lf) = StC8rrelse pC% arbeidskraften
#   SP.POP.GROW (p) = Vekstrate i befolkningen
#   BAR.SCHL.15UP (educ) = Gjennomsnittlig antall C%r i skole (befolkning 15+)
#   SPI.D3.4.EDUC = kvalitet pC% skole. Mulig alternativ til gjennomsnittlig antall C%r i skole
#   NE.GDI.FTOT.KD.ZG (gi) = Crlig vekstrate i investeringer
#   NE.EXP.GNFS.KD.ZG (gx) = Crlig vekstrate i eksport
#   NY.ADJ.DRES.GN.ZS (nry) = Crlig reduksjonsrate i naturressurser

# 1. En tabell med deskriptiv statistiskk (gjennomsnitt, standard avvik, min og maks)
# Velg ut de variabler du vil ha med i tabellen
#NB: Du mC% selv legge til variabler her!
df <- subset(df_growth2019, select = c("avg_gdpgrowth",  "avg_gi",  "avg_nsy",  "avg_nry",  "avg_gx",  "avg_p",  "avg_educ", "gdppc0"))
#Gi variablene navn som gir mening. NB: rekkefC8lgen mC% vC&re lik rekkefC8lgen til variablene i df.
labs <- c("Vekstrate i BNP pc","Vekstrate i investeringer (%)","Sparing som andel av BNI(%)","??rlig vekstrate i eksport","Vekstrate i befolkningen","Befolkningst??rrelse","Gjennomsntilig antal ??r i skole","BNP per innbygger 2000") 



#help("sumtable")
#Lag tabellen
st(df, labels=labs)
st(df, vars = c("avg_gdpgrowth",  "avg_gi",  "avg_nsy",  "avg_nry",  "avg_gx",  "avg_p",  "avg_educ", "gdppc0"))
st(df, labels=labs,
   summ = list(
     c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'), # Dette beskriver hvilken informasjon du vil ha med i tabellen
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Gjennomsnitt','SD','Min','Maks') #Dette setter titler til kolumnene.
  ))



#

df_growth2019_no_sle <- df_growth2019 %>%
  filter(iso3c != "SLE")  # Filtrer bort observasjoner med iso3c lik "SLE"



# 2. Grafer

# Vekst i BNP per innbygger og vekst i investeringer
p_growth_gi <- df_growth2019_no_sle %>%
  ggplot(aes(x = avg_nsy, y = log(gdppc))) +
  xlab("Sparing som andel i BMI %") + 
  ylab("BNP per innbygger 2000-2019") + 
  geom_text(aes(label=iso3c),size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))+
  theme_classic()

p_growth_gi


#Plot 2
# Befolkningsvekst og Antall ??r utdanning p?? skolen
p_growth_gi2 <- df_growth2019_no_sle %>%
  ggplot(aes(x = avg_p, y = log(gdppc))) +
  xlab("Belfolkningsvekst") + 
  ylab("BNP per innbygger 2000-2019") + 
  geom_text(aes(label=iso3c),size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))+
  theme_classic()

p_growth_gi2

#plot3
# Opprett graf med Sparing + utdaninng med BNP per innbygger 
p_growth_gi3 <- df_growth2019_no_sle %>%
  ggplot(aes(x = avg_educ, y = log(gdppc))) +
  xlab("Antall som har fullf??rt grunnskole") + 
  ylab("BNP per innbygger 2000-2019") + 
  geom_text(aes(label=iso3c), size=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white')) +
  theme_classic()

# Vis grafen
p_growth_gi3


#Plot 4
# Befolkningsvekst og Antall ??r utdanning p?? skolen
p_growth_gi4 <- df_growth2019_no_sle %>%
  ggplot(aes(x = avg_nsy, y = avg_gdpgrowth)) +
  xlab("Sparing som andel av BNI") + 
  ylab("??rlig vekstrate i BNP 2000-2019") + 
  geom_text(aes(label=iso3c),size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))+
  theme_classic()

p_growth_gi4



#Plot 5
# Befolkningsvekst og Antall ??r utdanning p?? skolen
p_growth_gi5 <- df_growth2019_no_sle %>%
  ggplot(aes(x = avg_educ, y = avg_gdpgrowth)) +
  xlab("Antall ??r Utdanning") + 
  ylab("??rlig vekstrate i BNP 2000-2019") + 
  geom_text(aes(label=iso3c),size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))+
  theme_classic()

p_growth_gi5
















suppressPackageStartupMessages(library(scales))

df_growth2019_n <- df_growth2019[complete.cases(df_growth2019$region,df_growth2019$poptot, df_growth2019$avg_p, df_growth2019$gdppc),]

plot1 <- ggplot(df_growth2019_n, aes(x = avg_p , y = ln_gdppc, na.rm = TRUE)) +
  xlab("Befolkningsvekst") +
  ylab("BNP per innbygger 2019") +
  theme_minimal(base_size = 14, base_family = "Georgia") +
  geom_point(aes(size = poptot, color = region), alpha = 0.8) +
  scale_x_continuous(breaks=c(-1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5 )) + # Remove labels argument
  scale_size_area(guide = "none", max_size = 14) +
  theme(legend.text = element_text(size = 10,color="black")) +
  scale_colour_manual(values = rainbow(9)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white')) +
  scale_y_continuous(trans = 'log2', labels = scales::dollar, breaks=c(500, 2000, 8000, 32000, 120000))

plot1



# Sjekk om det er ekstreme observasjoner i data
model1 <- lm(avg_gdpgrowth  ~ avg_gi , data=  df_growth2019 ) #NB: Du mC% legge inn "Dine" variabler her
summary(model1)
# Vi har ekstreme observasjoner i datamaterialet. Vi mC% sjekke hvordan resultatene pC%virkes av at vi tar vekk disse. 
# Koden her nede fC8lger metoden som er beskrevet her: https://www.r-bloggers.com/2021/09/how-to-remove-outliers-in-r-3/ for reference

# Basert pC% den grafiske analysen av sammenhengen mellom vekst i BNP per innbygger og vekst i investeringer, starter jeg med C% se pC% ekstreme observasjoner i avg_gi
Q1gi <- quantile(df_growth2019$avg_gi, .25 )
Q3gi <- quantile(df_growth2019$avg_gi, .75)
IQRgi <- IQR(df_growth2019$avg_gi)
no_outliers1 <- subset(df_growth2019, avg_gi > (Q1gi - 1.5*IQRgi) & avg_gi < (Q3gi + 1.5*IQRgi)) #& avg_nsy > (Q1nsy - 1.5*IQRnsy) & avg_nry > (Q1nry - 1.5*IQRnry) & avg_gx > (Q1gx - 1.5*IQRgx) & avg_p > (Q1p - 1.5*IQRp) & avg_educ > (Q1educ - 1.5*IQReduc) & gdppc0 > (Q1gdppc0 - 1.5*IQRgdppc0))
dim(no_outliers1)




#
View (no_outliers1)
model2 <- lm(avg_gdpgrowth  ~ avg_gi , data= no_outliers1) #NB: Du mC% legge inn "Dine" variabler her
summary(model2)

# Jeg kan deretter gC% videre C% se om det er andre ekstreme observasjoner i andre variabler. 

suppressPackageStartupMessages(library(sjPlot))
suppressPackageStartupMessages(library(sjmisc))
suppressPackageStartupMessages(library(sjlabelled))



# Definerer modellen slik at den passer inn i model2
model2 <- lm(avg_gdpgrowth ~ avg_gi + avg_nsy + avg_nry + avg_gx + avg_p + avg_educ + ln_gdppc0, data = no_outliers1)

# Define the labels for predictor variables
labs2 <- c("Vekstrate i BNP pc","Vekstrate i investeringer (%)","Sparing som andel av BNI(%)","??rlig vekstrate i eksport","Vekstrate i befolkningen","Befolkningst??rrelse","Gjennomsntilig antal ??r i skole","BNP per innbygger 2000")

# bruker tab_model kombinert med model2 and pred.labels. Gj??r dette for ?? fram navn p?? mine variabler
tab_model(model2, p.style = "stars", pred.labels = labs2)


