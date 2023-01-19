

##### Zadania 3

### 1. Przefiltruj dane z bazy wybierając najnowsze ogłoszenia, daty
# wybierz z tabeli data.

### 2.Wybierz kolumny:
# cena,Przebieg,Rok.produkcji,Marka.pojazdu, Model.pojazdu,Wersja,Generacja,Rodzaj.paliwa,
# Pojemność.skokowa,Moc,Skrzynia.biegów, Napęd,Typ.nadwozia,Liczba.drzwi,Liczba.miejsc,
# Bezwypadkowy,Uszkodzony,Stan,Kolor

# Oraz ‘napraw’ kolumny :Przebieg, Pojemność.skokowa,Moc

### 3.Wykonaj raport RMarkdown (kod R uwzględniony w raporcie)
# oraz zaproponuj najlepszy według Ciebie samochód z
# dostępnych ogłoszeń

# -samochód powinien być nieuszkodzony
# -rocznik od 2013.
# -budżet klienta to 60000 zł.

# -Przedstaw w tabeli: średnie ceny, przebieg, rocznik, liczbę ogłoszeń wybranych
# modeli.
# -Przedstaw w tabeli: mediany cen, przebiegu, rocznika, liczbę ogłoszeń wybranych
# modeli.

# -Na wykresach przedstaw dla wybranych modeli:
# -liczbę ogłoszeń
# -liczbę w zależnośći od rocznika,typu napędu,skrzyni biegów,rodzaju paliwa,typu
# nadwozia.
# -cenę w zależności od rocznika,napędu,skrzyni biegów,rodzaju paliwa,typu nadwozia.

# -Opisz, krótko który samochód zaproponujesz i dlaczego. Do
# raportu dołącz zdjęcie poglądowe pojazdu.

### 4. Czy średnia cena zaproponowanego pojazdu różni się
# statysycznie od ceny takiego samego pojazdu ze staryszch
# ogłoszeń - z pliku auta2.csv (equality of means)

library(pryr)
library(odbc)
library(DBI)
library(RSQL)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(rstudioapi)
library(dplyr)
library(RMySQL)
library(stringr)

### 1. Przefiltruj dane z bazy wybierając najnowsze ogłoszenia, daty
# wybierz z tabeli data.

# Poniżej dane zostały poprane z bazy i wczytane do pliku csv (zakomentowany fragment kodu):

# con <- DBI::dbConnect(RMySQL::MySQL(),
#                       encoding ="UTF-8",
#                       host = "11.11.111.11",
#                       user = "student",
#                       dbname = "rzajecia23",
#                       password ="haslo"#rstudioapi::askForPassword("Database password")
# )

# dbGetQuery(con,'SET NAMES utf8')
# dbGetQuery(con,'set character set "utf8"')
# dbListTables(con)

# auta<-tbl(con,"autaDB")
# auta1 <- auta %>% collect()
# write.csv(auta1, "auta1.csv")
# View(auta1)

auta1 <- read.csv("auta1.csv")
auta2 <- read.csv("auta2.csv")
View(auta1)

# Dane:
# auta1 15.01.2023r
# auta2 kwiecień 2023r

### 2.Wybierz kolumny:
# cena,Przebieg,Rok.produkcji,Marka.pojazdu, Model.pojazdu,Wersja,Generacja,Rodzaj.paliwa,
# Pojemność.skokowa,Moc,Skrzynia.biegów, Napęd,Typ.nadwozia,Liczba.drzwi,Liczba.miejsc,
# Bezwypadkowy,Uszkodzony,Stan,Kolor

auta1<-auta1%>%select(cena,Przebieg,Rok.produkcji,Marka.pojazdu,Model.pojazdu,Wersja,Generacja,Rodzaj.paliwa,Pojemność.skokowa,Moc,Skrzynia.biegów, Napęd,Typ.nadwozia,Liczba.drzwi,Liczba.miejsc,Bezwypadkowy,Uszkodzony,Stan,Kolor)

# Oraz ‘napraw’ kolumny :Przebieg, Pojemność.skokowa,Moc

auta1 <- auta1%>%mutate( Przebieg = as.numeric( Przebieg%>%str_replace_all("km","")%>%str_replace_all(" ","") ) )
auta1 <- auta1%>%mutate( Pojemność.skokowa = as.numeric( Pojemność.skokowa%>%str_replace_all("cm3","")%>%str_replace_all(" ","") ) )
auta1 <- auta1%>%mutate( Moc = as.numeric( Moc%>%str_replace_all("KM","")%>%str_replace_all(" ","") ) )


### 3.Wykonaj raport RMarkdown (kod R uwzględniony w raporcie)
# oraz zaproponuj najlepszy według Ciebie samochód z
# dostępnych ogłoszeń

### Wybrane do zadania modele: wszystkie modele marki BMW (271 ogłoszeń):
autaBMW<- auta1%>%filter(Marka.pojazdu=="BMW")
View(autaBMW)

# -samochód powinien być nieuszkodzony
# -rocznik od 2013.
# -budżet klienta to 60000 zł.

autaBMW <- autaBMW%>%filter(is.na(Uszkodzony))
autaBMW <- autaBMW%>%select(cena,Przebieg,Rok.produkcji,Marka.pojazdu,Model.pojazdu,Wersja,Generacja,Rodzaj.paliwa,Pojemność.skokowa,Moc,Skrzynia.biegów, Napęd,Typ.nadwozia,Liczba.drzwi,Liczba.miejsc,Bezwypadkowy,Stan,Kolor)
autaBMW <- autaBMW%>%filter(Rok.produkcji>2013)
autaBMW <- autaBMW%>%mutate( cena = as.numeric( cena%>%str_replace_all(" ","") ) )
autaBMW <- autaBMW%>%filter(cena<60000)


# -Przedstaw w tabeli: średnie ceny, przebieg, rocznik, liczbę ogłoszeń wybranych
# modeli.
# -Przedstaw w tabeli: mediany cen, przebiegu, rocznika, liczbę ogłoszeń wybranych
# modeli.

tabelameanBMW <- autaBMW%>%group_by(Model.pojazdu)%>%summarise(średnia_cena=mean(cena,na.rm=TRUE),
                                                               średni_przebieg=mean(Przebieg,na.rm=TRUE),
                                                               średni_rocznik=mean(Rok.produkcji,na.rm=TRUE),
                                                               liczba_ogłoszeń=n())%>% arrange(desc(średnia_cena))
View(tabelameanBMW)

tabelamedianBMW <- autaBMW%>%group_by(Model.pojazdu)%>%summarise(średnia_cena=median(cena,na.rm=TRUE),
                                                                 średni_przebieg=median(Przebieg,na.rm=TRUE),
                                                                 średni_rocznik=median(Rok.produkcji,na.rm=TRUE),
                                                                 liczba_ogłoszeń=n())%>% arrange(desc(średnia_cena))
View(tabelamedianBMW)

# -Na wykresach przedstaw dla wybranych modeli:
# -liczbę ogłoszeń
# -liczbę w zależnośći od rocznika,typu napędu,skrzyni biegów,rodzaju paliwa,typu
# nadwozia.
# -cenę w zależności od rocznika,napędu,skrzyni biegów,rodzaju paliwa,typu nadwozia.

# Dla marki BMW (czyli dla tabeli tabelameanBMW) można, przykładowo,  przedstawić na wykresach:

p1 <- ggplot(tabelameanBMW, aes(y=liczba_ogłoszeń,x=Model.pojazdu)) + 
  geom_bar(stat = "identity", color="blue") + 
  theme(legend.position = 'none')


p2 <- ggplot(tabelameanBMW, aes(y=średnia_cena,x=Model.pojazdu)) + 
  geom_bar(stat = "identity", color="blue") + 
  theme(legend.position = 'none')

p3 <- ggplot(tabelameanBMW, aes(x=średni_rocznik, y=średnia_cena)) + 
  geom_line(size=2) + 
  theme(panel.background = element_blank(),
        axis.line = element_line())

p4 <- ggplot(tabelameanBMW, aes(x=średni_przebieg, y=średnia_cena)) + 
  geom_line(size=2) + 
  theme(panel.background = element_blank(),
        axis.line = element_line())

grid.arrange(p1, p2, p3, p4, nrow = 2)

# -Opisz, krótko który samochód zaproponujesz i dlaczego. Do
# raportu dołącz zdjęcie poglądowe pojazdu.

# Można zaprponować Serię 2 BMW:
# Ma średnią cenę mieszczącą się w budżecie: 54273.53  zł,
# nie najwyższy średni przebieg: 155301.8 km,
# średni rocznik to 2015.500,
# można wybierać z 40 ogłoszeń.

zdjecie <- readPNG("model_zdjecie.png")

ggdraw() +
  draw_image(zdjecie)

### 4. Czy średnia cena zaproponowanego pojazdu różni się
# statysycznie od ceny takiego samego pojazdu ze staryszch
# ogłoszeń - z pliku auta2.csv (equality of means)

# Nie ma modelu BMW w starszych ogłoszeniach, weźmy więc model Audi:

auta1 <- read.csv("auta1.csv")
auta2 <- read.csv("auta2.csv")

new <- auta1%>%select(cena,Marka.pojazdu)
new <- new%>%filter(Marka.pojazdu=="Audi")
new <- new%>%mutate( cena = as.numeric( cena%>%str_replace_all(" ","") ) )
new <- new %>%filter(!is.na(cena))

old <- auta2%>%filter(Marka.pojazdu=="Audi")
old <- old%>%select(cena)
old <- old%>%mutate( cena = as.numeric( cena%>%str_replace_all(".",",") ) )
old <- old %>%filter(!is.na(cena))

t.test(new, old, var.equal = FALSE)

# Jeśli p-value jest mniejsza od poziomu istotności alpha=0.05,
# odrzucamy hizpotezę zerową H0, że średnie cen są równe.
# Oznacza to, że nowe ceny średnie Audi różnią się od starych.
