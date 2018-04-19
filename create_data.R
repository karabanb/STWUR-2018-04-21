# na które mieszkania stać Wrocławian?
# 
# Według GUS (http://wroclaw.stat.gov.pl/) przeciętne (średnie)
# wynagrodzenie we Wrocławiu wyniosło 4800,54 zł brutto w pazdzierniku 2016.
# To jest 3 411,57 zł netto
pensja <- 3411.57
#odjmiemy 1500 złotych na życie (jedzenie, ubrania, rozrywkę)
max_rata <- pensja - 1500

#zalozmy kredyt na 5% rocznie 

# rata = kwota kredytu * (1 + procent/12)^ liczba_rat * ((1 + procent/12)-1)/((1 + procent/12)^liczba_rat-1)
liczba_rat <- 25*12
r <- 0.05
max_rata/(1 + r/12)^liczba_rat/((1 + r/12)-1)*((1 + r/12)^liczba_rat-1)

#wychodzi 327K kredytu

library(dplyr)
read.csv(file = "https://raw.githubusercontent.com/STWUR/STWUR-2017-06-07/master/data/mieszkania_dane.csv", 
         encoding = "UTF-8") %>% 
  na.omit %>% 
  mutate(cena = metraz*cena_m2) %>%
  mutate(tanie = cena < 300000) %>%
  select(-cena) %>% 
  write.csv(file = "mieszkania_mlr.csv", row.names = FALSE)
