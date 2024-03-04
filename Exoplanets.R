library(ggplot2) # głównie do rysowania wykresów
library(dplyr) # praca z pakietem opiera się na operatorze %>%
library(tidyverse)
library(mice)
library(corrplot)
library(CGPfunctions)

str(exoplanety)

# Zamieniam dane kategoryczne na typ factor
exoplanety$planet_type <- as.factor(exoplanety$planet_type)
exoplanety$mass_wrt <- as.factor(exoplanety$mass_wrt)
exoplanety$radius_wrt <- as.factor(exoplanety$radius_wrt)
exoplanety$detection_method <- as.factor(exoplanety$detection_method)
str(exoplanety)
head(exoplanety)


summary(exoplanety) # mamy trochę wartości NA więc postaramy się ich pozbyć
# usuwamy całe wiersze, w których występuje jakaś wartość NA
exo_cleaned <- na.omit(exoplanety)



# Zapoznajemy się z rozkładem zmiennych


str(exo_cleaned)
head(exo_cleaned)

summary(exo_cleaned)
#distance
exo_cleaned %>%
  ggplot() +
  geom_density(aes(x = distance), color = "blue", size = 1.25) +
  labs(title = "Odległości planet od Ziemi w latach świetlnych",
       x = "odległość",
       y = "gęstość")

#stellar_magnitude
exo_cleaned %>%
  ggplot() +
  geom_density(aes(x = stellar_magnitude), color = "blue", size = 1.25) +
  labs(title = "Wykres funkcji gęstości jasności planet",
       x = "jasność",
       y = "gęstość")



#typ planety
exo_cleaned %>%
  ggplot(aes (x = planet_type)) +
  geom_bar(fill = "lightblue",
           colour = "darkblue") +
  labs(title = "Wykres kolumnowy z ilościami obserwacji dla każdego typu planety",
       x = "typ planety",
       y = "liczba obserwacji")

#rok odkrycia
# Używam wykresu słupkowego, ponieważ interesuje mnie ile mniej
#więcej planet zostało odkrytych w danych roku
exo_cleaned %>%
  ggplot() +
  geom_histogram(aes(x = discovery_year), fill = "lightblue", color = "darkblue") +
  labs(title = "Rok odkrycia planety",
       x = "rok odkrycia",
       y = "ilość planet")

#masa
#przycinamy wartości drastycznie odstające
exo_bez_outlierow_masy <- exo_cleaned[exo_cleaned$mass_multiplier <
                                   quantile(exo_cleaned$mass_multiplier, 0.95),]

exo_bez_outlierow_masy %>%
  rename(planeta_bazowa = mass_wrt) %>%
  ggplot(aes(x = mass_multiplier,
             fill = planeta_bazowa)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Masa względem planety bazowej",
       x = "mnożnik masy",
       y = "gęstość")


#promień
# dodajemy mutate żeby znormalizować wartości gęstości (powinny należeć do przedziału [0,1])
exo_bez_outlierow_promien <- exo_cleaned[exo_cleaned$radius_multiplier <
                                           quantile(exo_cleaned$radius_multiplier, 0.95),]


exo_bez_outlierow_promien %>%
  rename(planeta_bazowa = radius_wrt) %>%
  ggplot(aes(x = radius_multiplier, fill = planeta_bazowa)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Promień względem planety bazowej",
       x = "mnożnik promienia",
       y = "gęstość")


#promień orbity
exo_bez_outlierow_promienia_orbity <- exo_cleaned[exo_cleaned$orbital_radius <
                                             quantile(exo_cleaned$orbital_radius, 0.95),]
exo_bez_outlierow_promienia_orbity %>%
  ggplot() +
  geom_boxplot(aes(x = orbital_radius), color = "blue") +
  labs(title = "Wykres pudełkowy dla promienia orbity planety ",
       x = "promień orbity",
       y = "gęstość")

# czas obiegu wokół gwiazdy
exo_bez_outlierow_czasu_obiegu <- exo_cleaned[exo_cleaned$orbital_period <
                                                    quantile(exo_cleaned$orbital_period, 0.9),]
exo_bez_outlierow_czasu_obiegu %>%
  ggplot() +
  geom_boxplot(aes(x = orbital_period), color = "blue") +
  labs(title = "Wykres pudełkowy dla czasu obiegu wokół gwiazdy",
       x = "czas obiegu w latach ziemskich",
       y = "gęstość")




# ecentryczność
exo_cleaned %>%
  ggplot() +
  geom_boxplot(aes(x = eccentricity), color = "blue") +
  labs(title = "Wykres pudełkowy ecentryczności orbity planet",
       x = "ekscentryczność",
       y = "gęstość")

#metoda odkrycia planety
exo_cleaned %>%
  ggplot(aes(y = detection_method )) +
  geom_bar(fill = "darkgreen",
           colour = "darkgreen") +
  labs(title = "Wykres kolumnowy z ilościami obserwacji dla każdej metody odkrycia planety",
       y = "metoda odkrycia",
       x = "liczba obserwacji")
# Przez to, że zdecydowanie najczęstszą metodą odkrywania planet jest TRANSIT i RADIAL VELOCITY
# to mało dowiadujemy się o innych metodach
# Narysujmy powyższy wykres jeszcze raz ale bez tych dwóch metod

exo_cleaned %>%
  filter(!detection_method %in% c("Transit", "Radial Velocity")) %>%
  ggplot(aes(y = detection_method )) +
  geom_bar(fill = "darkgreen",
           colour = "darkgreen") +
  labs(title = "Wykres kolumnowy z ilościami obserwacji dla każdej metody odkrycia planety",
       y = "metoda odkrycia",
       x = "liczba obserwacji")

# Teraz widzimy, że inne metody odkrywania planet to zdecydowanie rzadkość


#1. Czy typ planety wpływa na jej jasność?

tapply(exo_cleaned$stellar_magnitude, exo_cleaned$planet_type, summary)

exo_cleaned %>%
  ggplot(aes(x = planet_type, y = stellar_magnitude, color = planet_type)) +
  geom_jitter() +
  labs(title = "Zależność między typem planety a jasnością",
       x = "Typ planety",
       y = "Jasność")

exo_cleaned$planet_type[which.max(sd(exo_cleaned$stellar_magnitude))]


#jasność w zależności od roku odkrycia
exo_cleaned %>%
  rename(typ_planety = planet_type) %>%
  ggplot(aes(x = discovery_year, y = stellar_magnitude, col = typ_planety)) +
  geom_line(size = 1.5) +
  labs(title = "Jasność w zależności od roku odkrycia",
       x = "rok odkrycia", 
       y = "jasność") +
facet_wrap(~typ_planety)

cor(exo_cleaned$discovery_year, exo_cleaned$stellar_magnitude, method = "spearman")


# Brak zależności między rokiem odkrycia a jasnością planet


# jasność w zależności od dystansu od zieni
exo_cleaned %>%
  rename(typ_planety = planet_type) %>%
  ggplot(aes(x = distance, y = stellar_magnitude, col = typ_planety)) +
  geom_line(size = 1.5) +
  labs(title = "Jasność w zależności od roku odkrycia",
       x = "rok odkrycia", 
       y = "jasność") +
  facet_wrap(~typ_planety)

cor(exo_cleaned$distance, exo_cleaned$stellar_magnitude, method = "spearman")



#2. Jakie są zależności między masą i promieniem planety a jej typem?

# Wyświetlenie podstawowtch statystyk opisowych dla masy planet, w zależności
# od bazy mnożnika masy i typu planety
masa_typ_baza <- exo_cleaned %>%
  select(mass_multiplier, mass_wrt, planet_type) %>%
  group_by(mass_wrt, planet_type) %>%
  summarize(sr_masa = mean(mass_multiplier),
            me_masa = median(mass_multiplier),
            max_masa = max(mass_multiplier),
            min_masa = min(mass_multiplier),
            sd_masa = sd(mass_multiplier),
            liczba_obs = n())
masa_typ_baza

# Jaka jest zależność między masą planety a jej typem?
exo_bez_outlierow_masy %>%
  rename(typ_planety = planet_type, 
         mnoznik_masy = mass_wrt) %>%
  ggplot(aes(y = mass_multiplier, col = typ_planety)) +
  geom_boxplot() +
  labs(title = "Masa w zależności od podstawy mnożnika masy i typu planety",
       x = "podstawa mnożnika masy", 
       y = "masa") +
  facet_wrap(~mnoznik_masy) +
  theme(axis.text.x = element_blank())

#Zobaczmy jak procentowo to wygląda
exo_bez_outlierow_masy %>%
  rename(typ_planety = planet_type, 
         mnoznik_masy = mass_wrt) %>%
  ggplot() +
  geom_bar(aes(x=mnoznik_masy, fill = typ_planety),
           color = "white",
           position = "fill") +
  labs(title = "Masa w zależności od podstawy mnożnika masy i typu planety",
       x = "podstawa mnożnika masy", 
       y = "masa") +
  facet_wrap(~mnoznik_masy) +
  theme(axis.text.x = element_blank())



#Bardzo dużą masę mają planety podobne do Neptuna, dla których podstawą mnożnika masy
# jest Ziemia


promien_typ_baza <- exo_cleaned %>%
  select(radius_multiplier, radius_wrt, planet_type) %>%
  group_by(radius_wrt, planet_type) %>%
  summarize(sr_promien = mean(radius_multiplier),
            me_promien = mean(radius_multiplier),
            max_promien = max(radius_multiplier),
            min_promie = min(radius_multiplier),
            sd_promien = sd(radius_multiplier),
            liczba_obs = n())
promien_typ_baza


exo_bez_outlierow_promien %>%
  rename(typ_planety = planet_type, 
         mnoznik_promienia = radius_wrt) %>%
  ggplot(aes(y = radius_multiplier, col = typ_planety)) +
  geom_boxplot() +
  labs(title = "Promień w zależności od podstawy mnożnika promienia i typu planety",
       x = "podstawa mnożnika promienia", 
       y = "promień") +
  facet_wrap(~mnoznik_promienia) +
  theme(axis.text.x = element_blank())

# 3. Czy większy promień oznacza większą masę?

exo_bez_outlierow_promien %>%
  rename(mnoznik_masy = mass_wrt) %>%
  group_by(mnoznik_masy, radius_wrt) %>%
  ggplot(aes(x = radius_multiplier, y = mass_multiplier, col = mnoznik_masy)) +
  geom_line() +
  labs(title = "Zależność masy od promienia",
       x = "promień", 
       y = "masa") +
  facet_wrap(~radius_wrt)

cor(exo_cleaned$mass_multiplier, exo_cleaned$radius_multiplier, method = "spearman")


# Z wykresu widać że większy promień wcale nie oznacza większej masy

# 4. Jaka jest zależność między promieniem orbity a czasem obiegu planety wokół gwiazdy
# według typów planet?

exo_bez_outlierow_promienia_orbity %>%
  rename(typ_planety = planet_type) %>%
  ggplot(aes(x = orbital_radius, y = orbital_period, col = typ_planety)) +
  geom_line() +
  labs(title = "Zależność promienia orbity od czasu obiegu wokół gwiazdy",
       x = "promień orbity",
       y = "czas obiegu") +
  facet_wrap(~typ_planety)


cor(exo_cleaned$orbital_radius, exo_cleaned$orbital_period, method = "spearman")
# korelacja spearmana, ponieważ mamy wiele wartości odstających 

# Widać zależność między promieniem orbity a czasem obiegu wokół gwiazdy.
# Im większy promień orbity typ większy czas obiegu wokół gwiazdy



# A w takim razie czy istnieje zależność między ecentrycznością
# a czasem obiegu wokół gwiazdy

exo_bez_outlierow_czasu_obiegu %>%
  rename(typ_planety = planet_type) %>%
  ggplot() +
  geom_line(aes(x = eccentricity, y = orbital_period, col = typ_planety)) +
  labs(title = "Zależność ecentryczności orbity od czasu obiegu wokół gwiazdy",
       x = "ecentryczność orbity",
       y = "czas obiegu") +
  facet_wrap(~typ_planety)

cor(exo_cleaned$eccentricity, exo_cleaned$orbital_period, method = "spearman")
# Słaba zależność liniowa między ecentrycznością orbity a czasem obiegu wokół gwiazdy


# Sprawdżmy na podstawie macierzy korelacji czy istnieją korelacje między tymi 3 zmiennymi:
wybrane_dane <- exo_cleaned[, c("eccentricity", "orbital_period", "orbital_radius")]
macierz_korelacji = cor(wybrane_dane, method = "spearman")
round(macierz_korelacji, 2)

corrplot(macierz_korelacji, method = "color", addCoef.col = "purple", tl.cex = 0.7, tl.col = "black")
# Bardzo silna zależność między promieniem orbity a czasem obiegu wokół gwiazdy
# Słaba zależność między ecentrycznością a czasem obiegu wokół gwiazdy
# Słaba zależność między ecentrycznością a promieniem orbity



# 5. Zbadajmy teraz różne anomalie jakie występują we wszechświecie

# Wyświetlam informacje o planecie z największą jasnością
exo_cleaned[exo_cleaned$stellar_magnitude == max(exo_cleaned$stellar_magnitude),]

# Wyświetlam informacje o planecie z najmniejszą jasnością
exo_cleaned[exo_cleaned$stellar_magnitude == min(exo_cleaned$stellar_magnitude),]
# Obie z wyżej wyświetlonych planet to Gazowe Giganty

# Jakimi metodami były odkrywane planety znajdujące się najdalej od Ziemi a jakimi te
# znajdujące się najbliżej niej


# 7. Do jakich planet najszybciej byśmy dolecieli? I może zamieszkali (?)

najblizsze_planety <- exo_cleaned %>%
  filter(planet_type == "Super Earth") %>%
  arrange(distance) %>%
  head(10, distance)
najblizsze_planety

najblizsze_planety %>%
  ggplot(aes(y = reorder(name, distance), x = distance, fill = detection_method)) +
  geom_col() +
  labs(title = "10 najbliższych planet",
       x = "dystans", 
       y = "nazwa planety")

# Znajdźmy planety, których czas obiegu wokół gwiazdy i promień orbity jest najbardziej zbliżony do Ziemi
closest_orbital_period <- najblizsze_planety[which.min(abs(najblizsze_planety$orbital_period - 1.0)), ]
closest_orbital_period

closest_orbital_radius <- najblizsze_planety[which.min(abs(najblizsze_planety$orbital_radius - 1.0)), ]
closest_orbital_radius

# dla porównania - najdalsze planety
exo_cleaned %>%
  select(distance, detection_method, name) %>%
  arrange(desc(distance)) %>%
  head(10, distance) %>%
  ggplot(aes(y = reorder(name, distance), x = distance, fill = detection_method)) +
  geom_col() +
  labs(title = "10 najdalszych planet",
       x = "dystans", 
       y = "nazwa planety")



# 6. Jaka jest zależność między jasnością planety a jej odległością od Ziemi

exo_cleaned %>%
  rename(typ_planety = planet_type) %>%
  ggplot(aes(x = distance, y = stellar_magnitude, col = typ_planety)) +
  geom_point(size = 1.5) +
  labs(title = "Jasność w zależności od odległości od Ziemi",
       x = "dystans", 
       y = "jasność") +
  facet_wrap(~typ_planety)


cor(exo_cleaned$distance, exo_cleaned$stellar_magnitude, method = "spearman")
# Coś wysoko xd



# 8. Czy planety giganty przez to, że są duże to poruszają się wolniej?



# 9. Jak zmienia się średnia liczba planet odkrytych daną metodą na przestrzeni lat?
exo_cleaned %>%
  group_by(discovery_year, detection_method) %>%
  summarise(liczba_planet = n()) %>%
  ggplot(mapping = aes(x = discovery_year, y = liczba_planet, color = detection_method)) +
  geom_line() +
  geom_point() +
  labs(title = "Liczba planet uzyskanych daną metodą na przestrzeni lat",
       x = "Rok",
       y = "Liczba planet") +
  theme_minimal()


# Zależność między liczba planet a metoda bez transit i radial velocity

exo_cleaned %>%
  filter(!detection_method %in% c("Transit", "Radial Velocity")) %>%
  group_by(discovery_year, detection_method) %>%
  summarise(liczba_planet = n()) %>%
  ggplot(mapping = aes(x = discovery_year, y = liczba_planet, color = detection_method)) +
  geom_line() +
  geom_point() +
  labs(title = "Liczba planet odkrytych daną metodą na przestrzeni lat",
       x = "Rok",
       y = "Liczba planet") +
  theme_minimal()


# 10. Zależność między odległością od Ziemi a metodą wykrycia planet

exo_cleaned %>%
  group_by(distance, detection_method) %>%
  summarise(liczba_planet = n()) %>%
  ggplot(mapping = aes(x = distance, y = liczba_planet, color = detection_method)) +
  geom_point() +
  labs(title = "Odległość od Ziemi a metoda odkrywania",
       x = "Dystans",
       y = "Liczba planet",
       color = "Metoda odkrywania") +
  theme_minimal()

exo_cleaned %>%
  filter(detection_method %in% c("Gravitational Microlensing", "Transit")) %>%
  group_by(distance, detection_method) %>%
  summarise(liczba_planet = n()) %>%
  ggplot(mapping = aes(x = distance, y = liczba_planet, color = detection_method)) +
  geom_point() +
  labs(title = "Odległość od Ziemi a metoda odkrywania",
       x = "Dystans",
       y = "Liczba planet",
       color = "Metoda odkrywania") +
  theme_minimal() +
  facet_wrap(~detection_method)

exo_cleaned %>%
  filter(!detection_method %in% c("Gravitational Microlensing", "Transit")) %>%
  group_by(distance, detection_method) %>%
  summarise(liczba_planet = n()) %>%
  ggplot(mapping = aes(x = distance, y = liczba_planet, color = detection_method)) +
  geom_point() +
  labs(title = "Odległość od Ziemi a metoda odkrywania",
       x = "Dystans",
       y = "Liczba planet",
       color = "Metoda odkrywania") +
  theme_minimal() +
  facet_wrap(~detection_method)


# 11. Zależność między typem planety a metodą odkrycia

exo_cleaned %>%
  ggplot() +
  geom_bar(aes(x = planet_type,
               fill = detection_method),
           color = "black", 
           position = "dodge2") +
  theme_light() +
  labs(title = "Typ planety a metoda odkrycia",
       x = "typ planety",
       y = "liczba planet",
       fill = "metoda odkrycia")

# bez transit i radial velocity
exo_cleaned %>%
  filter(!detection_method %in% c("Transit", "Radial Velocity")) %>%
  ggplot() +
  geom_bar(aes(x = planet_type,
               fill = detection_method),
           color = "black", 
           position = "dodge2") +
  theme_light() +
  labs(title = "Typ planety a metoda odkrycia",
       x = "typ planety",
       y = "liczba planet",
       fill = "metoda odkrycia")

