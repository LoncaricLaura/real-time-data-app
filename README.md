# real-time-data-processing-app

Posljednjih godina obrada podataka u stvarnom vremenu dobila je značajnu važnost zbog potrebe za analizom kontinuiranih tokova podataka u raznim područjima poput praćenja vremena, IoT sustava i automatizacije. Tema ovog projekta je izgradnja funkcionalne aplikacije za obradu podataka u stvarnom vremenu, s fokusom na prikupljanje podataka o okolišu tijekom duljeg razdoblja i provođenje statističke analize. Glavni cilj je učinkovito upravljanje velikim tokovima podataka i njihovo prikazivanje na intuitivan način. Sustav koristi Raspberry Pi za prikupljanje podataka i Haskell za obradu i analizu. 


## Arhitektura sustava i dizajn procesora za streaming

Sustav za obradu podataka u stvarnom vremenu koristi integrirani pristup za učinkovito prikupljanje, pohranu, obradu, distribuciju i analizu podataka [10]. Streaming arhitektura omogućuje da se podaci kontinuirano obrađuju i ažuriraju, bez prekida u toku, čime se omogućuje pravovremena reakcija na promjene u okolišnim uvjetima.

<b>Senzor i Prikupljanje Podataka:</b> Streaming proces započinje s DHT11 senzorom, koji mjeri temperaturu i vlagu svakih 30 minuta. Raspberry Pi služi kao središnji uređaj za povezivanje s DHT11 senzorom putem GPIO pinova i pokreće Python skriptu koja automatski prikuplja podatke. Prikupljeni podaci se pohranjuju u logove na disku, svaki zapis sadrži vremensku oznaku, temperaturu i vlagu, što omogućuje detaljno praćenje i kasniju analizu.

<b>Obrada i Vizualizacija Podataka:</b> Prikupljeni podaci se obrađuju korištenjem funkcionalnih pristupa u Haskellu što omogućuje manipulaciju tokovima podataka. Funkcije poput map i filter koriste se za kontinuiranu transformaciju i filtriranje podataka, dok se agregacija koristi za izračun statističkih pokazatelja kao što su prosječna temperatura i vlažnost. Haskell je odabran jer omogućuje visoku razinu modularnosti i efikasnosti za obradu velikih skupova podataka. Za analizu se generiraju grafički prikazi, uključujući grafikone i histograme, koji vizualiziraju promjene u temperaturi i vlagi tijekom vremena.

<b>API i Server:</b> Sustav koristi [Flask API](https://github.com/mvisnjic/project_iot) koji omogućuje kontrolu funkcionalnosti sustava kao što su kontrola svijetla i pristup podacima putem REST API-ja. Flask aplikacija se implementira i poslužuje uz pomoć [WSGI Gunicorn](https://flask.palletsprojects.com/en/3.0.x/deploying/gunicorn/) servera, koji omogućava učinkovito pokretanje aplikacije u produkcijskom okruženju. [Nginx servera](https://docs.gunicorn.org/en/latest/deploy.html) služi kao web server i reverse proxy, upravljajući HTTP zahtjevima i raspodjelom opterećenja između poslužitelja. Omogućuje obradu i distribuciju zahtjeva kroz sustav, optimizirajući performanse streaminga podataka i pružajući stabilnost i pouzdanost za aplikacije koje zahtijevaju rad u stvarnom vremenu.


## Funkcionalnosti

### Funkcije za prikupljanje podataka
<b>fetchDataWithParams:</b> ključna funkcija koja omogućuje dohvaćanje podataka s API-ja uz specificiranje parametara kao što su datum i vrijeme. Ova funkcija podržava fleksibilnost u određivanju vremenskog okvira te olakšava dohvaćanje prilagođenih skupova podataka.

<b>getDataForYesterday, getDataForLast7Days i getDataForPreviousMonth:</b> dohvaćaju podatke za specifične vremenske intervale (jučer, posljednjih sedam dana i prethodni mjesec). Ove funkcije automatiziraju proces prikupljanja podataka za određena razdoblja, eliminirajući potrebu za ručnim unosom parametara. Time se postiže jednostavnost i efikasnost u radu s vremenski ograničenim podacima.

### Funkcije za statističku analizu
<b>calculateStatistics:</b> izračunava osnovne statističke mjere poput prosjeka, maksimalne i minimalne vrijednosti za temperaturu i vlažnost zraka. Osim toga, ova funkcija izračunava standardnu devijaciju, koeficijent varijacije te korelaciju između temperature i vlage, što omogućava uvid u povezanost ovih dvaju parametara.

<b>average, mean, standardDeviation, coefficientOfVariation, correlation:</b> pomoćne funkcije koje omogućavaju računanje različitih statističkih mjera, što je ključno za analizu podataka i donošenje zaključaka o ponašanju okolišnih uvjeta tijekom određenih vremenskih perioda.

### Funkcije za vizualizaciju podataka
<b>chartWeeklyStatistics, chartMonthlyStatistics:</b> prikazuju prosječne vrijednosti temperature i vlage kroz tjedan ili mjesec, koristeći dijagrame. Ovi vizualni prikazi omogućuju praćenje vremenskih obrazaca i varijacija u okolišnim uvjetima.

<b>plotTemperatureHistogram, plotHumidityHistogram:</b> generiraju histogram raspodjele temperature i vlage. Histogrami su ključni za prikaz distribucije podataka u vizualnom obliku i omogućuju otkrivanje eventualnih nepravilnosti ili odstupanja.


### Funkcija za izračun toplinskog indeksa
<b>heatIndex:</b> Funkcija koja računa "heat index" (osjećaj temperature uzimajući u obzir vlažnost). Pruža realniji uvid u stvarni osjećaj temperature, što je posebno važno u analizama okoline, gdje kombinacija visoke temperature i visoke vlažnosti može značajno utjecati na udobnost i sigurnost ljudi.


## Analiza kako funkcijsko programiranje poboljšava obradu podataka u stvarnom vremenu

Funkcijsko proramiranje je programska paradigma koja se fokusira na korištenje čistih matematičkih funkcija [5]. Funkcije poput average, correlation i mean su primjeri čistih funkcija. Čista funkcija je funkcija u kojoj je povratna vrijednost određena samo njezinim ulaznim vrijednostima, bez vidljivih nuspojava [2]. 

Jedan od ključnih aspekata funkcijskog programiranja je imutabilnost podataka što znači da se podaci ne mijenjaju direktno, već kreiraju nove instance podataka za svaku promijenu. Primjerice, kada se obrađuju liste s podacima o temperatiru I vlazi, svaki novi izračun ne mijenja postojeću listu, već generira novu. Ovaj pristup značajno smanjuje rizik od neočekivanih pogrešaka uzrokovanih promjenom stanja podataka.

Osim toga, funkcijsko programiranje naglašava bezstanje operacija. Funkcije obrađuju ulazne podatke i vraćaju rezultate bez utjecaja na ostatak sustava, što pojednostavljuje razumijevanje I održavanje koda.

Funkcije višeg reda su funkcije koje uzimaju druge funkcije kao argumente ili vraćaju funkcije kao rezultate [5].  Funkcije poput map I filer, omogućuju fleksibilnu obradu podataka. Ove funkcije višeg reda koriste se za transformaciju i filtriranje tokova podataka, čime se omogućuje dinamička i prilagodljiva obrada informacija u stvarnom vremenu.

Funkcionalni pristup omogućava kompoziciju funkcija, gdje se funkcije kombiniraju za stvaranje složenih operacija [13]. Kod je obično čitljiviji jer se fokusira na transformacije podataka, a ne na promjene stanja.

Funkcijsko programiranje nudi fleksibilnost, predvidljivost i učinkovitost, čineći ga idealnim izborom za aplikacije koje zahtijevaju brzu i pouzdanu obradu podataka u stvarnom vremenu.


### Diskusija o prednostima i izazovima u korištenju funkcijskog pristupa za streaming

Korištenje funkcijskog pristupa za aplikacije koje obrađuju podatke u stvarnom vremenu donosi niz prednosti, ali i izazova. Zbog imutabilnosti funkcijsko programiranje smanjuje rizik od pogrešaka izazvanih promjenom stanja, što je ključno za održavanje stabilnosti sustava prilikom analize vremenskih serija podataka, poput temperature i vlažnosti. Na primjer, svaki put kada se obradi nova vrijednost o temperaturi i vlagi, stvara se nova instanca podataka, bez utjecaja na prethodne vrijednosti.

Funkcionalno programiranje potiče stvaranje malih, čistih funkcija koje su ponovno iskoristive, čime se olakšava održavanje i proširenje softvera tijekom vremena [1]. Prilagođeno je za istodobno i paralelno izvršavanje zadataka, što je važno za skalabilnost aplikacija koje obrađuju velike količine podataka u stvarnom vremenu [1]. Podaci prikupljeni sa DHT11 senzorom mogu se istovremeno obrađivati i analizirati koristeći čiste funkcije, čime se smanjuje složenost i potreba za ručnim upravljanjem stanjima. Zbog čistih funkcija koje obrađuju temperaturne podatke uvijek vraćaju iste statističke rezultate ako im se proslijede isti ulazni podaci.

Još jedna prednost funkcijskog pristupa je jednostavnije testiranje [1]. Funkcije višeg reda omogućuju brzu konstrukciju, modifikaciju i refaktoriranje programa, što dovodi do potencijalnih prednosti produktivnosti [7]. Funkcionalni jezici dodatno pojačavaju prednosti funkcijskog pristupa zahvaljujući jakom sustavu tipova, koji omogućuje hvatanje grešaka već u fazi sastavljanja koda, čime se poboljšava sigurnost i pouzdanost softvera [1]. 

Iako nudi brojne prednosti, postoje i izazovi. Jedan od izazova je učenje. funkcijsko programiranje zahtijeva drugačiji način razmišljanja u odnosu na imperativni stil, što može biti izazov za programere nisu upoznati s ovom paradigmom. S obzirom da se funkcionlani jezici koriste manje od imperativnih, postoji manje okvira, alata i paketa što može stvoriti probleme pri implementaciji složenijih aplikacija. Budući da se u funkcijskom programiranju varijable ne mijenjaju, već stvaraju nove, to znači dvostruko korištenje memorije što može utjecati na performanse sustava u slučaju obrade velikih količina podataka [9].


### Pregled i usporedba s tradicionalnim pristupima u obradi podataka u stvarnom vremenu

Imperativno programiranje je paradigma u kojoj se navodi kako program treba postići željeni rezultat [11]. Izvodi izračunavanje kao slijed izjava koje manipuliraju pohranjenim podacima dok se ne postigne željeni rezultat [4]. Takav pristup, može postati ne fleksibilan i teško skalabilan u aplikacijama za obradu podataka u stvarnom vremenu, gdje su potrebne visoke performanse i niska latencija. Imperativni programi koriste sekvencijalnu obradu podataka, što može dovesti do problema s latencijom jer svaka operacija mora biti izvršena prije nego što se pređe na sljedeću. Kôd imperativnog programiranja djeluje poput recepta koji mijenja stanje računala u svakoj liniji, stvarajući i ponovo iskorištavajući varijable [8].

S druge strane, deklarativno programiranje je paradigma koja se usredotočuje na ono što program treba postići, bez navođenja kako ga postići [11]. Ovakav pristup omogućuje fleksibilnije upravljanje resursima i prilagodbu sustava promjenjivim zahtjevima obrade podataka u stvarnom vremenu. Često koriste funkcije za filtriranje, agregaciju i distribuciju podataka, što olakšava programiranje složenih operacija iz više izvora u stvarnom vremenu [11]. 

Objektno orijentirano programiranje, iako nudi strukturu kroz enkapsulaciju i nasljeđivanja, kod aplikacija za obradu podataka u stvarnom vremenu može postati teško skalabilno. Manipulacija objektima i nasljeđivanje može dovesti do prekomjernog korištenja memorije i usporavanja performansi u radu s velikim količinama podataka. Isto tako, često imaju višu latenciju zbog stvaranja i brisanja objekata, troše više resursa u usporedbi s nepromjenjivim podacima u funkcionalnom programiranju.

Funkcijsko programiranje nudi brojne prednosti za aplikacije koje obrađuju podatke u stvarnom vremenu. Zbog nepromjenjivih podataka i bezstanja funkcija olakšava paralelno procesiranje podataka. Kad se podaci prikupljaju iz više izvora (senzori, web servisi, tokovi podataka), paralelizacija omogućuje bržu obradu bez zabrinutosti za promjene stanja ili konflikte pristupa podacima. Latencija se dodatno smanjuje jer funkcijsko programiranje koristi lijeno izvođenje, gdje se izračuni ne izvršavaju odmah, već tek kada su potrebni. To znači da se sustav ne opterećuje nepotrebnim zadacima, što poboljšava performanse i smanjuje kašnjenja u procesima koa što je agregacija podataka. Također, funkcijsko programiranje nudi bolju skalabilnost jer s rastom podataka, zadaci se mogu paralelno raspodijeliti na više CPU jezgri. Objektno-orjentirano programiranje može biti manje efikasno u aplikacijama koje zahtijevaju brzu obradu velikih količina podataka zbog povećane potrošnje memorije.


### Zaključak

Funkcijsko programiranje pruža mnoge prednosti u obradi podataka u stvarnom vremenu. Nadmašuje tradicionalne pristupe poput imperativnog i objektno-orjentiranog programiranja zahvaljujući imutabilnosti podataka i bezstanja funkcija, paralelizaciji, smanjenoj latenciji i boljoj skalabilnosti. Ove karakteristike omogućuju učinkovitije upravljanje velikim tokovima podataka, smanjenje rizika od grešaka uzrokovanih promjenama stanja i jednostavnije održavanje koda. Funkcijsko programiranje predstavlja snažan alat za aplikacije koje zahtijevaju visoke performanse i pouzdanost u stvarnom vremenu. 


### Literatura

- [1] AdityaPratapBhuyan. (2023, October 4). Unlocking the Power of Functional Programming: A Comprehensive guide. DEV Community. https://dev.to/adityapratapbh1/unlocking-the-power-of-functional-programming-a-comprehensive-guide-506e
- [2] Brasseur, A., & Brasseur, A. (2024, February 13). Functional Programming: pure functions. SitePoint. https://www.sitepoint.com/functional-programming-pure-functions/#purefunctions
- [3] Deploying Gunicorn — Gunicorn 23.0.0 documentation. (n.d.). https://docs.gunicorn.org/en/latest/deploy.html
- [4] Frame, S., & Coffey, J. W. (2014). A comparison of functional and imperative programming techniques for mathematical software development. DOAJ (DOAJ: Directory of Open Access Journals). https://doaj.org/article/0b3ced5fe88e44f4b63b1bc6026d9416
- [5] GeeksforGeeks. (2024, September 2). Functional Programming Paradigm. GeeksforGeeks. https://www.geeksforgeeks.org/functional-programming-paradigm/
- [6] Gunicorn — Flask Documentation (3.0.x). (n.d.). https://flask.palletsprojects.com/en/3.0.x/deploying/gunicorn/
- [7] Hammond, K. (2005). Is it time for Real-Time functional programming? In Intellect Books (pp. 1–18). https://doi.org/10.2307/j.ctv36xvxxx.4
- [8] Jorgeehrhardt. (2021, December 12). Why you should learn Functional Programming for Big Data. Medium. https://medium.com/@jorgeehrhardt123/why-you-should-learn-functional-programming-for-big-data-a85bd429bf49
- [9] Karasek, D. (2024, April 18). Unleashing the power: The advantages of functional programming in the digital age. Scalac - Software Development Company - Akka, Kafka, Spark, ZIO. https://scalac.io/blog/unleashing-the-power-the-advantages-of-functional-programming-in-the-digital-age/
- [10] Richman, J. (2024, June 5). What is Real-Time Processing (In-depth guide for beginners). Estuary. https://estuary.dev/what-is-real-time-processing/
- [11] Team, C. O. (2023, November 28). Declarative vs. Imperative Programming: 4 Key Differences. Codefresh. https://codefresh.io/learn/infrastructure-as-code/declarative-vs-imperative-programming-4-key-differences/
- [12] The Haskell Cabal | Overview. (n.d.). https://www.haskell.org/cabal/
- [13] Wikipedia contributors. (2024, May 16). Function composition (computer science). Wikipedia. https://en.wikipedia.org/wiki/Function_composition_(computer_science)


