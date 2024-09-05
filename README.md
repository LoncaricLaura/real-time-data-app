# real-time-data-processing-app

Tema ovog projekta je izgradnja funkcionalne aplikacije za obradu podataka u stvarnom vremenu, s fokusom na prikupljanje podataka o okolišu tijekom duljeg razdoblja i provođenje statističke analize. Posljednjih godina obrada podataka u stvarnom vremenu dobila je značajnu važnost zbog potrebe za analizom kontinuiranih tokova podataka u raznim područjima poput praćenja vremena, IoT sustava i automatizacije.


Ovaj projekt koristi Haskell za obradu i analizu podataka o okolišu u stvarnom vremenu prikupljenih pomoću Raspberry Pi i DHT11 senzora. DHT11 senzor koristi se za mjerenje temperature i vlažnosti, što ga čini idealnim za kućnu automatizaciju i sustave nadzora okoliša. 


[Flask API](https://github.com/mvisnjic/project_iot) se koristi za upravljanje funkcionalnošću sustava, kao što je paljenje/gašenje svjetla, uz kontinuirano mjerenje temperature i vlažnosti svakih 30 minuta. Podaci za analizu se ispisuju u logovima. Podaci su prikupljani od 09/2023. Raspberry Pi pokreće Python skriptu koje surađuju sa senzorom preko GPIO pinova, što uključuje:

- Temperatura (°C)
- Vlažnost (%)
- Vremenske oznake koje označavaju vrijeme prikupljanja podataka

Podaci se pohranjuju ​​i pristupa im se pomoću REST API-ja koji je deploy-jan pomoću [WSGI Gunicorn](https://flask.palletsprojects.com/en/3.0.x/deploying/gunicorn/) i [Nginx servera](https://docs.gunicorn.org/en/latest/deploy.html).