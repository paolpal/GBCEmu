# Emulatore Game Boy Color in Rust

## Introduzione
Questo progetto è un emulatore del Game Boy Color (GBC) scritto in Rust. L'obiettivo principale è quello di creare un emulatore completo e preciso del GBC che possa eseguire giochi commerciali senza problemi.

## Funzionalità
Il progetto mira a fornire le seguenti funzionalità:

- Emulazione accurata delle specifiche hardware del GBC, compresi CPU, GPU, audio e input.
- Supporto per il caricamento e l'esecuzione di ROM di giochi GBC.
- Opzioni di salvataggio dello stato del gioco e caricamento da file.
- Interfaccia utente semplice per la gestione dei giochi e delle impostazioni.

## Architettura del Progetto
Il progetto è suddiviso in diversi moduli per garantire una buona separazione delle responsabilità e una manutenibilità del codice. Le principali componenti includono:

- **CPU**: Implementazione della CPU del GBC, incluso l'insieme di istruzioni e il ciclo di clock.
- **GPU**: Emulazione del processore grafico del GBC per la visualizzazione dei grafici dei giochi.
- **Audio**: Gestione dell'audio per riprodurre suoni e musiche del gioco.
- **Input**: Monitoraggio degli input dell'utente, come tastiera o controller, per l'interazione con il gioco.
- **Caricatore di ROM**: Modulo per il caricamento e l'analisi dei file ROM dei giochi.
- **Interfaccia utente**: Una semplice GUI o interfaccia a riga di comando per avviare e gestire i giochi.

## Tecnologie Utilizzate
Il progetto sfrutta le seguenti tecnologie principali:

- **Rust**: Linguaggio di programmazione principale per lo sviluppo del progetto.
- **SDL2**: Libreria per la grafica, l'audio e la gestione degli input.

## Riferimento agli Opcode
Per ulteriori informazioni sugli opcode della CPU del Game Boy, consulta i seguenti siti:
- [Tabella degli Opcode](https://izik1.github.io/gbops/).
- [Dettagli delle Istruzioni](http://z80-heaven.wikidot.com/instructions-set)

## Stato del Progetto
Il progetto è ancora nelle fasi iniziali di sviluppo. Attualmente sono implementate solo parzialmente la gestione della memoria e la CPU del GBC. Molte altre caratteristiche chiave devono ancora essere sviluppate e ottimizzate per raggiungere gli obiettivi desiderati.

## Contributi
Sono benvenuti i contributi da parte della comunità! Se sei interessato a contribuire al progetto, controlla il repository su GitHub e unisciti alla discussione.

## Licenza
Il progetto è rilasciato sotto la licenza MIT. Consulta il file LICENSE per ulteriori dettagli.

## Contatti
Per domande, suggerimenti o segnalazioni di bug, non esitare a contattare il team di sviluppo all'indirizzo email o sul canale Slack dedicato.
