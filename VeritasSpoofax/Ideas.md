## Ideen um Veritas zu verbessern


- starte mehrere Vampire-Aufrufe, die immer die Zeit verdoppeln, mit der Vampire aufgerufen wird
- openssl-Aufrufe durch Stratego-hash-Aufrufe ersetzen
- Gleichungen optimieren: nicht benötigte Gleichungen früh rausschmeißen, benötigte Gleichungen evtl inlinen (scheinen Vampire zu bremsen)
- rudimentäres Typsystem für Spezifikationssprache, die Typfehler in Definitionen schon vor der TPTP-Generierung abfängt

- Configure Vampire's timeout as annotation of a goal.

