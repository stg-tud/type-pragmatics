## Ideen um Veritas zu verbessern


- starte mehrere Vampire-Aufrufe, die immer die Zeit verdoppeln, mit der Vampire aufgerufen wird
- führe “lemma” Keyword ein: generiert erst ein goal, das bewiesen werden muss, und wenn das Goal bewiesen werden kann, wird es als Axiom behandelt
- openssl-Aufrufe durch Stratego-hash-Aufrufe ersetzen
- Gleichungen optimieren: nicht benötigte Gleichungen früh rausschmeißen, benötigte Gleichungen evtl inlinen (scheinen Vampire zu bremsen)
