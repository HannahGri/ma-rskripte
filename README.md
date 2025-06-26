# Übersicht der R-Skripte

Dieses Projekt enthält die R-Skripte (R Version 4.3.1), mit denen die Berechnungen und Analysen im Rahmen meiner Masterarbeit durchgeführt wurden.

Zur Ausführung der Skripte werden simulierte Daten verwendet. Diese orientieren sich näherungsweise in Struktur, Verteilung und Korrelation an den Originaldaten.
Die simulierten Daten dienen der Veranschaulichung der Methoden und Codestrukturen.

## Inhalte

Der Ablauf der Berechnungen ist wie folgt strukturiert:

- Datensimulation
  - Erzeugung von Kovariabeln und Zielvariablen mit Berücksichtigung von Korrelationen zwischen den Variablen   
  - Aufteilung Test- und Trainingsdaten
  - Korrelationsgrafik
- Neuronale Netze
  - Architektur und Training am Beispiels der simulierten Daten
  - Vorhersagen auf den Testdaten
- Logistisch-Lineares Modell
  - Aufbau und Training
  - Vorhersagen auf den Testdaten
- Scoring-Funktionen
  - Implementierung Logarithmischer Score (LogS) und Continuous Ranked Probability Score (CRPS)
  - Berechnung mit den Beispielsvorhersagen der trainierten Modelle
- Methoden zur Featureanalyse
  - Permutation Feature Importance (PFI): Implementierung und Beispiel
  - Partial Dependence Plots (PDP): Implementierung und Beispiel
  - Accumulated Local Effects (ALE): Anwendung mithilfe des Pakets `iml`


## Pakete
Alle verwendeten Pakete sind im Skript `Pakete.R` aufgeführt.
