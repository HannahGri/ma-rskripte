# Übersicht der R-Skripte

Dieses Projekt enthält die R-Skripte (R Version 4.3.1), mit denen die Berechnungen und Analysen im Rahmen meiner Masterarbeit durchgeführt wurden.

Zur Ausführung der Skripte werden simulierte Daten verwendet. Diese orientieren sich in ihrer Struktur und Verteilung an den Originaldaten, enthalten jedoch keinerlei Korrelationen zwischen den Variablen. Es bestehen keine Zusammenhänge zwischen Kovariablen und Zielvariablen, sodass die Modelle keine realen Beziehungen erlernen können. 
Sie dienen ausschließlich der Demonstration der Codestruktur und sind nicht zur Interpretation gedacht.


## Inhalte

Der Ablauf der Berechnungen ist wie folgt strukturiert:

- Datensimulation
  - Aufteilung Test- und Trainingsdaten
  - Korrelation
- Neuronale Netze
  - Architektur und Training anhand des Beispiels mit Datensimulation
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
