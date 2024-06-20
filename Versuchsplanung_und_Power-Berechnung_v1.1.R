### ---------------------------------------------------------------------- ###
### Eine App zur Versuchsplanung (Stichprobengroesse) und Power-Berechnung ###
### ---------------------------------------------------------------------- ###
# Mit dieser App koennen benoetigte Stichprobenumfaenge fuer t-Tests und ANOVA
# berechnet werden. Ausgegangen wird dabei von Vorversuchen aus denen der/die 
# Mittelwert/e und Standardabweichung/en abgeschaetzt werden koennen (t-Test).
# Fuer die ANOVA werden Abschaetzungen der systematischen und unsystematischen 
# Varianz benoetigt. Neben der Ausgabe des benoetigten Stichprobenumfang werden 
# in einer Abbildung die zentrale (H0) und die nicht-zentrale (HA) Verteilung 
# inklusive der Fehlerbereiche dargestellt.
# 
# Detailiertere Informationen zu der App, ihrer Bedienung, den Funktionen und
# der Implementierung sind in der Dokumentation innerhalb der App zu erhalten.
# ------------------------------------------------------------------------------
# Autor: Bjarne Schultze                    [v1.1 letzte Aenderung: 13.06.2024]
#        (bjarne.schultze@uol.de)
# Lizenzhinweis: CC BY-NC 4.0 (Lizenztext unter: 
#                 http://creativecommons.org/licenses/by-nc/4.0/?ref=chooser-v1)
# ------------------------------------------------------------------------------

##### Vorbereitung -------------------------------------------------------------
# Hilfsvariable fuer die Erfolgsmeldung beim Laden der Pakete
packs = c(F,F,F)
packages = c('bslib','pwr','shinyjs')

# Pruefen, ob benoetigte Pakete installiert sind und ggf. installieren
while(sum(packs) < 3){
  # Paket 'bslib' laden, um optisch ansprechende 'Bootstrap' Themes zu nutzen
  packs[1] = require(bslib, quietly = T, warn.conflicts = F)
  # Paket 'pwr' laden, um die Stichprobengroesse fuer eine Anova zu berechnen
  packs[2] = require(pwr, quietly = T, warn.conflicts = F)
  # Paket 'shinyjs' laden, um User-Interface flexibel anzupassen
  packs[3] = require(shinyjs, quietly = T, warn.conflicts = F)
  
  # Wenn mindestens eines der Pakete nicht installiert ist ...
  if(sum(packs) < 3){
    # Abfrage nach automatischer Installation der fehlenden Pakete
    autoInstall = readline(
      prompt = "Es fehlen Pakete. Sollen diese automatisch installiert werden (J/N)? ")
    
    # fehlende(s) Pakt(e) heraussuchen
    fehlendePacks = packages[!packs]
    
    # Pakete automatisch installieren, wenn gewuenscht, ansonsten einen Hinweis 
    # ausgeben und App beenden
    if(any(autoInstall == c('J','j'))){
      # Pakete installieren
      install.packages(fehlendePacks, quiet = T)
    }else{
      # Hinweis zu den zu installierenden Paketen 
      cat(paste("\nBitte installieren Sie folgende(s) Paket(e): ", 
                  paste(fehlendePacks, collapse = ', '),"\n"))
      # App beenden
      stop("Es fehlen Pakete!", call. = F)
    }
  }
}

# Zusaetzliche Funktionen laden
source("www/calculating_functions.R")

# Festlegen des Themes der App (Nutzung eines Standard Bootsprap Themes)
appTheme = bs_theme(version = 4, bootswatch = "flatly")
# minimale Anpassung des Themes
appTheme = bs_theme_update(appTheme, primary = "#173553", secondary = "#00959E", 
                           success = "#319E0F", info = "#538EB2", 
                           spacer = "0.8rem")

# HTML-Datei fuer die Anleitungs- und Dokumentationsseite einlesen
doku = readLines("www/dokumentation.html", encoding = "UTF-8")
# HTML-Dateien fuer die Kopf- und Fusszeile einlesen
Kopfzeile = readLines("www/kopf.html", encoding = "UTF-8")
Fusszeile = readLines("www/fuss.html", encoding = "UTF-8")

# Dictionary fuer die statistischen Tests erstellen (Name ausgeschrieben - 
# Kurzform zum besseren Arbeiten innerhalb des Codes)
testDict = c("t-Test"="panel_ttest",
             "Analysis of Variance (ANOVA)"="panel_anova")
# Dictionary fuer die t-Test-Eingaben Stichprobenanzahl und (un)gerichteter Test
typeDict = c("eine Stichprobe"="one.sample",
             "zwei Stichproben (ungepaart)"="two.sample",
             "zwei Stichproben (gepaart)"="paired",
             "zweiseitig/ungerichtet"="two.sided",
             "einseitig/gerichtet"="one.sided")


##### Definition des User-Interfaces (ui) --------------------------------------
# Seite als 'fluidPage' aufsetzen, damit die App-Inhalte fluide auf Skalierungen
# des App-Fensters reagieren
ui <- fluidPage(
  # Shiny JavaScript fuer Benutzung initialisieren
  useShinyjs(),
  # Aussehen der App auf das definierte Theme festlegen
  theme = appTheme,
  
  # Titel der App
  title = "Versuchsplanung mit R",
  
  ### Kopfzeile mit Titel und Button fuer die Anleitung
  HTML(Kopfzeile),
  
  ### Definition des Bodys der App als eine tab set panel Umgebung mit
  ### versteckter Steuerung. Ein panel fuer das Hauptfenster mit allen 
  ### Bedienmoeglichkeiten, Berechnungen und Ergebnissen, sowie ein panel, fuer
  ### die Anleitung
  tabsetPanel(id = "fensterAuswahl",
              type = "hidden",
              
    ## Definition des panels fuer das Hauptfenster der App
    tabPanelBody(value = "hauptFenster",{
      
      # Layout des panels als sidebar-Layout definieren (Hauptbereich und 
      # abgesetzter Seitenbereich auf der linken Seite)
      sidebarLayout(
        # Gestaltung des Seitenbereichs
        sidebarPanel(
          # User-Input fuer die Auswahl des Modus (Stichprobenumfang/Power)
          selectInput("appModus", label = "Modus der App",
                      choices = c("Stichprobenumfang (a-priori)",
                                  "Power (post hoc)")),
          # User-Input zum statistischen Test fuer folgende Berechnungen
          selectInput("testAuswahl", label = "Statistischer Test", 
                    choices = c("t-Test", "Analysis of Variance (ANOVA)")),

          
          # Definition einer tab set panel Umgebung um die Eingabe Optionen
          # an den ausgewaehlten Test anzupassen (versteckte Steuerung)
          tabsetPanel(id = "testTabs", 
                    type = "hidden",
                    # Eingabe-Optionen fuer den t-Test (Stichprobenumfang)
                    tabPanelBody(value = "panel_ttest",
                             # User-Input fuer das Signifikanzniveau
                             numericInput("alphaIN", HTML("Signifikanzniveau (&alpha;)"), 
                                          value = 0.05, min = 0, max = 1, step = 0.01),
                             # User-Input fuer die Power
                             numericInput("powerIN", HTML("Power (1-&beta;)"), 
                                          value = 0.8, min = 0, max = 1, 
                                          step = 0.01),
                             # User-Eingabe der Anzahl der Stichproben 
                             # (entsprechend eines Ein- oder Zweistichproben 
                             # Tests, letzterer mit der Option gepaarte oder 
                             # ungepaarte Stichproben)
                             selectInput("numStichprobenIN", 
                                         label = "Anzahl der Stichproben",
                                         choices = c("eine Stichprobe", 
                                                     "zwei Stichproben (ungepaart)", 
                                                     "zwei Stichproben (gepaart)")),
                             # User-Eingabe, ob es ein gerichteter Test ist
                             selectInput("testRichtungIN", 
                                         label = "Richtung des Tests",
                                         choices = c("zweiseitig/ungerichtet", 
                                                     "einseitig/gerichtet")),
                             # User-Eingabe fuer den Stichprobenumfang
                             numericInput("nIN",
                                           label = "Stichprobenumfang (pro Gruppe)",
                                           min = 1, step = 1, value = 5),
                             # User-Eingabe zum Mittelwertsabstand
                             numericInput("deltaMuIN", 
                                          label = "Mittelwertsabstand (abgeschätzt)",
                                          min = 0, value = 0.5, step = 0.01),
                             # User-Eingabe zur Standardabweichung
                             numericInput("stdabwIN", 
                                          label = "Standardabweichung (abgeschätzt)",
                                          min = 0, value = 0.5, step = 0.01)),
                    
                    
                    # Eingabe-Optionen fuer die ANOVA (Stichprobenumfang)
                    tabPanelBody(value = "panel_anova", 
                                 # User-Input fuer das Signifikanzniveau
                                 numericInput("alphaINa", HTML("Signifikanzniveau (&alpha;)"), 
                                              value = 0.05, min = 0, max = 1, step = 0.01),
                                 # User-Input fuer die Power
                                 numericInput("powerINa", HTML("Power (1-&beta;)"), 
                                              value = 0.8, min = 0, max = 1, 
                                              step = 0.01),
                                 # User-Eingabe fuer die Anzahl der Gruppen
                                 numericInput("numGruppenIN",
                                              label = "Anzahl der Gruppen", 
                                              min = 1, value = 3, step = 1),
                                 # User-Eingabe fuer den Stichprobenumfang
                                 numericInput("nINa",
                                               label = "Stichprobenumfang (pro Gruppe)",
                                               min = 1, step = 1, value = 5),
                                 # User-Eingabe fuer die Effektstaerke
                                 numericInput("effStaerkeIN",
                                              label = "Effektstärke  (abgeschätzt)",
                                              min = 0, value = 0.3, step = 0.01),
                                 radioButtons("effSMassIN",
                                              label = "Effektstärkemaß",
                                              choiceNames = list("f",
                                                                 HTML("&eta;<sup>2</sup>")),
                                              choiceValues = c("fwert","eta_sq"),
                                              selected = "fwert", inline = T),
                                 htmlOutput("inputError", inline = T)),
                    
          # Ende der tab set panel Umgebung fuer die variablen Eingaben
          ),
          
        # Definition eines Buttons zum Ausloesen der Berechnungen
        actionButton("berechnenButton", label = "Berechnen", 
                     class = "btn-secondary")
        # Ende der Definition des Seitenbereichs (sidebar panel)
        ),
        
        # Gestaltung des Hauptbereichs
        mainPanel(
          # Breite auf 8 von 12 festlegen
          width = 8,
          # Bereich der Ergebnis-Ausgabe mit fluid rows gestalten (variable Breite
          # aber festes Seitenverhaeltins)
          # Erste Zeile mit einem Titel und der Art des Ergebnisses/der Berechnung
          fluidRow(
            # eine Spalte mit voller Breite
            column(12, htmlOutput("ergebnisTyp"))),
          # Zeilen mit den berechneten Werten inkl. passender Beschriftung
          fluidRow(
            # Spalte mit Beschriftung
            column(6, htmlOutput("ausgabeBeschriftung")),
            # Spalte fuer die Werte
            column(6, htmlOutput("ausgabeWerte"))),
          # Zeilen mit einem Hinweis zur gewuenschten Power
          fluidRow(
            column(12, htmlOutput("powerHinweis"))),
          # Zeilen fuer die tatsaechliche Power mit dem errechneten Stichproben-
          # umfang inkl. Beschriftung
          fluidRow(
            # Beschriftung, gleiche Breite wie oben fuer konsistentes Layout
            column(6, htmlOutput("aktPowerText")),
            # Wert
            column(6, htmlOutput("aktPowerWert"))),
          
          # Definition eines Grafik-Outputs in Form eines Plots
          plotOutput("ergebnisPlot")
          
        # Ende der Definition des Hauptbereichs (main panel) 
        )
      # Ende der Definition des sidebar Layouts
      )
      ## Ende der Definition des Hauptfensters
      }),
    
    ## Anlegen des Dokumentationsfensters
    tabPanelBody(value = "dokuFenster",
                 # komplette Gestaltung als HTML-Output
                 htmlOutput("doku"))
    
  ### Ende der tab set panel Umgebung fuer das Haupt- und Dokufenster  
  ),      
      
  ### Fusszeile mit Autor- und Versions- und Lizenzinfo
  HTML(Fusszeile)
  

#### Ende der Definition des User-Interface -----------------------------------
)

# ------------------------------------------------------------------------------

##### Definition der Server-Funktion -------------------------------------------
server <- function(input, output, session) {
  
  # Inhalt fuer Dokufenster aus der eingelesenen html-Datei uebernehmen und als
  # HTML interpretieren lassen
  output$doku = renderUI(HTML(doku))
  
  ### Funktion des Buttons zum Wechseln zwischen Berechnung und Doku
  observeEvent(input$fensterButton, {   # bei Klick ...
    # if-Abfrage um den Button umzubenennen und ein Wechsel in beide Richtungen
    # zu ermoeglichen. Wenn der Klick-Zaehler des Buttons gerade ist, wird das 
    # Hauptfenster ausgewaehlt, ansonsten das Dokumentationsfenster
    if(input$fensterButton[1] %% 2 == 0){
      # Hauptfenster panel zum aktiven/ausgewaehlten/sichtbaren panel machen
      updateTabsetPanel(session, "fensterAuswahl",
                        selected = "hauptFenster")
      # Beschreibung des Buttons zu 'Anleitung' aendern
      updateActionButton(inputId = "fensterButton", 
                         label = "Anleitung")
    }else{
      # Dokufenster panel zum aktiven/sichtbaren panel machen
      updateTabsetPanel(session, "fensterAuswahl",
                        selected = "dokuFenster")
      # Beschriftung des Buttons zu 'Zurueck zur Anwendung' aendern
      updateActionButton(inputId = "fensterButton", 
                        label = "Zurück zur Anwendung")
    }
  ### Ende der Fenster-Button Befehle
  })
  
  ### Auswahl des Testes observieren (Aktion bei Klick)
  observeEvent(input$testAuswahl, {
    # Tabsetpanel entsprechend der Test-Auswahl aendern, den Testnamen aus dem
    # user interface dabei anhand des dictionaries uebersetzen
    updateTabsetPanel(session, "testTabs",
                      selected = testDict[input$testAuswahl][[1]]
    )
  ### Ende der Befehle fuer die Auswahl des Tests
  })
  
  ### Auswahl des App Modus observieren
  observeEvent(input$appModus, {
    # Wenn der Modus 'Power' ausgewaehlt ist 
    if(input$appModus == "Power (post hoc)"){
      # Verstecken der Power-Eingabefelder
      hide("powerIN"); hide("powerINa")
      # Felder zur Eingabe des Stichprobenumfangs anzeigen
      show("nIN"); show("nINa")
      # Entfernen der Hinweise zu abgeschaetzten Werten
      updateNumericInput(session, "effStaerkeIN", label = "Effektstärke")
      updateNumericInput(session, "deltaMuIN", label = "Mittelwertsabstand")
      updateNumericInput(session, "stdabwIN", label = "Standardabweichung")
    # Wenn der Modus 'Stichprobenumfang' ausgewaehlt ist
    }else{
      # Power Eingabefelder wieder einblenden
      show("powerIN"); show("powerINa")
      # Felder zur Eingabe des Stichprobenumfangs verstecken
      hide("nIN"); hide("nINa")
      # Hinweise zu abgeschaetzten Werten wieder hinzufuegen 
      updateNumericInput(session, "effStaerkeIN", 
                         label = "Effektstärke (abgeschätzt)")
      updateNumericInput(session, "deltaMuIN", 
                         label = "Mittelwertsabstand (abgeschätzt)")
      updateNumericInput(session, "stdabwIN",
                         label = "Standardabweichung (abgeschätzt)")
    }
  ### Ende der Befehle fuer die Auswahl des App-Modus
  })
  
  ### Auswahl des Effektstaerkemasses observieren
  observeEvent(input$effSMassIN, {
    # Wenn Eta-Quadarat ausgewaehlt ist, wird das Maximum der Eingabe auf knapp 
    # unter 1 gesetzt, da ab Werten von 1 keine Umrechnung in f erfolgen kann
    if(input$effSMassIN == "eta_sq"){
      updateNumericInput(session, "effStaerkeIN", max = 0.99)
    }else{
      # Ist f ausgewaehlt, wird keine Grenze gesetzt
      updateNumericInput(session, "effStaerkeIN", max = "")
    }
  ### Ende der Befehle fuer die Effektstaerke-Auswahl  
  })
  
  
  ### Funktion des Berechnen-Button (Aktionen bei Klick)
  observeEvent(input$berechnenButton, {
    # Aktionen, wenn der t-Test gewaehlt wurde
    if(input$testTabs == "panel_ttest"){
      # Test-Typ (1/2 Stichproben) und Test-Richtung (ein-/zweiseitig) im 
      # Dictionary abrufen, um benutzerfreundliche Namen in Programm-freundliche 
      # zu uebersetzen
      testType = typeDict[input$numStichprobenIN][[1]]
      testAlt = typeDict[input$testRichtungIN][[1]]
      
      # Berechnung - Stichprobenumfang
      if(input$appModus == "Stichprobenumfang (a-priori)"){
        Ergebnisse = stpUfg_ttest(testType,testAlt,input$deltaMuIN,
                                  input$stdabwIN,input$alphaIN,input$powerIN)
      # Berechnung - Power
      }else{
        Ergebnisse = power_ttest(testType,testAlt,input$deltaMuIN,
                                  input$stdabwIN,input$alphaIN,input$nIN)
      }
      
      if(testType == "two.sample"){
        # Wenn Zweistichproben-Test (ungepaart), wird die Anzahl der Stichproben 
        # fuer die Ausgabe auf 2 gesetzt
        ausgabeAnzahl = 2
      }else{
        # Stichprobenanzahl fuer Ausgabe bei Einstichproben-Test/gepaartem 
        # Zweistichproben-Test
        ausgabeAnzahl = 1
      }
      
      # Kritischen Wert aus den Ergebnissen extrahieren
      kritWert = Ergebnisse$kritWert
      
      # Limits der x-Achse festlegen
      if(testAlt == "one.sided"){
        # Zentral um den kritischen Wert
        xLimits = round(c(kritWert-5,kritWert+5),2)
      }else{
        # Beide Enden der Verteilung innerhalb der Limits
        xLimits = round(c(-kritWert-2,kritWert+2),2)
      }
      # Beschriftung fuer den kritischen t-Wert erstellen
      kritText = sprintf("kritischer t-Wert: %g", round(kritWert,2))
      
      
    # Aktionen, wenn die ANOVA gewaehlt wurde    
    }else if(input$testTabs == "panel_anova"){
      # Hinweis-Feld leeren (fuer den Fall einer problemlosen Ausfuerung)
      output$inputError = renderUI("")
      
      # Berechnung - Stichprobenumfang
      if(input$appModus == "Stichprobenumfang (a-priori)"){
        # Ergebnisse berechnen
        Ergebnisse = stpUfg_anova(input$effSMassIN,input$effStaerkeIN,
                                  input$numGruppenIN,input$alphaINa,
                                  input$powerINa)
      # Berechnung - Power
      }else{
        # Ergebnisse berechnen
        Ergebnisse = power_anova(input$effSMassIN,input$effStaerkeIN,
                                 input$numGruppenIN,input$alphaINa,
                                 input$nINa)
      }
      
      # Wenn es einen Fehler gab ...
      if(!is.list(Ergebnisse)){
        # ... wird ein Hinweis ueber einen zu groessen f-Wert angezeigt ...
        output$inputError = renderUI(HTML("<b style='color:#c10000'>Fehler: 
                                              Die angegebene Effektstärke ist zu 
                                              groß!</b><br><br>"))
        # ... und die App wird angehalten (folgender Code wird nicht ausgefuehrt)
        shiny:::reactiveStop("Fehler: f ist zu gross!")
      }
      
      # Anzahl der Gruppen (Faktorstufen) fuer die Ausgabe speichern
      ausgabeAnzahl = input$numGruppenIN
      
      # Kritischen Wert aus den Ergebnissen extrahieren
      kritWert = Ergebnisse$kritWert
      # Limits der x-Achse festlegen (zental um den kritischen Wert)
      xLimits = round(c(kritWert-5,kritWert+5),2)
      # Beschriftung fuer den kritischen t-Wert erstellen
      kritText = sprintf("kritischer F-Wert: %g", round(kritWert,2))
    }
    
    
    ### Erzeugen des grafischen outputs im user interface
    
    # Ueberschrift fuer die Ergebnisse ausgeben
    output$ergebnisTyp = renderUI({
      if(isolate(input$appModus) == "Stichprobenumfang (a-priori)"){
        HTML("<h2>Ergebnis</h2><h4>Stichprobenumfang</h4>")
      }else{
        HTML("<h2>Ergebnis</h2><h4>Power (post hoc)</h4>")
      }
    })
    
    # Text zur Beschriftung der Ausgabewerte festlegen
    ausgabeTextAnzahl = "Anzahl der Gruppen:"
    ausgabeTextUmf = "Stichprobenumfang pro Gruppe:"
    ausgabeTextGesamtUmf = "Gesamt-Stichprobenumfang:"
    powerHinweis = "<strong>Hinweis:</strong> Durch das Runden der 
                      Stichprobenumfänge auf ganzzahlige Werte ändert 
                      sich die Power der geplanten Analyse (geringfügig)."
    
    # Beschriftungstext ausgeben (HTML-formatiert)
    output$ausgabeBeschriftung = renderUI({
      HTML(paste(ausgabeTextAnzahl,"</br>",
                 ausgabeTextUmf,"</br>",
                 ausgabeTextGesamtUmf))
    })
    
    # zugehoerige Ausgabewerte ausgeben (HTML-formatiert)
    output$ausgabeWerte = renderUI({
      HTML("<strong>",
           ausgabeAnzahl,"</br>",
           Ergebnisse$StichpUmfang,"</br>",
           Ergebnisse$GesamtUmf,"</strong>")
    })
    
    # Ausgabe des Hinweises zur sich aendernden Power (nur fuer die Berechnung 
    # des Stichprobenumfangs)
    output$powerHinweis = renderUI({
      if(isolate(input$appModus) == "Stichprobenumfang (a-priori)"){
        HTML("</br>",powerHinweis)
      }
    })
    
    # Beschriftung fuer die angepasste bzw. erreichte Power ausgeben
    output$aktPowerText = renderText({
      # Angepasst (Stichprobenumfang berechnet)
      if(isolate(input$appModus) == "Stichprobenumfang (a-priori)"){
        "Aktualisierte Power:"
      # Erreicht (Post hoc Power Berechnung)
      }else{
        HTML("</br><strong>Erreichte Power:</strong>")
      }
    })
    
    # Wert der angepassten oder erreichten Power ausgeben (HTML-formatiert)
    output$aktPowerWert = renderUI({
      # Angepasst 
      if(isolate(input$appModus) == "Stichprobenumfang (a-priori)"){
        HTML("<strong>",round(Ergebnisse$aktPower,3),"</strong>")
      # Erreicht 
      }else{
        HTML("</br><strong>",round(Ergebnisse$pwr,3),"</strong>")
      }
    })
    
    # Grafische Ausgabe der Wahrscheinlichkeitsdichtefunktionen erzeugen
    output$ergebnisPlot = renderPlot({
      
      # x Werte zum Plotten extrahieren
      x = Ergebnisse$x
      
      # Erweiterung des Plot-Bereichs mit der Moeglichkeit Objekte ausserhalb 
      # der Plot-Box zu platzieren (fuer die Legende)
      par(mar = c(9,4,4,2),xpd = T)
      
      # Darstellung der zentralen Dichteverteilung 
      plot(x, Ergebnisse$distCentral,
           xlab = "x",
           ylab = "Wahrscheinlichkeitsdichte",
           xlim = xLimits,
           ylim = c(0,max(Ergebnisse$distNonCentral)+0.05),
           xaxs = "i", yaxs = "i",                # Label-Intervall-Ermittlung
           xaxt = "n", yaxt = "n",                # Achsen unterdruecken 
           col = "#275d93", 
           type = "l", lwd = 2,
           xpd = F)                               # Nur innerhalb der Plot-Box
      # Hinzufuegen der nicht-zentralen Dichteverteilung
      lines(x, Ergebnisse$distNonCentral,
            col = "#93275d",
            type = "l", lty = 2, lwd = 2, 
            xpd = F)                              # Nur innerhalb der Plot-Box
      
      # Linienstaerke der Plot-Box erhoehen
      box(lwd = 2)
      
      
      # Einfaerben der Flaeche des alpha-Fehlers
      if(exists('testAlt') && testAlt == 'two.sided'){
        # Fuer einen zweiseitigen Test: 2x alpha/2 (rechts und links)
        polygon(c(min(x), x[x<=(-kritWert)], -kritWert), 
                c(Ergebnisse$distCentral[x<=(-kritWert)],0,0), 
                col = rgb(39,93,147,77,'maxColorValue',255), 
                border = rgb(39,93,147,77,'maxColorValue',255),
                xpd = F)
        polygon(c(x[x>=kritWert], max(x), kritWert), 
                c(Ergebnisse$distCentral[x>=kritWert],0,0), 
                col = rgb(39,93,147,77,'maxColorValue',255), 
                border = rgb(39,93,147,77,'maxColorValue',255),
                xpd = F)
      }else{
        # Fuer einen einseitigen Test 1x alpha (rechts)
        polygon(c(x[x>=kritWert], max(x), kritWert), 
                c(Ergebnisse$distCentral[x>=kritWert],0,0), 
                col = rgb(39,93,147,77,'maxColorValue',255), 
                border = rgb(39,93,147,77,'maxColorValue',255),
                xpd = F)
      }
      
      
      # Einfaerben der Flaeche des beta-Fehlers 
      polygon(c(min(x), kritWert, x[x<=kritWert]), 
              c(Ergebnisse$distNonCentral[x<=kritWert],0,0), 
              col = rgb(147,39,93,77,'maxColorValue',255), 
              border = rgb(147,39,93,77,'maxColorValue',255),
              xpd = F)
      
      # Werte fuer die Beschriftung der x-Achse definieren
      xTicks = c(seq(from = ceiling(xLimits[1]), to = floor(xLimits[2]), 
                     by = 1))
      # Neue Achsen erzeugen und anpassen (Linienstaerke, Beschriftung)
      axis(1,lwd = 2, at = xTicks)
      axis(2,lwd = 2, at = seq(from = 0, to = max(Ergebnisse$distNonCentral) 
                               + 0.05, by = 0.05), las = 1)
      
      # Kritischen Wert einzeichnen
      abline(v=kritWert, col = "darkgreen", lwd = 2, xpd = F)
      # Kritischen Wert beschriften
      mtext(kritText, side = 3, at = kritWert, col = "darkgreen")
      
      # Hinzufuegen einer passenden Legende
      legend('bottom', inset = c(0,-0.5),         # Position unter der Plot-Box
             legend = c(expression("zentrale Verteilung (H"[0]~")"), 
                        expression("nicht-zentrale Verteilung (H"[A]~")"),
                        expression("Bereich des"~alpha-"Fehler"),
                        expression("Bereich des"~beta-"Fehlers")),
             col = c("#275d93","#93275d",
                     rgb(39,93,147,77,'maxColorValue',255),
                     rgb(147,39,93,77,'maxColorValue',255)),
             pch = c(NA,NA,15,15), pt.cex = 2,    # Symbole und Symbolgroesse
             lty = c(1,1,0,0), lwd = 3,           # Linien und Linienstaerke
             ncol = 2)                            # Zwei-Spalten-Layout
      
    })
    
  ### Ende der Definition der Funktion des Berechnen-Buttons  
  })
#### Ende der Definition der Server-Funktion -----------------------------------
}

###### -------------------------------------------------------------------------

# Kombinieren des User-Interface und der Server-Funktion zur fertigen App
shinyApp(ui, server)
