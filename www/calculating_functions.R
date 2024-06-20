
##### Stichprobenumfang Berechnung - t-Test

stpUfg_ttest <- function(testType,testAlt,deltaMu,stdw,alpha,beta){
  # benoetigten Stichprobenumfang ermitteln
  pwrTest = power.t.test(delta = deltaMu,
                         sd = stdw,
                         sig.level = alpha,
                         power = beta,
                         type = testType,
                         alternative = testAlt)
  # erhaltenen Stichprobenumfang auf ganzzahligen Wert runden
  StichpUmfang = round(pwrTest$n,0)
  
  # mit dem gerundeten Stichprobenumfang tatsaechliche Power berechnen
  aktPower = power.t.test(n = StichpUmfang,
                          delta = deltaMu,
                          sd = stdw,
                          sig.level = alpha,
                          type = testType,
                          alternative = testAlt)
  
  # Unterscheidung in Ein- oder Zweistichproben-Test
  if(testType == "two.sample"){
    # Berechnung der Freiheitsgrade
    df = 2*StichpUmfang-2
    # Berechnung des Gesamt-Stichprobenumfangs (beide Stichproben zusammen)
    GesamtUmf = StichpUmfang * 2
    
    # Berechnung des Nichtzentralitaetsparameters fuer die nicht-zentrale 
    # t-Verteilung (ALternativhypothese)
    nonZentParameter = deltaMu/stdw * sqrt(StichpUmfang^2/(StichpUmfang*2))
    
  }else{
    # Fuer einen gepaarten Zweistichproben-Test oder einen 
    # Einstichproben-Test
    
    # Gesamtstichprobenumfang entspricht dem berechneten (bei gepaarten 
    # Tests: Anzahl der Paare)
    GesamtUmf = StichpUmfang
    
    # Berechnung der Freiheitsgrade
    df = GesamtUmf - 1
    
    # Berechnung des Nichtzentralitaetsparameters der nicht-zentralen
    # t-Verteilung (Alternativhypothese)
    nonZentParameter = deltaMu*sqrt(StichpUmfang)/stdw
  }
  
  # Berechnung des kritischen t-Wertes, unterschieden nach ein- und 
  # zweiseitige Tests
  if(testAlt == "one.sided"){
    # Fuer einseitige Tests (alpha Quantil)
    kritWert = qt(1-alpha,df)
  }else{
    # fuer zweiseitige Tests (alpha/2 Quantil)
    kritWert = qt(1-alpha/2,df)
  }
  
  # x-Vektor fuer die Wahrscheinlichkeitsdichtefunktionen erzeugen
  x = seq(from = -5,to = 10, by = 0.001)
  # y-Werte der Dichtefunktion der zentralen t-Verteilung bestimmen 
  distCentral = dt(x, df, 0)
  # y-Werte der Dichtefunktion der nicht-zentralen t-Verteilung bestimmen
  distNonCentral = dt(x, df, nonZentParameter)
  
  # Alle noetigen Variablen zu einer output Liste kombinieren
  output = list(StichpUmfang=StichpUmfang, GesamtUmf=GesamtUmf, 
                aktPower=aktPower$power, nonZentParameter=nonZentParameter,
                kritWert=kritWert,distCentral=distCentral,
                distNonCentral=distNonCentral, x=x)
  # Output Liste zurueckgeben
  return(output)
}


##### Power Berechnung - t-Test

power_ttest <- function(testType,testAlt,deltaMu,stdw,alpha,n){
  # benoetigten Stichprobenumfang ermitteln
  pwrTest = power.t.test(delta = deltaMu,
                         sd = stdw,
                         sig.level = alpha,
                         n = n,
                         type = testType,
                         alternative = testAlt)
  
  # Unterscheidung in Ein- oder Zweistichproben-Test
  if(testType == "two.sample"){
    # Berechnung der Freiheitsgrade
    df = 2*n-2
    # Berechnung des Gesamt-Stichprobenumfangs (beide Stichproben zusammen)
    GesamtUmf = n * 2
    
    # Berechnung des Nichtzentralitaetsparameters fuer die nicht-zentrale 
    # t-Verteilung (ALternativhypothese)
    nonZentParameter = deltaMu/stdw * sqrt(n^2/(n*2))
    
  }else{
    # Fuer einen gepaarten Zweistichproben-Test oder einen 
    # Einstichproben-Test
    
    # Gesamtstichprobenumfang entspricht dem berechneten (bei gepaarten 
    # Tests: Anzahl der Paare)
    GesamtUmf = n
    
    # Berechnung der Freiheitsgrade
    df = GesamtUmf - 1
    
    # Berechnung des Nichtzentralitaetsparameters der nicht-zentralen
    # t-Verteilung (Alternativhypothese)
    nonZentParameter = deltaMu*sqrt(n)/stdw
  }
  
  # Berechnung des kritischen t-Wertes, unterschieden nach ein- und 
  # zweiseitige Tests
  if(testAlt == "one.sided"){
    # Fuer einseitige Tests (alpha Quantil)
    kritWert = qt(1-alpha,df)
  }else{
    # fuer zweiseitige Tests (alpha/2 Quantil)
    kritWert = qt(1-alpha/2,df)
  }
  
  # x-Vektor fuer die Wahrscheinlichkeitsdichtefunktionen erzeugen
  x = seq(from = -5,to = 10, by = 0.001)
  # y-Werte der Dichtefunktion der zentralen t-Verteilung bestimmen 
  distCentral = dt(x, df, 0)
  # y-Werte der Dichtefunktion der nicht-zentralen t-Verteilung bestimmen
  distNonCentral = dt(x, df, nonZentParameter)
  
  # Alle noetigen Variablen zu einer output Liste kombinieren
  output = list(StichpUmfang=n, GesamtUmf=GesamtUmf, 
                pwr=pwrTest$power, nonZentParameter=nonZentParameter,
                kritWert=kritWert,distCentral=distCentral,
                distNonCentral=distNonCentral, x=x)
  # Output Liste zurueckgeben
  return(output)
}


##### Stichprobenumfang Berechnung - Anova

stpUfg_anova <- function(effMass,effStaerke,nGruppen,alpha,beta){
  # Effektstaerkemass abfragen und ggf. von etq-Quadrat in f umrechnen
  if(effMass == "eta_sq"){
    # Eta-Quadrat in f umrechnen
    f = sqrt(effStaerke/(1-effStaerke))
    # Wenn die Effektstaerke direkt als f gegeben ist
  }else{
    f = effStaerke
  }
  
  # Eventuelle Fehler von pwr.anova.test abfangen (wenn f zu gross ist)
  tryCatch({
    # benoetigten Stichprobenumfang berechnen
    pwrTest = pwr.anova.test(k = nGruppen, 
                             f = f, 
                             power = beta,
                             sig.level = alpha)},
    
    # Kommt es zu einem Fehler ...
    error = function(e){
      return("f_error")
    }
  )
  
  # Stichprobenumfang auf ganze Zahlen runden
  StichpUmfang = round(pwrTest$n,0)
  # mit dem Stichprobenumfang (gerundet auf ganze Zahlen) tatsaechliche 
  # Power berechnen
  aktPower = pwr.anova.test(k = nGruppen, 
                            f = f, 
                            n = StichpUmfang,
                            sig.level = alpha)
  
  # Gesamtstichprobenumfang bestimmen
  GesamtUmf = StichpUmfang * nGruppen
  
  # Freiheitsgerade der systematischen Varianz bestimmen 
  # (Anzahl der Faktorstufen - 1)
  dfs = nGruppen-1
  # Freiheitsgerade der unsystematischen Varianz bestimmen 
  # (Gesamtstichprobenumfang - Anzahl der Faktorstufen)
  dfu = GesamtUmf - nGruppen
  
  # Berechnung des kritischen F-Wertes
  kritWert = qf(1-alpha,dfs,dfu)
  
  # Nicht-Zentralitaetsparameter berechnen
  nonZentParameter =  GesamtUmf * f^2
  
  # x-Werte fuer die Dichtefunktionen erzeugen
  x = seq(from = -5,to = 10, by = 0.001)
  # Werte der zentralen F-Verteilung ermitteln
  distCentral = df(x, dfs, dfu)
  # Werte fuer die nicht-zentrale F-Verteilung ermitteln
  distNonCentral = df(x, dfs, dfu, nonZentParameter)
  
  # Alle Ergebnisse in einer Liste zusammenfassen
  output = list(StichpUmfang=StichpUmfang, GesamtUmf=GesamtUmf, 
                aktPower=aktPower$power, nonZentParameter=nonZentParameter,
                kritWert=kritWert,distCentral=distCentral,
                distNonCentral=distNonCentral, x=x)
  # Output Liste zurueckgeben
  return(output)
}


##### Power Berechnung - Anova

power_anova <- function(effMass,effStaerke,nGruppen,alpha,n){
  # Effektstaerkemass abfragen und ggf. von etq-Quadrat in f umrechnen
  if(effMass == "eta_sq"){
    # Eta-Quadrat in f umrechnen
    f = sqrt(effStaerke/(1-effStaerke))
    # Wenn die Effektstaerke direkt als f gegeben ist
  }else{
    f = effStaerke
  }
  
  # Eventuelle Fehler von pwr.anova.test abfangen (wenn f zu gross ist)
  tryCatch({
    # benoetigten Stichprobenumfang berechnen
    pwrTest = pwr.anova.test(k = nGruppen, 
                             f = f, n = n,
                             sig.level = alpha)},
    
    # Kommt es zu einem Fehler ...
    error = function(e){
      return("f_error")
    }
  )
  
  # Gesamtstichprobenumfang bestimmen
  GesamtUmf = n * nGruppen
  
  # Freiheitsgerade der systematischen Varianz bestimmen 
  # (Anzahl der Faktorstufen - 1)
  dfs = nGruppen-1
  # Freiheitsgerade der unsystematischen Varianz bestimmen 
  # (Gesamtstichprobenumfang - Anzahl der Faktorstufen)
  dfu = GesamtUmf - nGruppen
  
  # Berechnung des kritischen F-Wertes
  kritWert = qf(1-alpha,dfs,dfu)
  
  # Nicht-Zentralitaetsparameter berechnen
  nonZentParameter =  GesamtUmf * f^2
  
  # x-Werte fuer die Dichtefunktionen erzeugen
  x = seq(from = -5,to = 10, by = 0.001)
  # Werte der zentralen F-Verteilung ermitteln
  distCentral = df(x, dfs, dfu)
  # Werte fuer die nicht-zentrale F-Verteilung ermitteln
  distNonCentral = df(x, dfs, dfu, nonZentParameter)
  
  # Alle Ergebnisse in einer Liste zusammenfassen
  output = list(StichpUmfang=n, GesamtUmf=GesamtUmf, 
                pwr=pwrTest$power, nonZentParameter=nonZentParameter,
                kritWert=kritWert,distCentral=distCentral,
                distNonCentral=distNonCentral, x=x)
  # Output Liste zurueckgeben
  return(output)
}
 