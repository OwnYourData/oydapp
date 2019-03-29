# <img src="https://github.com/OwnYourData/oydapp/raw/master/assets/service.png" width="92"> OwnYourData R Framework    
Dieses R-Packet stellt die Infrastruktur zum Zugriff auf den OwnYourData Datentresor für [Shiny Apps](https://shiny.rstudio.com) zur Verfügung.

&nbsp;    

## Installation und Verwendung    

Das Packet kann mit folgenden Befehlen installiert und eingebunden werden:    
```
devtools::install_github('ownyourdata/oydapp')
library(oydapp)
```    

## Struktur einer OwnYourData Shiny App    

Das Plugin [app-answers](https://github.com/OwnYourData/app-answers) ist ein gutes Beispiel für die Entwicklung einer eigenen Shiny App. Die folgende Struktur und Vorgehensweise wird bei der Entwicklung empfohlen:

* `app.R`: enthält die Aufrufe des client- und serverseitigen Codes einer Shiny App    
* `constants.R`: setzt allgemeine Attribute wie Name, Version, etc. und enthält die App-spezifischen Übersetzungen    
* `appLogic.R`: Programmlogik des Plugins    
* `uiBody.R`: Code zur Darstellung des User Interface    

&nbsp;    

## Verbessere das OwnYourData R Framework    

Bitte melde Fehler oder Vorschläge im [GitHub Issue-Tracker](https://github.com/OwnYourData/oydapp/issues) und halte dich dabei an die [Contributor Guidelines](https://github.com/twbs/ratchet/blob/master/CONTRIBUTING.md).

Wenn du selbst an der App mitentwickeln möchtest, folge diesen Schritten:

1. Fork it!
2. erstelle einen Feature Branch: `git checkout -b my-new-feature`
3. Commit deine Änderungen: `git commit -am 'Add some feature'`
4. Push in den Branch: `git push origin my-new-feature`
5. Sende einen Pull Request

&nbsp;    

## Lizenz

[MIT Lizenz 2019 - OwnYourData.eu](https://raw.githubusercontent.com/OwnYourData/oydapp/master/LICENSE)
