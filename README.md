# TimeTracking for Museum des Kapitalismus in Elm using Electron -

## Motivation

The capitalism is destroying most people life and our planet, but
not so many people talks about it and accepts it naturally
without questioning it.
We are worry about it, and we want to put the debate and questions over
the table.
MDK creators put this topic on the table exposing how this system works and
exposing the mechanics to the people and give the chance to think about it.

You can check more in the [Museum des Kapitalusmus web site] (http://museumdeskapitalismus.de/)

## The architecture

![MDK Time Tracking architecture] (https://github.com/colektivo/mdk-employee/blob/master/docs/infographic/infographic.001.jpg?raw=true "MDK Time Tracking architecture")

### MDK-police

This app access USB-devices via HID. Is a Java 8 app that communicates
with mdk-boss via Socket.io. Requires Java 8 Runtime.

### MDK-boss

This app receives the Card Id and USB Location of the device and store it
into the DB for later calculations.

### MDK-employee

When the Museum Visitor put the card on the last reader the mdk-boss app sends
the tracking information to mdk-employee, and mdk-employee renders the
information about the time tracking.

## Why Elm-lang?
We found elm-lang very promising in term of web development and as this app
will run in a local computer we found a very good chance to play with it.

# Making mdk-employee work

Run Elm in [Electron](http://electron.atom.io/)!

Electron is a way to package web apps into a cross platform desktop application and Elm is a fascinating new way of writing front end code.  

You must have the [elm platform](http://elm-lang.org/install) installed as well as [electron](http://electron.atom.io/).


Compile the elm file into javascript.
```bash
elm-make src/TimeTracking.elm
```

Then run electron on the current directory.
```bash
electron .
```

### NOTES

If the number of the rooms change on the space the VisitorData should be changed to have a TimeReport size accordingly

### TODO
* modularize code (specially view code)
* add nice design to configuration
* translate configuration texts to german?
* shutdown?
* make more settings configurable, now hardcoded in mdk-boss
