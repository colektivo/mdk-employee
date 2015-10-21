# TimeTracking for Museum des Kapitalismus in Elm using Electron -

![MDK Time Tracking architecture] (https://github.com/colektivo/mdk-employee/blob/master/docs/infographic/infographic.001.jpg?raw=true "MDK Time Tracking architecture")

Run Elm in [Electron](http://electron.atom.io/)!

Electron is a way to package web apps into a cross platform desktop application and Elm is a fascinating new way of writing front end code.  

You must have the [elm platform](http://elm-lang.org/install) installed as well as [electron](http://electron.atom.io/).

# Running the thing
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
* view functions that:
* modularize code (specially view code)
* add nice design to configuration
* translate configuration texts to german?
* shutdown?
* handle connected from socketIO?
