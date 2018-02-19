# Ploy
An Xresources color scheme manager.

```
Usage: ploy scheme-option [arg ..]
       ploy SCHEME color-option [arg ..]

Create and modify color schemes.

scheme-option [-a|-r|-l|-s]:
  -a SCHEME      Adds a new scheme named SCHEME.
  -r SCHEME      Removes the scheme SCHEME.
  -l             Lists all schemes.
  -s SCHEME      Selects the scheme SCHEME.

color-option [-c|-ca|-cr|-cl]:
  -c COLOR       Displays information about the color COLOR in the 
                 scheme SCHEME.
  -ca COLOR HEX  Adds a color with the name COLOR and the hexidecimal 
                 value HEX to the scheme SCHEME.
  -cr COLOR      Removes the color COLOR from the scheme SCHEME.
  -cl            Lists the colors in scheme SCHEME.
```
