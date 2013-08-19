A regular expression library using NFAs. Also has support for graphical illustration through graphviz.

# Usage
## To Build
```bash
cabal-dev install --enable-tests regex/ regex-server/
```
## To Run
```bash
./cabal-dev/bin/regex-server
```

Navigate to http://localhost:8000/regex?r=&lt;your regex here&gt;. Be careful with URL encoding, particularly for `+`.
