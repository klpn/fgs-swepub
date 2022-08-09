# fgs-swepub

This program transforms bibliographic formats that can be used with Swepub using
[CERIF](https://github.com/EuroCRIS/CERIF-DataModel). It can be built and run
using the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).
After setting up Stack and cloning the repository (or after pulling or making 
changes to the code), run `stack build`. The program can be copied to a binary path
with `stack install`. Otherwise, run it with the `stack exec` prefix.

Usage of the program .

```
fgs-swepub options
  -f INFORMAT   --from=INFORMAT  Input format
  -t OUTFORMAT  --to=OUTFORMAT   Output format
```

To transform the MODS XML example file to CERIF XML (with `stack exec` prefix).

```
stack exec fgs-swepub -- -f modsxml -t cerifxml < test/Slupub_82412f.mods.xml
```

Pipe into [xmllint](http://xmlsoft.org/) for indentation.

```
stack exec fgs-swepub -- -f modsxml  -t cerifxml < test/Slupub_82412f.mods.xml | xmllint --format -
```

The output should be identical to the file `test/Slupub_82412f.cerif.xml`. If this file is
tranformed to modsxml, the output should be identical to the file
`test/Slupub_82412f.mods.test.xml`.

## Supported formats

Note that the support of the different formats is still very incomplete.

The input is transformed to a CERIF data structure, from which the output is generated,
regardless of input format.

### Input formats

* `slupubjson`: JSON generated from Slupub.
* `swepubjson`: Bibframe JSON lines generated from Swepub.
* `cerifxml`: CERIF XML.
* `modsxml`: MODS XML.

### Output formats

* `cerifnat`: CERIF (as native Haskell data structure).
* `cerifxml`: CERIF XML.
* `modsxml`: MODS XML.
