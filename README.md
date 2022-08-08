# fgs-swepub

This program transforms bibliographic formats that can be used with Swepub using
[CERIF](https://github.com/EuroCRIS/CERIF-DataModel). It can be built and run
using the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).
After setting up Stack and cloning the repository (or after pulling or making 
changes to the code), run `stack build`.


To transform the MODS XML example file to CERIF XML.

```
stack exec fgs-swepub -- -f swepubjson -t cerifxml < test/Slupub_82412f.mods.xml
```

Pipe into [xmllint](http://xmlsoft.org/) for indentation.

```
stack exec fgs-swepub -- -f swepubjson -t cerifxml < test/Slupub_82412f.mods.xml | xmllint --format -
```

The output should be identical to the file `test/Slupub_82412f.cerif.xml`.
