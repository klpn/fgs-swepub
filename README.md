# fgs-swepub

This program transforms Swepub
[BIBFRAME JSON Lines](https://www.kb.se/samverkan-och-utveckling/swepub/datamodell/swepub-bibframe.html) to
[CERIF](https://github.com/EuroCRIS/CERIF-DataModel). It can be built and run
using the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).
After setting up Stack and cloing the repository, run `stack build`.

To transform the example file to CERIF (support for XML output and XML class
scheme will be implemented).

```
stack exec fgs-swepub < SwepubBibframe_82412.jsonl
```

To output the example file in Swepub data format.

```
stack exec fgs-swepub -- -s < SwepubBibframe_82412.jsonl
```
