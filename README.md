# fgs-swepub

This program transforms Swepub
[BIBFRAME JSON Lines](https://www.kb.se/samverkan-och-utveckling/swepub/datamodell/swepub-bibframe.html) to
[CERIF](https://github.com/EuroCRIS/CERIF-DataModel). It can be built and run
using the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).
After setting up Stack and cloing the repository, run `stack build`.

To transform the example file to CERIF with native Haskell representation.
Note that the transformations are still very incomplete, and a Swepub class
scheme should be added.

```
stack exec fgs-swepub -- -f swepubjson -t cerifnat < SwepubBibframe_82412.jsonl
```

To transform the example file to CERIF XML.

```
stack exec fgs-swepub -- -f swepubjson -t cerifxml < SwepubBibframe_82412.jsonl
```

Pipe into [xmllint](http://xmlsoft.org/) for indentation.

```
stack exec fgs-swepub -- -f swepubjson -t cerifxml < SwepubBibframe_82412.jsonl | xmllint --format -
```

To output the example file in Swepub data format.

```
stack exec fgs-swepub -- -f swepubjson -t swepubnat < SwepubBibframe_82412.jsonl
```
