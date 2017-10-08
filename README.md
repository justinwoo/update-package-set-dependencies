## Usage

```
psc-package install
npm install
pulp build --to index.js

node index.js packages.json
```

This will spit out `wip.json` as it works, since the Bower API often crashes on Windows and I don't care enough about improving this tool for now. PRs welcome. If you don't use Windows at home, probably you don't need to worry about this detail at all.

### This tool Writes over the packages.json file that you point to it, but it's not like it matters because your package sets are always version controlled. When in doubt, go to your package-sets repo and commit all of your changes.