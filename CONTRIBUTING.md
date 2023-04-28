# Contributors' Guide

Thank you for considering contributing to the maintenance or development of
`hi-file-parser`. We hope the following information will assist you.

## Code Quality

The project uses [yamllint](https://github.com/adrienverge/yamllint) as a YAML
file quality tool and [HLint](https://github.com/ndmitchell/hlint) as a code
quality tool.

### Linting of YAML files

The yamllint configuration extends the tool's default and is set out in
`.yamllint.yaml`. In particular, indentation is set at 2 spaces and `- ` in
sequences is treated as part of the indentation.

### Linting of Haskell source code

The HLint configuration is set out in `.hlint.yaml`.

The objective is clear code, not consise code or what HLint thinks. Contributors
need not follow dogmatically the suggested HLint hints but are encouraged to
debate their usefulness. If you find a HLint hint is not useful and detracts
from the objective, consider marking it in the configuration file to be ignored.

## Testing

The library is tested using GitHub Actions. The GitHub Actions script aims to
test builds with versions of GHC released within the last three years. GHC 8.8.4
was released on 15 July 2020.

A separate Stack project-level configuration file is provided for the most
recent release of each major GHC version for which there is a Stackage snapshot.
`stack.yaml` is based on the most recent LTS Haskell snapshot on Stackage.

The tests seek to test the parsing of `*.hi` files for the latest version of
each major version of GHC and, if different, the latest version of each major
version of GHC that uses a different `*.hi` file format.
