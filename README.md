cabal-edit
==========

![Cabal CI](https://github.com/sdiehl/cabal-edit/workflows/Cabal%20CI/badge.svg)
![Stack CI](https://github.com/sdiehl/cabal-edit/workflows/Stack%20CI/badge.svg)

This is an extension to Haskell's package manager Cabal to allow you to add,
remove, and upgrade dependencies by modifying your cabal file from the
command line. This behaves similar to  `install --save` commands in other
package managers.

* `cabal-edit add`
* `cabal-edit list`
* `cabal-edit upgrade`
* `cabal-edit remove`
* `cabal-edit extensions`
* `cabal-edit format`
* `cabal-edit rebuild`

For example to setup a new project one often wants to add common dependencies
like `text` and `aeson`. We can use `cabal-edit` to automatically append these
to our dependency list and deduce the latest versions from Hackage.

```bash
$ cabal init --lib --cabal-version=3.0    

$ cabal-edit add aeson                
Adding dependency: aeson ^>=1.5 to sample.cabal

$ cabal-edit add text 
Adding dependency: text ^>=1.2 to sample.cabal
```

If we want to depend on a specific version of `aeson`, we can pass this
explicitly as an argument.

```bash
$ cabal-edit add aeson==1.4.0.0
```

The Hackage database can be queried from the command line to search for all
available versions to use with the `list` command.

```bash
$ cabal-edit list filepath
1.0
1.1.0.0
1.1.0.1
1.1.0.2
1.1.0.3
1.1.0.4
1.2.0.0
1.2.0.1
1.3.0.0
1.3.0.1
1.3.0.2
1.4.0.0
1.4.1.0
1.4.1.1
1.4.1.2
1.4.2
1.4.2.1
```

The `format` command will canonicalise the Cabal into by parsing it and running
it through the pretty printer again.

```bash
$ cabal-edit format
Formatting: sample.cabal
```

You may then wish to then run [cabal-fmt] on the outputted file to canonicalise
it furter and layout the dependency table.  Auto-formatting of the outputted
cabal may be added in the future.

[cabal-fmt]: https://github.com/phadej/cabal-fmt


The `extensions` command will enumerate all the default extensions enabled for
the given library. This is useful if you wish to add these headers to files
within the project.

```bash
$ cabal-edit extensions

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
```

Installation
------------

Download this repository.

```bash
$ git clone git@github.com:sdiehl/cabal-edit.git
$ cd cabal-edit
```

Then install either with Stack, Cabal or Nix.

```bash
$ stack install 
$ cabal install --installdir=/home/$USER/.local/bin
$ nix-build -A cabal-edit
```

*Note: This library links directly against Cabal, so you must be using the same
version of Cabal you intend to compile your package against. It is reccomened to
use Cabal > 3.0 to support modern versioning schemes.*

Shell Completions
------------------

We can tab complete on the Hackage package index we install the local
completions for you shell. Run one of the following commands to generate the
shell completer appropriate to your shell. Then add the output to one of
`~/.zshrc`, `~/.bashrc` or `~/.config/fish/config.fish`.

```bash
$ cabal-edit --zsh-completion-script cabal-edit
$ cabal-edit --bash-completion-script cabal-edit
$ cabal-edit --fish-completion-script cabal-edit
```

This will completion against the Hackage database prefixed by name.

```
$ cabal-edit add gh
zsh: do you wish to see all 103 possibilities (15 lines)?
$ cabal-edit add ghc-pa
ghc-parmake  ghc-parser   ghc-paths
```

**bash**

```bash
_cabal-edit()
{
    local CMDLINE
    local IFS=$'\n'
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $(cabal-edit "${CMDLINE[@]}") )
}

complete -o filenames -F _cabal-edit cabal-edit
```

**zsh**

```bash
#compdef cabal-edit

local request
local completions
local word
local index=$((CURRENT - 1))

request=(--bash-completion-enriched --bash-completion-index $index)
for arg in ${words[@]}; do
  request=(${request[@]} --bash-completion-word $arg)
done

IFS=$'\n' completions=($( cabal-edit "${request[@]}" ))

for word in $completions; do
  local -a parts

  # Split the line at a tab if there is one.
  IFS=$'\t' parts=($( echo $word ))

  if [[ -n $parts[2] ]]; then
     if [[ $word[1] == "-" ]]; then
       local desc=("$parts[1] ($parts[2])")
       compadd -d desc -- $parts[1]
     else
       local desc=($(print -f  "%-019s -- %s" $parts[1] $parts[2]))
       compadd -l -d desc -- $parts[1]
     fi
  else
    compadd -f -- $word
  fi
done
```

**fish**

```bash
 function _cabal-edit
    set -l cl (commandline --tokenize --current-process)
    # Hack around fish issue #3934
    set -l cn (commandline --tokenize --cut-at-cursor --current-process)
    set -l cn (count $cn)
    set -l tmpline --bash-completion-enriched --bash-completion-index $cn
    for arg in $cl
      set tmpline $tmpline --bash-completion-word $arg
    end
    for opt in (cabal-edit $tmpline)
      if test -d $opt
        echo -E "$opt/"
      else
        echo -E "$opt"
      end
    end
end

complete --no-files --command cabal-edit --arguments '(_cabal-edit)'
```

License
-------

MIT License
Copyright (c) 2020, Stephen Diehl
