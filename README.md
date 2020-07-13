cabal-edit
==========

This is an extension to Haskell's package manager Cabal to allow you to add,
remove, and upgrade dependencies by modifying your cabal file from the
command line.

* `cabal-edit add`
* `cabal-edit list`
* `cabal-edit upgrade`
* `cabal-edit remove`

For example to setup a new 

```bash
$ cabal init --lib --cabal-version=3.0    

$ cabal-edit add aeson                
Adding dependency: aeson ^>=1.5 to sample.cabal
$ sample cabal-edit add text 
Adding dependency: text ^>=1.2 to sample.cabal
```

You may wish to then run [cabal-fmt] on the outputted file to canonicalise it.
Auto-formatting of the outputted cabal may be added in the future.

[cabal-fmt]: https://github.com/phadej/cabal-fmt

Installation
------------

Download this repository.

```bash
$ git clone git@github.com:sdiehl/cabal-edit.git
$ cd cabal-edit
```

Then install either with Stack or Cabal.

```bash
$ stack install 
$ cabal install --installdir=/home/$USER/.local/bin
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
$ cabal-edit --bash-completion-script cabal-edit
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
