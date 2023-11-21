#!/bin/bash

# TODO
#   `!p python code...`
#   `date +%d`
#   perl -e '$s=q(test code long ${0}); $s =~ s{\$\{(.+?)\}}{ (0+$1) * 99; eval($code) }ge; print $s;'
#   check filename (docker-compose, kube...)
#   fix space query

show_help() {
  echo "Usage: $0 <filetype>"
}

if [ -z "$1" ]; then
  show_help
  exit 1
fi

filetype=$1
filepath=$2

cd $HOME/dotfiles/config/nvim/snips

tmpfile=$(mktemp)

snippet_targets=("all.snippets")
if [ "$filetype" = "zsh" ]; then snippet_targets+=("sh.snippets");
elif [[ "$filetype" == "yaml" && "$filepath" == *"docker-compose"* ]]; then snippet_targets+=("docker_compose.snippets");
else snippet_targets+=("$filetype.snippets" );
fi

rg --with-filename --color=never '^snippet ' ${snippet_targets[@]} \
  | perl -ne 's/^(?<f>.+?):snippet (?<n>.+?) "(?<c>.+)"//; printf qq{%-30s\t\033[33m%-20s\033[0m\t%s\n}, $+{n}, $+{c}, $+{f}' \
  | fzf --delimiter "\t" --with-nth='1,2' --select-1 --height '~50%' \
        --preview 'perl -ne "\$a={1}; print if /^snippet \$a/../endsnippet/ and not /^snippet|endsnippet/" {3} \
          | bat --plain --color always --language ruby' \
        --preview-window 'right:nowrap' \
        --bind 'ctrl-e:become(nvim {3})' \
        --bind 'enter:become(perl -ne "\$a={1}; print if /^snippet \$a\b/../endsnippet/ and not /^snippet|endsnippet/" {3})' > $tmpfile

if [ ! -s "$tmpfile" ]; then
  exit 1
fi

replace_words=$(perl -nle 'print "$1" if /\${(.+?)}/' $tmpfile | sort -r | perl -nle '/(^\d+)/; print unless $seen{$1}++' | sort)

if [ ! -z "$replace_words" ]; then
  echo "$replace_words" | while IFS=$'\n' read -r line; do
    rnum=$(echo $line | perl -ne '/(\d+):?(.+)?/; print $1')
    rdefault=$(echo $line | perl -ne '/(\d+):?(.+)?/; print $2')

    input_word=$(echo '' | fzf --delimiter "\t" --query "$rdefault" \
      --header "$line" \
      --preview-window 'bottom:nowrap:99%' \
      --preview "perl -pe '\$a=q({q}); s;\\$\{$rnum.*?};\$a;g' $tmpfile | bat" \
      --bind "ctrl-c:become(echo $rdefault)" \
      --bind 'enter:become(echo {q})')

    perl -i -pe "s;\\$\{$rnum.*?};$input_word;g" $tmpfile
  done
fi

cat $tmpfile
