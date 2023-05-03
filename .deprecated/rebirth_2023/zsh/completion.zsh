autoload -Uz compinit && compinit -C -d "$HOME/.local/zsh/zcompdump"

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*' list-separator '-->'
zstyle ':completion:*:manuals' separate-sections true

compdef _ssh ssh
_ssh() {
  _ssh_keys() {
    if [[ -r "$HOME/.ssh" ]]; then
      compadd -M 'm:{a-zA-Z}={A-Za-z} r:|.=* r:|=*' \
        ${(@M)$(ls "$HOME/.ssh"):#(id_*[^p][^u][^b]|*.pem)}
    fi
  }

  _ssh_hosts() {
    if [[ -r "$HOME/.ssh/config" ]]; then
      compadd -M 'm:{a-zA-Z}={A-Za-z} r:|.=* r:|=*' \
        $( while IFS=$'=\t ' read -r key hosts; do
            if [[ "$key" == Host ]]; then
              echo ${(z)hosts:gs/*//:gs/?//};
            fi
          done < "$HOME/.ssh/config"
        )
    fi
  }

  _arguments -C -s \
    '-i: :_ssh_keys' \
    '*: :_ssh_hosts'

  return 0
}

