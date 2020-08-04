#!/bin/bash

if [[ `tmux -V | awk 'END{print ($2>=1.9)}'` -eq 1 ]]; then
  tmux -- \
    bind c new-window -c "#{pane_current_path}" \; \
    bind \" split-window -v -c "#{pane_current_path}" \; \
    bind % split-window -h -c "#{pane_current_path}" \; \
  ;
else
  tmux -- \
    bind c new-window \; \
    bind \" split-window -v \; \
    bind % split-window -h \; \
  ;
fi


if [[ `tmux -V | awk 'END{print ($2>=2.4)}'` -eq 1 ]]; then
  tmux -- \
    bind -T copy-mode-vi Escape send -X clear-selection \; \
    bind -T copy-mode-vi C-v    send -X rectangle-toggle \; \
    bind -T copy-mode-vi v      send -X begin-selection \; \
    bind -T copy-mode-vi V      send -X select-line \; \
    bind -T copy-mode-vi C-a    send -X start-of-line \; \
    bind -T copy-mode-vi C-e    send -X end-of-line \; \
    bind -T copy-mode-vi w      send -X next-word \; \
    bind -T copy-mode-vi e      send -X next-word-end \; \
    bind -T copy-mode-vi b      send -X previous-word \; \
    bind -T copy-mode-vi /      send -X search-forward \; \
    bind -T copy-mode-vi ?      send -X search-backward \; \
    bind -T copy-mode-vi C-b    send -X page-up \; \
    bind -T copy-mode-vi C-f    send -X page-down \; \
    bind -T copy-mode-vi C-u    send -X halfpage-up \; \
    bind -T copy-mode-vi C-d    send -X halfpage-down \; \
    bind -T copy-mode-vi Enter copy-pipe "$DOTFILES/bin/clipcopy" \; \
    bind -T copy-mode-vi y     copy-pipe "$DOTFILES/bin/clipcopy" \; \
  ;
else
  tmux -- \
    bind -t vi-copy Escape clear-selection \; \
    bind -t vi-copy C-v    rectangle-toggle \; \
    bind -t vi-copy v      begin-selection \; \
    bind -t vi-copy V      select-line \; \
    bind -t vi-copy C-a    start-of-line \; \
    bind -t vi-copy C-e    end-of-line \; \
    bind -t vi-copy w      next-word \; \
    bind -t vi-copy e      next-word-end \; \
    bind -t vi-copy b      previous-word \; \
    bind -t vi-copy /      search-forward \; \
    bind -t vi-copy ?      search-backward \; \
    bind -t vi-copy C-b    page-up \; \
    bind -t vi-copy C-f    page-down \; \
    bind -t vi-copy C-u    halfpage-up \; \
    bind -t vi-copy C-d    halfpage-down \; \
    bind -t vi-copy Enter copy-pipe "$DPATH/bin/clipcopy" \; \
    bind -t vi-copy y     copy-pipe "$DPATH/bin/clipcopy" \; \
  ;
fi


if [[ -z $SSH_CLIENT ]]; then
  tmux -- \
    set -sq status-right '#{?client_prefix,#[reverse],}' \; \
    set -sqa status-right '#[fg=colour09]' \; \
    set -sqa status-right '#(test kubectl && kubectl config get-contexts \
                            | perl -nale "printf q( <k8s:%%s> ),
                                @F[1] =~ s!.+/(.+)!\$1!r if /^\*/")' \; \
    set -sqa status-right '#[fg=colour11]#(whoami)@#h' \; \
    set -sqa status-right '#[fg=colour15]|' \; \
    set -sqa status-right '#[fg=colour14]%y-%m-%d(%a) %H:%M' \; \
    set -sq pane-border-fg colour1 \; \
    set -sq pane-active-border-fg colour9 \; \
  ;
else
  tmux -- \
    set -sq status-right '#{?client_prefix,#[reverse],}' \; \
    set -sqa status-right '#[fg=colour3]#(whoami)@#h' \; \
  ;
fi

exit 0;
