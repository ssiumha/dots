#!/bin/bash

if [[ `tmux -V | awk 'END{print ($2>=1.9)}'` ]]; then
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


if [[ -z $SSH_CLIENT ]]; then
  tmux -- \
    set -sq status-right '#{?client_prefix,#[reverse],}' \; \
    set -sqa status-right '#(whoami)@#h' \; \
    set -sqa status-right '#[fg=colour15]|' \; \
    set -sqa status-right '#[fg=colour14]%y-%m-%d(%a) %H:%M' \; \
  ;
else
  tmux -- \
    set -sq status-right '#{?client_prefix,#[reverse],}' \; \
    set -sqa status-right '#[fg=colour3]#(whoami)@#h' \; \
  ;
fi

exit 0;
