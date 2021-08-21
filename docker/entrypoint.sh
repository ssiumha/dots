#!/usr/bin/env bash
set -e

sync_host_user() {
  USER_ID=${HOST_USER_ID:-9001}
  GROUP_ID=${HOST_GROUP_ID:-9001} # mac: staff=20 -> in ubuntu: dialout=20

  # user_id.size.zero?
  if [ ! -z "$USER_ID" ] && [ "$(id -u me)" != "$USER_ID" ]; then
    groupadd --non-unique -g "$GROUP_ID" host-group
    usermod --non-unique --uid "$USER_ID" --gid "$GROUP_ID" me

    echo "Updated uid & gid: $USER_ID, $GROUP_ID"
  fi

  chown me: /var/run/docker.sock
}

################
# RUN
################

echo '---- CMD -----'
echo "$@"
echo '--------------'

if [ "$1" == "root" ]; then
  exec /bin/zsh

else
  sync_host_user

  su me -c 'touch ~/.zshrc'
  su me --login
fi
