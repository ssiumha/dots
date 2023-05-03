layout_poetry() {
  if [[ ! -f pyproject.toml ]]; then
    log_error 'not found pyproject.toml. run `poetry init`'
    return 1
  fi

  local VENV=$(poetry env list --full-path | cut -d' ' -f1)
  if [[ -z $VENV || ! -d $VENV/bin ]]; then
    log_error 'not found venv. run `poetry install`'
    return 1
  fi

  export VIRTUAL_ENV=$VENV
  export POETRY_ACTIVE=1
  PATH_add "$VENV/bin"
}
