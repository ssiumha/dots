layout_rails() {
  PATH_add $(dirname $(find_up bin/rails))

  # alias routes="bundle exec rails routes | fzf"
}
