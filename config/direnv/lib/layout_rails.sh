layout_rails() {
  PATH_add $(dirname ${BASH_SOURCE[0]})/rails;

  # PATH_add $(dirname $(find_up bin/rails))
  # alias routes="bundle exec rails routes | fzf"
}
