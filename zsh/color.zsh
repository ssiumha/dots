# '\e[x;ym $TEXT \e[m'
# x;y;.. is color pair
# 0:ResetAll, 1:Bold, 2:Dim, 4:Underline, 5:Blink, 7:Invert, 8:Hidden
# 21:ResetBold, 22:RestDim, 24:Reset UL, 25: Reset Blink ...
# 30 ~ 37, 90~97 : Foreground
# 39 : Default Foreground
# 40 ~ 47, 100~107 : Background
# 49 : Default Foreground
# 38;5;colcode or 48;5;colcode : use 255 colors
# 90~97 == 1;30~1;37
#
# or
# echo $(tput setaf 4) TEXT
# tput [ bold | rev | sgr0 | setaf $colcode | setab $colcode ]
#
# pirnt current rgb: printf "\033Ptmux;\033\033]4;9;?\007\033\\"

if [ -n "$TMUX" ]; then
  # tell tmux to pass the escape sequences through
  # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
  printf_template="\033Ptmux;\033\033]4;%d;#%s\007\033\\"
  printf_template_var="\033Ptmux;\033\033]%d;#%s\007\033\\"
elif [ "${TERM%%-*}" = "screen" ]; then
  # GNU screen (screen, screen-256color, screen-256color-bce)
  printf_template="\033P\033]4;%d;#%s\007\033\\"
  printf_template_var="\033P\033]%d;#%s\007\033\\"
else
  printf_template="\033]4;%d;#%s\033\\"
  printf_template_var="\033]%d;#%s\033\\"
fi

printf $printf_template \
  0  "31353c" 1  "b44b4b" 2  "5c9e5c" 3  "ce955b" \
  4  "3879b7" 5  "905ddb" 6  "1cbfbf" 7  "737680" \
  8  "505059" 9  "fe93be" 10 "b0ea77" 11 "dbdb70" \
  12 "95acda" 13 "d47fd4" 14 "7ec4a0" 15 "989ca7"

# fg bg cursor
printf $printf_template_var \
  10 "e1e1e1" 11 "1d1f21" 12 "e1e1e1"

unset printf_template
unset printf_template_var
