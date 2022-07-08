#!/bin/bash

# xset -b  # disable bell - commented out for servers where X is not installed

# Path
export PATH="/usr/sbin:$PATH"
export PATH="/opt/bin:$PATH"    # /opt/bin
export PATH="~/bin:$PATH"       # ~/bin
export PATH=".:$PATH"           # current dir

# Shell
export LS_COLORS=\
'no=0:fi=0:di=34:ln=37:pi=37:so=37:do=37:bd=37:cd=37:or=37:ex=31'
export LS_OPTIONS='-F'
if ls --color=auto                              &>/dev/null; then
  export LS_OPTIONS="$LS_OPTIONS --color=auto"; fi
if ls --dereference-command-line-symlink-to-dir &>/dev/null; then
  export LS_OPTIONS="$LS_OPTIONS --dereference-command-line-symlink-to-dir"; fi
if ls --group-directories-first                 &>/dev/null; then
  export LS_OPTIONS="$LS_OPTIONS --group-directories-first"; fi
if ls -N                                        &>/dev/null; then
  export LS_OPTIONS="$LS_OPTIONS -N"; fi
if ls --show-control-chars                      &>/dev/null; then
  export LS_OPTIONS="$LS_OPTIONS --show-control-chars"; fi
if ls --time-style=+%e\ %b\ %Y\ %k:%M           &>/dev/null; then
  export LS_OPTIONS="$LS_OPTIONS --time-style=+%e\ %b\ %Y\ %k:%M"; fi
if [[ -n $WINDIR ]]; then export LS_OPTIONS="$LS_OPTIONS -Gg"; fi
export PROMPT_COMMAND='echo -ne "\e];/\
`if [[ "$PWD" == "$HOME" ]];then echo "~";else basename "$PWD";fi`\a"'
export PS1='\[\e[1;7;37m\]\h\[\e[0m\]\w $ '
export SHELL=/bin/bash

# Locale
export LANG=en_US.UTF-8  # export LANG=is_IS.UTF-8
# export NLS_LANG=icelandic_america.AL32UTF8  # Oracle UTF-8 output
# export LC_NUMERIC=en_US.UTF-8
# export LC_TIME=en_CA.UTF-8  # show date as 1999-12-31 in Dolphin (~monospace)

# Security certificates
export NSS_DEFAULT_DB_TYPE=sql

# Program: admb
export ADMB_HOME=~/admb

# Program: emacs
if [[ -f /opt/bin/emacs ]]; then
  export EDITOR="/opt/bin/emacs -nw --no-site-file"
else
  export EDITOR="/usr/bin/emacs -nw --no-site-file"
fi

# Program: git
export GIT_FORMAT="%Cred%h%Creset %ai %an - %Cblue %s"

# Program: gmt
export GMTHOME=/usr/share/GMT

# Program: grep
export GREP_COLORS='mt=46'

# Program: desktop (gnome, kde, ...)
export DESKTOP=~

# Program: latex
export TEXINPUTS='.//:~/latex/cls//:~/latex/sty//:'

# Program: lpr
# export LPDEST=marsvin

# Program: r
export _R_CHECK_SYSTEM_CLOCK_=FALSE
export R_HISTFILE=~/r/.Rhistory
export R_HISTSIZE=5000
if [[ -z $R_LIBS_SITE ]]; then export R_LIBS_SITE=~/r/site; fi
export R_LIBS_USER=~/r/library
export R_MAKEVARS_USER=~/r/Makevars
if [[ -f /opt/bin/R ]]; then
  export RSTUDIO_WHICH_R=/opt/bin/R
else
  export RSTUDIO_WHICH_R=/usr/bin/R
fi
export TMPDIR=/tmp
# if [[ -n $WINDIR ]]; then export TZ=UTC; fi

# Alias
alias ..='cd ..'
# alias a='alpine'
alias add='git add'
alias adstudio='~/bin/emacs -Q -mm -l ~/git/admb-project/adstudio/dot/.emacs'
alias 'admb-ide'='~/bin/emacs -Q -mm \
-l ~/git/admb-project/admb/contrib/ide/dot/.emacs -f admb-mode'
alias b='bright'
alias benchmark='time'
alias bfg='java -jar /opt/bfg/bfg-*.jar'
alias bin='chmod 700 ~/bin/*'
alias br='git branch'
alias 'br-full'='git branch -av'
alias 'byte-compile'='emacs -batch -f batch-byte-compile *.el'
alias c='ofp-sam'
alias capslock='echo press both Shift keys'
alias cd..='cd ..'
alias ch755dir='find -type d -exec chmod 755 {} \;'
alias co='git checkout'
alias commit='git commit'
alias 'commits-full'='commits -f'
alias cp='cp --preserve=all'
alias d=docker
alias d2u='dos2unix'
alias di='git diff'
alias diff='diff --color'
alias dir='ll'
alias dock='sudo systemctl start docker'
alias 'dock-off'='sudo systemctl stop docker'
alias e='emacs'
alias eg='emacs --eval "(magit-status)"'
alias eman='emacs -nw -f man'
alias eR='emacs -e "Rni"'
alias f='firefox'
alias file644='find -type f -exec chmod 644 {} \;'
alias 'files'='git show --abbrev-commit --format=oneline --name-only'
alias 'files-full'='git show --format="$GIT_FORMAT" --name-only'
alias 'find?'='echo "find -name '\''*.R'\'' -exec wc -L {} \;"'
alias foxit=FoxitReader
alias g='gwenview'
alias 'git-unset-sshaskpass'='unset SSH_ASKPASS'
alias 'git-url'=url
alias gr='grep -IinRs --exclude-dir=.git --exclude-dir=.svn'
alias grep='grep --color=auto'
alias hi='echo "sudo openvpn --config ~/core/arni/vinna/hi/vpn/client.ovpn &"'
alias htmltidytree='find -iname "*\.html" \
-printf "\n\n\n*** %h/%f\n" -exec tidy -e -utf8 {} \;'
alias htmltidytreeq='find -iname "*\.html" \
-printf "*** %h/%f\n" -exec tidy -e -utf8 -q {} \;'
alias iconvert='convert'
alias ifind='find -iname'
alias 'ip-address'='dig +short myip.opendns.com @resolver1.opendns.com'
alias J='ssh -X hafstokkur'
alias j='ssh hafdruna'
alias k='konsole . &'
alias l="ls -x $LS_OPTIONS"
alias 'l.'="ls -dx $LS_OPTIONS .*"
alias l1="ls -1 $LS_OPTIONS"
alias la="ls -Ax $LS_OPTIONS"
alias latin='export LANG=en_US.ISO8859-1'
alias lh='log | head'
alias ll="ls -l $LS_OPTIONS"
alias ll.="ls -dl $LS_OPTIONS .*"
alias lla="ls -Al $LS_OPTIONS"
alias lld='ls -dl --time-style=+%e\ %b\ %Y\ %k:%M */ | sed "s/\///g"'
alias llh="ls -lh $LS_OPTIONS"
alias llk="ls -l --block-size=K $LS_OPTIONS"
alias lll='ll'
alias llm="ls -l --block-size=M $LS_OPTIONS"
alias llr="ls -lR $LS_OPTIONS"
alias llra="ls -AlR $LS_OPTIONS"
alias lls="ls -l $LS_OPTIONS --time-style=+%e\ %b\ %Y\ %k:%M:%S"
alias llt='ll --time-style="+%Y-%m-%d %H:%M:%S"'
# ll -t sorts by time (newest first), so lls -t and llt -t are useful
alias log='git log --abbrev-commit --format=oneline'
alias 'log-full'='git log --format="$GIT_FORMAT"'
alias longprompt="export PS1='\[\e[1;47;37m\]\h\[\e[0m\]\w $ '"
alias lpbw1='lpr -P marsvin -o sides=one-sided'
alias lpbw2='lpr -P marsvin -o sides=two-sided-long-edge'
alias lpcol1='lpr -P marblettur -o sides=one-sided'
alias lpcol2='lpr -P marblettur -o sides=two-sided-long-edge'
alias lr="ls -FHRx $LS_OPTIONS"
alias lra="ls -AFHRx $LS_OPTIONS"
alias lt='tree -ACF --dirsfirst'
alias lta='tree -AaCF --dirsfirst'
alias m='git commit -m'
alias markdown='pandoc -t plain'
alias md='mkdir -p'
alias merge='git merge'
alias mp3info='id3info'
alias nw='emacs -nw'
alias o=okular
alias one='onedrive --synchronize'
alias optipng='optipng -strip all'
alias pdfbook='book'
alias prtscr='gnome-screenshot -d 5'
alias push='git push'
alias q=condor_q
alias rd='rmdir'
alias reset='git reset'
alias skyperestart='pkill skype; sleep 0.5; skypeforlinux'
alias rm0='find -maxdepth 1 -size 0 -delete'
alias roxy='Roxy'
alias Run='Rscript --vanilla'
alias s='sudo apt update'
alias sao='user3'
alias scp='scp -p'
alias shortlog='git shortlog -ns'
alias 'shortlog-full=git shortlog --format="%Cred%h%Creset %ai - %Cblue %s"'
alias shortprompt="export PS1='$ '"
alias show='git show -s --abbrev-commit --format=oneline'
alias 'show-full'='git show -s --format="$GIT_FORMAT"'
alias 'signal-update'='sudo chmod 4755 /opt/Signal/chrome-sandbox'
alias skeleton='Rscript -e "icesTAF::taf.skeleton()"'
alias space='rename "s/ /_/g"'
alias sqlitestudio='/opt/sqlitestudio/sqlitestudio'
alias stash='git stash'
alias t='sudo apt upgrade'
alias tag='git tag'
alias 'tag-delete'='git push origin --delete'
alias 'tag-full'='git show -s --format="%d $GIT_FORMAT" `git tag`'
alias take='sudo chown -R arnim:arnim'
alias tmb='~/bin/emacs -Q -mm --eval "(setq initial-scratch-message nil)" \
--eval "(add-to-list '\''load-path \"~/emacs/lisp/ess/lisp\")" \
-l ~/emacs/lisp/tmb/tmb.el -f tmb-mode'
alias 'tmb-ide'='~/bin/emacs -Q -mm \
-l ~/core/verk/tmb/ide/dot/.emacs -f tmb-mode'
alias topme='top -u $USER'
alias u='cd ..'
alias u2d='unix2dos'
alias user3='echo \
https://stockassessment.org/datadisk/stockassessment/userdirs/user3/'
alias uu='cd ../..'
alias v='MFCL-Viewer.exe'
alias wgets='wget --no-check-certificate'
alias x='exit'

# Keybindings (only in interactive shell, to avoid login warnings)
if [[ $- =~ i ]]; then
  # The C-SPC binding messes up Dolphin terminal prompt
  # bind '"\C- ":dabbrev-expand'
  bind '"\e ":call-last-kbd-macro'
  bind '"\ei":overwrite-mode'
fi
