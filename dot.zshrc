# Config history
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

# Set Emacs key-bind
bindkey -e

# Load another config
check_and_source() {
	if [[ -r $1 ]]; then
		source $1
		return 0
	else
		return 1
	fi
}

# Load autojump
check_and_source /usr/share/autojump/autojump.zsh

# HomeBrew
if command -v brew >/dev/null; then
	brew_prefix=`brew --prefix`
# Load zsh-completions
	fpath=($brew_prefix/share/zsh-completions/ $fpath)
# Load autojump
	check_and_source $brew_prefix/etc/autojump.zsh
fi

# completion
zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit
compinit

# cd only with directory name.
setopt auto_cd
# automatically pushd, and you can popd everywhere.
setopt auto_pushd
setopt pushd_ignore_dups
# correct your command line.
setopt correct
# about history
setopt hist_ignore_space
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt noautoremoveslash
setopt share_history
setopt no_hup
setopt no_flow_control
# prevent redirection overwrite.
# disable it if too annoying.
setopt noclobber
# numerical sort of glob.
setopt numeric_glob_sort

# select with cursor keys.
zstyle ':completion:*:default' menu select=2
zstyle ':completion:*' use-cache yes

setopt prompt_subst
setopt prompt_percent
# Print rprompt only on the current commandline.
setopt transient_rprompt

autoload colors
colors

zmodload zsh/system

# With Emacs
[[ $EMACS = t ]] && unsetopt zle

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git cvs svn hg

precmd() {
    # PROMPT SET
    if which gxpc 1>/dev/null 2>&1; then
	PROMPT_GXPC="%{${reset_color}%}`gxpc prompt 2>/dev/null` %{${fg[green]}%}"
    else
	PROMPT_GXPC=" "
    fi
    if [[ -n "${REMOTEHOST}${SSH_CONNECTION}" ]]; then
    # in SSH connection (MAGENTA)
	HOST_COLOR='magenta'
    else
    # no SSH connection (GREEN)
	HOST_COLOR='green'
    fi
    case ${UID} in
	0)
	    PROMPT="%{${fg[green]}%}%~
${PROMPT_GXPC}%{${reset_color}%}%B%#%b"
	    USER_COLOR='white'
	    ;;
	*)
	    PROMPT="%{${fg[green]}%}%~
${PROMPT_GXPC}%#%{${reset_color}%}"
	    USER_COLOR="${HOST_COLOR}"
	    ;;
    esac
    PROMPT2="%_%{${fg[green]}%}>%{${reset_color}%} "
    SPROMPT="%{${fg[green]%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
    PROMPT="%{$fg[${USER_COLOR}]%}%n%{${reset_color}%}%{$fg[${HOST_COLOR}]%}@%m ${PROMPT} "
    psvar=()
    if LANG=en_US.UTF-8 vcs_info 2> /dev/null; then
	psvar[1]="$vcs_info_msg_0_"
	RPROMPT=$'%F{yellow}%1v%f'
    fi
    case "${TERM}" in
	kterm*|xterm|mlterm)
	    echo -ne "\033]0;${USER}@${HOST%%.*}: ${PWD/~HOME/~}\007"
            ;;
    esac
}

zmodload -i zsh/mathfunc

case "${OSTYPE}" in
    linux*|darwin*|darwin*)
	which dircolors > /dev/null && eval `dircolors -b`
	zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
	;;
esac

# Local config
check_and_source ~/.zshrc.local
# alias config
check_and_source ~/.alias

return
