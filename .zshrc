# If not running interactively, don't do anything
[[ $- != *i* ]] && return

bindkey '^E' end-of-line              # Ctrl-E
bindkey '^[[E' end-of-line            # End
bindkey '^[[P' delete-char            # Delete
bindkey '^[[1;5C' forward-word        # Ctrl-RightArrow
bindkey '^[[1;5D' backward-word       # Ctrl-LeftArrow
bindkey '^[[H' beginning-of-line      # Home
bindkey '^H' beginning-of-line        # Ctrl-H

# Run SSH agent
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [[ ! "$SSH_AUTH_SOCK" ]]; then
    eval "$(<"$XDG_RUNTIME_DIR/ssh-agent.env")"
fi

# Add aliases
if [ -f .zsh_aliases ]; then
    source .zsh_aliases
fi

# Function which adds changed dotfiles to git stage
track_dotfiles () {
		for dotfile in `cat dotfiles_to_track`; do
				git add -f $dotfile
		done
}

# Function which you can limit cpu with like:
# limit_cpu firefox
limit_cpu () {
    systemd-run --user --scope -p CPUQuota=25% $1
}

# Run dotfiles function if the dotfiles_to_track file exists
if [ -f dotfiles_to_track ]; then
	 track_dotfiles
fi

# Enable colors
autoload -U colors && colors

# KITTY
# Print image
# kitty +kitten icat --silent --align left Pictures/kittyLogo.png
# kitty + complete setup zsh | source /dev/stdin

# Make it possible to use commands in prompts
setopt prompt_subst
# username@host [git branch] as prompt
PROMPT='%F{green}%n%f@%F{magenta}%m%f %F{yellow}$(git rev-parse --abbrev-ref HEAD 2>/dev/null) %F{blue}%B%~%b%f %# '

# Use historyfile
HISTFILE=~/.cache/zsh/zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory

# Better tab completion
autoload -U compinit
compinit

# No longer need cd to switch dirs
setopt autocd

# Add deploy to website script to PATH
export PATH="/home/rmw/Documents/Projects/GitProjects/hugo-source-pages/hugo_build_and_deploy.sh:$PATH"

# Add CUDA to path, because annoying Nvidia stuff
# export PATH="/opt/cuda/bin:$PATH"
# Older CUDA version, because blender
export PATH="/opt/cuda-10.1/bin:$PATH"
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/opt/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/opt/miniconda3/etc/profile.d/conda.sh" ]; then
#         . "/opt/miniconda3/etc/profile.d/conda.sh"
#     else
#         export PATH="/opt/miniconda3/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# <<< conda initialize <<<

# conda deactivate
