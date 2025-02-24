# ~/.zshenv
export ZDOTDIR="$HOME/.config/zsh"


# zshを起動したときに必ず読み込まれるファイル

# シムリンクを書き換える必要がある場合等、一時的にgsedへ切り替える
# alias sed='gsed'

___MY_VMOPTIONS_SHELL_FILE="${HOME}/.jetbrains.vmoptions.sh"; if [ -f "${___MY_VMOPTIONS_SHELL_FILE}" ]; then . "${___MY_VMOPTIONS_SHELL_FILE}"; fi
