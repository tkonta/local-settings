# local-settings

開発環境セットアップファイル等の管理

brew install
```
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

tmux install
```
brew install tmux
```

anyenv install

```
git clone https://github.com/riywo/anyenv ~/.anyenv
echo 'export PATH="$HOME/.anyenv/bin:$PATH"' >> ~/.zshrc
echo 'eval "$(anyenv init -)"' >> ~/.zshrc
exec $SHELL -l
anyenv install --init
```

perlEnv install
```
anyenv install plenv
exec $SHELL -l
plenv install 5.30.1-RC1
plenv install-cpanm
plenv global 5.30.1-RC1
```

pyenv install
```
anyenv install pyenv
exec $SHELL -l
pyenv install 3.8.1
pyenv global 3.8.1
```
