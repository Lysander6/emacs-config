# .emacs.d

```sh
rm -r ~/.emacs.d

git clone https://github.com/Lysander6/emacs-config.git ~/.emacs.d
```

## Compiling Emacs with fast JSON parser, native compilation and tree-sitter

Based on:
- [Speed up Emacs with libjansson and native elisp compilation](https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation)
- [Experimenting With the Built-in Tree-sitter Support in Emacs](https://blog.markhepburn.com/posts/experimenting-with-the-built-in-treesitter-support-in-emacs/)

Tested on Ubuntu 20.04 (focal).

Emacs 30.0.50

```sh
# prerequisites

sudo apt-get update
sudo apt-get install -y apt-transport-https ca-certificates curl gnupg-agent software-properties-common git

sudo add-apt-repository ppa:ubuntu-toolchain-r/ppa
sudo apt-get update -y
sudo apt-get install -y gcc-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev

cp /etc/apt/sources.list ~/sources.list.backup
sudo sed -i 's/# deb-src/deb-src/' /etc/apt/sources.list
sudo apt-get update

sudo apt-get build-dep -y emacs

cd ~

# tree-sitter

git clone https://github.com/tree-sitter/tree-sitter.git
# commit 3e8d029ab34b93300c6523a138d9af9298a89cb3

cd tree-sitter
make all
sudo make install
sudo ldconfig

cd ~

# emacs

git clone --depth 1 https://github.com/emacs-mirror/emacs.git
# commit 9377a3c889aa3b178a11a3b849c3d1665da096d6

export CC="gcc-10"

cd emacs/
./autogen.sh

# optionally inspect available toggles `./configure --help`

TREE_SITTER_CFLAGS=-I~/tree-sitter/lib/include TREE_SITTER_LIBS="-L~/tree-sitter -ltree-sitter" ./configure \
    --with-native-compilation \
    --without-mailutils \
    --with-json \
    --with-tree-sitter \
    --without-compress-install

make -j 4

sudo make install

# optionally turn on emacs and check (`M-:`) if both `(native-comp-available-p)`
# and `(functionp 'json-serialize)` return `t`

# download your config

rm -r ~/.emacs.d
git clone https://github.com/Lysander6/emacs-config.git ~/.emacs.d


# get language modules

cd ~

git clone https://github.com/casouri/tree-sitter-module.git
# commit 62f3703a327777baae2e96fbf574b9daf14bf575

cd tree-sitter-module
./batch.sh

# make folder for them (path from `C-h v` `treesit-extra-load-path`)
mkdir ~/.emacs.d/tree-sitter

cp dist/*.so ~/.emacs.d/tree-sitter/

# done!
```
