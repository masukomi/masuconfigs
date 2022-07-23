#!/bin/sh

# in order to regenerate the list of installed packages with
# descriptions run this
# brew leaves | xargs -n1 brew desc
# brew leaves --installed-on-request | xargs -n1 brew desc


# maybrew == maybe brew (install)
# $1 must be package to install
#
function maybrew() {
	if ! is_installed $1; then
		install_or_die "$1"
	else
		echo "-- Skipping $1 (installed already)"
	fi
}

function install_or_die() {
	brew install $1
	echo ""
	echo ""
	echo "-- Installing $1"
	brew install $1
	if [ $? -ne 0 ]; then
		echo "problem installing $1"
		echo "exiting"
		exit 1
	fi
}

function is_installed() {
	if brew ls --versions $1 > /dev/null; then
		# the package is installed
		true
	else
		# the package is not installed
		false
	fi
}

# install homebrew if we don't have it already
if ! command -v brew >/dev/null 2>&1; then
	echo "Installing homebrew"
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# contains days_progress, hey, oho, and private_comments
brew tap masukomi/homebrew-apps
# contains pgcli
brew tap dbcli/tap

# Strip or convert ANSI codes into HTML, (La)Tex, RTF, or BBCode
# http://www.andre-simon.de/doku/ansifilter/en/ansifilter.php
maybrew "ansifilter"
# Yet another dotfiles manager
# maybrew "yadm"
# Search tool like grep, but optimized for programmers
# have switched to ripgrep (installed below)
#maybrew "ack"
# https://github.com/asdf-vm/asdf
# Manage multiple runtime versions with a single CLI tool, extendable via plugins
if ! is_installed "asdf"; then
	maybrew "asdf"
	asdf plugin add ruby https://github.com/asdf-vm/asdf-ruby.git
	# sadly, can't avoid needing node...
	asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
fi
# Automatic configure script builder
maybrew "autoconf"
# Tool for generating GNU Standards-compliant Makefiles
maybrew "automake"
# Official Amazon AWS command-line interface
maybrew "awscli"
# because apple has issues with open source licenses
maybrew "bash"
# bat: Clone of cat(1) with syntax highlighting and Git integration
maybrew "bat"
# Compiler for the Scheme programming language
maybrew "chicken"
# Statistics utility to count lines of code
maybrew "cloc"
# Cross-platform make
maybrew "cmake" # used by yajl
# GNU core utilities. These will be prefixed with "g" (ls -> gls)
maybrew "coreutils"
# Clone of cat(1) with syntax highlighting and Git integration
# Tool for browsing source code
maybrew "cscope"
# universal ctags
if ! is_installed "universal-ctags/universal-ctags/universal-ctags"; then
	brew tap universal-ctags/universal-ctags
	brew install --HEAD universal-ctags/universal-ctags/universal-ctags
	if [ $? -ne 0 ]; then
		echo "problem installing universal-ctags. exiting"
		exit 1
	fi
else
	echo "-- Skipping universal-ctags (installed already)"
fi


# cli utility that shows your progress through the day
maybrew "days_progress"
ls /Applications/DevUtils.app > /dev/null
# normal is_installed doesn't work for devutils For some reason
# it just doesn't show up in brew ls --versions
if [ $? -ne 0 ]; then
	brew install devutils
	# opening it just to get past the "are you sure" dialog
	open /Applications/DevUtils.app
else
	echo "-- Skipping devutils (installed already)"
fi

# Load/unload environment variables based on $PWD
maybrew "direnv"
# Diff that operates at the word level
maybrew "dwdiff"
# Elixir programming language
maybrew "elixir"
# Charset analyzer and converter
maybrew "enca"
# User-friendly command-line shell for UNIX-like operating systems
maybrew "exa"
# Simple, fast and user-friendly alternative to find
maybrew "fd"
# fish: User-friendly command-line shell for UNIX-like operating systems
maybrew "fish"
# Command-line outline and bitmap font editor/converter
maybrew "fontforge"

# A command-line fuzzy finder
if ! is_installed "fzf"; then
    brew install fzf
    y | $(brew --prefix)/opt/fzf/install
else
	echo "-- Skipping fzf (installed already)"
fi


# GNU internationalization (i18n) and localization (l10n) library
maybrew "gettext"
# GitHub command-line tool
maybrew "gh"
# Interpreter for PostScript and PDF
maybrew "ghostscript"
# Distributed revision control system
maybrew "git"
# mass rewrite commit history
maybrew "git-filter-repo"
# Find where a commit was merged in git
maybrew "git-when-merged"
# Core application library for C
maybrew "glib"
# GNU Pretty Good Privacy (PGP) package
maybrew "gpg"
# Library access to GnuPG
maybrew "gpgme"
# Graph visualization software from AT&T and Bell Labs
maybrew "graphviz"
# Make JSON greppable
maybrew "gron"
# Interruption Tracker  https://interruptiontracker.com
maybrew "hey"
# Improved top (interactive process viewer)
maybrew "htop"

# Add GitHub support to git on the command-line
# maybrew("hub
# C/C++ and Java libraries for Unicode and globalization
maybrew "icu4c"
# Tools and libraries to manipulate images in many formats
maybrew "imagemagick"
# imagemagick@6 is keg-only, which means it was not symlinked into /usr/local,
# because this is an alternate version of another formula.
#
# If you need to have imagemagick@6 first in your PATH run:
#   echo 'set -g fish_user_paths "/usr/local/opt/imagemagick@6/bin" $fish_user_paths' >> ~/.config/fish/config.fish
#
# For compilers to find imagemagick@6 you may need to set:
#   set -gx LDFLAGS "-L/usr/local/opt/imagemagick@6/lib"
#   set -gx CPPFLAGS "-I/usr/local/opt/imagemagick@6/include"
#
# For pkg-config to find imagemagick@6 you may need to set:
#   set -gx PKG_CONFIG_PATH "/usr/local/opt/imagemagick@6/lib/pkgconfig"


# JBIG2 decoder and library (for monochrome documents)
maybrew "jbig2dec"
# Image manipulation library
maybrew "jpeg"
# Lightweight and flexible command-line JSON processor
maybrew "jq" # json parser
# Command-line note taker
maybrew "jrnl"
# C library of Git core methods that is re-entrant and linkable
maybrew "libgit2"
# Simple terminal UI for git commands
maybrew "lazygit"
# Portable Foreign Function Interface library
maybrew "libffi"
# Common error values for all GnuPG components
maybrew "libgpg-error"
# X.509 and CMS library - for GnuPG
maybrew "libksba"
# TIFF library and utilities
maybrew "libtiff"
# Generic library support script
maybrew "libtool"
# GNOME XML library
maybrew "libxml2"
# C XSLT library for GNOME
maybrew "libxslt"
# YAML Parser
maybrew "libyaml"
# Color management engine supporting ICC profiles
maybrew "little-cms2"
# Like sed, awk, cut, join & sort for name-indexed data such as CSV
maybrew "miller"
# pgcli: CLI for Postgres with auto-completion and syntax highlighting
maybrew "pgcli"

# Shell command parallelization utility
# a version is included in moreutils
# but for reasons I forget we'd rather have this one.
#maybrew "parallel"

# plantuml: Draw UML diagrams (and more)
# https://plantuml.com
maybrew "plantuml"


# Collection of tools that nobody wrote when UNIX was young
# used to install with --without-parallel
# but that's no longer an option
maybrew "moreutils"

# MIME mail packing and unpacking
maybrew "mpack"
# SMTP client that can be used as an SMTP plugin for Mutt
maybrew "msmtp"
# Text-based UI library
maybrew "ncurses"
# E-mail reader with support for Notmuch, NNTP and much more
maybrew "neomutt"
# Platform built on V8 to build network applications
maybrew "npm"
# custom shell prompt generator thing
# https://ohmyposh.dev/
if ! is_installed "oh-my-posh"; then
	brew install jandedobbeleer/oh-my-posh/oh-my-posh
fi
# ANSI->HTML
maybrew "oho"
# SSL/TLS cryptography library
maybrew "openssl"
# ISO-C API and CLI for generating UUIDs
maybrew "ossp-uuid"
# store secrets in your macOS keychain
# useful for not making gpg ask you for your password constantly
maybrew "pinentry-mac"
# Manage compile and link flags for libraries
maybrew "pkg-config"
# Object-relational database system
if ! is_installed "postgresql"; then
	brew install postgresql
	brew services start postgresql # start it
else
	echo "-- Skipping postgresql (installed already)"
fi
# psql postgres -c "create database $USER;"

# allows you to comment on a file without commenting _in_ the file
maybrew "private_comments"
maybrew "python" # python 3 yo!
# Library for command-line editing
maybrew "readline"
#	set -gx LDFLAGS "-L/usr/local/opt/readline/lib"
#	set -gx CPPFLAGS "-I/usr/local/opt/readline/include"
#	set -gx PKG_CONFIG_PATH "/usr/local/opt/readline/lib/pkgconfig"

# readline is keg-only, which means it was not symlinked into /usr/local,
# because macOS provides the BSD libedit library, which shadows libreadline.
# In order to prevent conflicts when programs look for libreadline we are
# defaulting this GNU Readline installation to keg-only.
#
# For compilers to find readline you may need to set:
#   set -gx LDFLAGS "-L/usr/local/opt/readline/lib"
#   set -gx CPPFLAGS "-I/usr/local/opt/readline/include"
#
# For pkg-config to find readline you may need to set:
#   set -gx PKG_CONFIG_PATH "/usr/local/opt/readline/lib/pkgconfig"
# ripgrep like ack but better
maybrew "ripgrep"
# the rust compiler & language
# Installed via bootstrap script now instead
# maybrew "rust"
# Readline wrapper: adds readline support to tools that lack it
maybrew "rlwrap"

# Intuitive find & replace CLI
maybrew "sd"
# Static analysis and lint tool, for (ba)sh scripts
maybrew "shellcheck"

# Command-line interface for SQLite
maybrew "sqlite"
# File system client based on SSH File Transfer Protocol
#maybrew "sshfs"
# Feature-rich console based todo list manager
maybrew "task" # task warrior
# Send macOS User Notifications from the command-line
maybrew "terminal-notifier"
# Smart multi-file multi-part decoder
# like ack but faster
maybrew "the_silver_searcher"
# Text interface for Git repositories
maybrew "tig"
# Command-line translator using Google Translate and more
maybrew "translate-shell"
# Display directories as trees (with optional color/HTML output)
maybrew "tree"
# Simple GTD-style task management for the command-line
maybrew "ultralist"
# uudeview: Smart multi-file multi-part decoder
maybrew "uudeview"
# Pager/text based browser
maybrew "w3m"
# General-purpose data compression with high compression ratio
maybrew "xz"
# Programatically correct mistyped console commands
# maybrew "thefuck"
# Program that can automate interactive applications
if ! is_installed "gcalcli"; then
	pip install gcalcli
else
	echo "-- Skipping gcalcli (installed already)"
fi


# maybrew "homebrew/dupes/expect" # gets you unbuffer

# pandoc: Swiss-army knife of markup format conversion
maybrew "pandoc" # neomutt is using it if nothing else...
# CLI for Postgres with auto-completion and syntax highlighting
maybrew "pgcli"
# emacs
# https://github.com/syl20bnr/spacemacs
brew tap d12frosted/emacs-plus
if ! is_installed "emacs-plus"; then
	brew unlink gawk 2>&1 /dev/null
	# it complaind about this vs awk
	# so now we unlink gawk and relink later
	install_or_die "emacs-plus"
	# EMACS_VERSION=$(ls /usr/local/Cellar/ \
	# 	| grep --color=never emacs-plus \
	# 	| sort \
	# 	| tail -n1 \
	# 	| sed -e "s/@/\\\@/g" )
    #
	# EMACS_SUB_VERSION=$( ls /usr/local/Cellar/$EMACS_VERSION \
	# 	| sort \
	# 	| tail -n1)
	# ln -s /usr/local/Cellar/$VERSION/$EMACS_SUB_VERSION/Emacs.app /Applications/
	eval "$(brew info emacs-plus | grep 'ln -s' | sed -e 's/^ //')"
	eval "$( brew info emacs-plus | grep 'services start' | sed -e 's/^ //')"
	brew link gawk
# 	--natural-title-bar option was removed from this formula, in order to
# 	  duplicate its effect add following line to your init.el file
# 	  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
# 	  (add-to-list 'default-frame-alist '(ns-appearance . dark))
# 	or:
# 	  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
# 	  (add-to-list 'default-frame-alist '(ns-appearance . light))
#   If you are using macOS Mojave, please note that most of the experimental
#   options are forbidden on Mojave. This is temporary decision.
# 	To have launchd start d12frosted/emacs-plus/emacs-plus now and restart at login:
# 	  brew services start d12frosted/emacs-plus/emacs-plus
# 	Or, if you don't want/need a background service you can just run:
# 	  emacs
else
	echo "-- Skipping emacs-plus (installed already)"
fi

# ispell: International Ispell
maybrew "ispell" #something in/via neomutt wants this
# A command line interface to the macOS Address Book.
# DEPRECATED INSTALL COMMAND. NO LONGER WORKS WITH ARBITRARY URL
# Error: Non-checksummed download of contacts2 formula file from an arbitrary URL is unsupported!
#  `brew extract` or `brew create` and `brew tap-new` to create a formula file in a tap on GitHub instead.
# if ! is_installed "https://raw.github.com/tgray/homebrew-tgbrew/master/contacts2.rb"; then
# 	brew install https://raw.github.com/tgray/homebrew-tgbrew/master/contacts2.rb
# else
# 	echo "-- Skipping contacts2.rb (installed already)"
# fi
