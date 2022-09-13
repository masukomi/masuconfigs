#!/usr/bin/env bash

# in order to regenerate the list of installed packages with
# descriptions run this

### via bash
# echo '#!/bin/sh' > brews.sh;
# brew leaves --installed-on-request \
#   | grep --color=none "/" \
#   | sed -e "s/\(.*\)\/.*/\1/" -e "s/^/brew tap /" \
#   | sort -u \
#   >> brews.sh;
# brew info $(brew leaves --installed-on-request) 2>/dev/null \
#   | grep --color=none -A2 "==> [^[:space:]]\+:" \
#   | grep -v -- "--" \
#   | sed -e 's/==> \(.*\):.*/maybrew \1/' \
#     -e '/^maybrew/!s/\(.*\)/# \1/' \
#   | ruby -e 'lines=STDIN.each_line.to_a; (2..(lines.size - 1)).step(3){|n| puts "\n#{lines[n - 1]}"; puts lines[n]; puts lines[n - 2]}' \
#   >> brews.sh
#
### via fish
# echo '#!/bin/sh' > brews.sh;
# brew leaves --installed-on-request \
#  | grep --color=none "/" \
#  | sed -e "s/\(.*\)\/.*/\1/" -e "s/^/brew tap /" \
#  | sort -u \
#   >> brews.sh;
# brew info (brew leaves --installed-on-request) 2>/dev/null  \
#  | grep --color=none -A2 "==> [^[:space:]]\+:" \
#  | grep -v -- "--" \
#  | sed -e 's/==> \(.*\):.*/maybrew \1/' \
#      -e '/^maybrew/!s/\(.*\)/# \1/' \
#  | ruby -e 'lines=STDIN.each_line.to_a; (2..(lines.size - 1)).step(3){|n| puts "\n#{lines[n - 1]}"; puts lines[n]; puts lines[n - 2]}' \
#  >> brews.sh
#



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

# contains gum
brew tap charmbracelet/tap
# contains days_progress, hey, oho, and private_comments
brew tap masukomi/homebrew-apps
# contains pgcli
brew tap dbcli/tap

brew tap d12frosted/emacs-plus
# tl;dr go client https://github.com/isacikgoz/tldr
brew tap isacikgoz/taps


# do gum first because this script uses it.
maybrew "charmbracelet/tap/gum"




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
	if ! is_installed "rbenv"; then
		gum confirm "Do you want me to install asdf?"
		if [ $? -eq 0 ]; then
			maybrew "asdf"
			asdf plugin add ruby https://github.com/asdf-vm/asdf-ruby.git
			asdf plugin add janet https://github.com/Jakski/asdf-janet.git
			# sadly, can't avoid needing node...
			asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
			gum confirm "Do you want me to install a Ruby?"
			if [ $? -eq 0 ]; then
				version=$(gum input --value="latest")
				asdf install ruby $version
				asdf global ruby $version
			fi
			gum confirm "Do you want me to install a Janet?"
			if [ $? -eq 0 ]; then
				version=$(gum input --value="latest")
				asdf install janet latest
				asdf global janet $version
			fi
		fi
	else
		echo "WARNING: Won't install asdf because rbenv is installed"
	fi
fi
maybrew "awscli"
# Official Amazon AWS command-line interface
# https://aws.amazon.com/cli/
maybrew "bash"
# Bourne-Again SHell, a UNIX command interpreter
# https://www.gnu.org/software/bash/
maybrew "bat"
# Clone of cat(1) with syntax highlighting and Git integration
# https://github.com/sharkdp/bat
maybrew "broot"
# New way to see and navigate directory trees
# https://dystroy.org/broot/
maybrew "charmbracelet/tap/gum"
# A tool for glamorous shell scripts
# https://charm.sh/
maybrew "chicken"
# Compiler for the Scheme programming language
# https://www.call-cc.org/
maybrew "cloc"
# Statistics utility to count lines of code
# https://github.com/AlDanial/cloc/
maybrew "cmake"
# Cross-platform make
# https://www.cmake.org/
maybrew "coreutils"
# GNU File, Shell, and Text utilities
# https://www.gnu.org/software/coreutils
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

maybrew "direnv"
# Load/unload environment variables based on $PWD
# https://direnv.net/
maybrew "dwdiff"
# Diff that operates at the word level
# https://os.ghalkes.nl/dwdiff.html
# maybrew "elixir"
# Functional metaprogramming aware language built on Erlang VM
# https://elixir-lang.org/
maybrew "enca"
# Charset analyzer and converter
# https://cihar.com/software/enca/
maybrew "exa"
# Modern replacement for 'ls'
# https://the.exa.website
maybrew "exercism"
# Command-line tool to interact with exercism.io
# https://exercism.io/cli/
maybrew "exif"
# Read, write, modify, and display EXIF data on the command-line
# https://libexif.github.io/
maybrew "fd"
# Simple, fast and user-friendly alternative to find
# https://github.com/sharkdp/fd
maybrew "fish"
# User-friendly command-line shell for UNIX-like operating systems
# https://fishshell.com

# maybrew "fontforge"
# Command-line outline and bitmap font editor/converter
# https://fontforge.github.io

# Command-line fuzzy finder written in Go
# https://github.com/junegunn/fzf
if ! is_installed "fzf"; then
    brew install fzf
    y | $(brew --prefix)/opt/fzf/install
else
	echo "-- Skipping fzf (installed already)"
fi


maybrew "gh"
# GitHub command-line tool
# https://github.com/cli/cli
maybrew "git"
# Distributed revision control system
# https://git-scm.com
maybrew "git-filter-repo"
# Quickly rewrite git repository history
# https://github.com/newren/git-filter-repo
maybrew "git-when-merged"
# Find where a commit was merged in git
# https://github.com/mhagger/git-when-merged
maybrew "glade"
# RAD tool for the GTK+ and GNOME environment
# https://glade.gnome.org/
maybrew "gnu-sed"
# GNU implementation of the famous stream editor
# https://www.gnu.org/software/sed/
maybrew "gnu-tar"
# GNU version of the tar archiving utility
# https://www.gnu.org/software/tar/
maybrew "go"
# Open source programming language to build simple/reliable/efficient software
# https://go.dev/
maybrew "gron"
# Make JSON greppable
# https://github.com/tomnomnom/gron

maybrew "masukomi/apps/hey"
# Hey allows you to track your interruptions as they occur.
# https://interrupttracker.com
# Improved top (interactive process viewer)
maybrew "htop"
# Improved top (interactive process viewer)
# https://htop.dev/
maybrew "icu4c"
# C/C++ and Java libraries for Unicode and globalization
# https://site.icu-project.org/home
maybrew "isacikgoz/taps/tldr"
# fast and interactive tldr client written with go
# https://github.com/isacikgoz/tldr
maybrew "ispell"
# International Ispell
# https://www.cs.hmc.edu/~geoff/ispell.html
# Add GitHub support to git on the command-line
# maybrew("hub

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


maybrew "jq"
# Lightweight and flexible command-line JSON processor
# https://stedolan.github.io/jq/
maybrew "jrnl"
# Command-line note taker
# http://jrnl.sh/en/stable/
maybrew "json-glib"
# Library for JSON, based on GLib
# https://wiki.gnome.org/Projects/JsonGlib
maybrew "lazygit"
# Simple terminal UI for git commands
# https://github.com/jesseduffield/lazygit/
maybrew "lefthook"
# Fast and powerful Git hooks manager for any type of projects
# https://github.com/evilmartians/lefthook
maybrew "libgit2"
# C library of Git core methods that is re-entrant and linkable
# https://libgit2.github.com/
maybrew "libgnt"
# NCurses toolkit for creating text-mode graphical user interfaces
# https://keep.imfreedom.org/libgnt/libgnt
maybrew "libotr"
# Off-The-Record (OTR) messaging library
# https://otr.cypherpunks.ca/
maybrew "libxslt"
# C XSLT library for GNOME
# http://xmlsoft.org/XSLT/
maybrew "llvm@11"
# Next-gen compiler infrastructure
# https://llvm.org/
maybrew "lynx"
# Text-based web browser
# https://invisible-island.net/lynx/
maybrew "make"
# Utility for directing compilation
# https://www.gnu.org/software/make/
maybrew "masukomi/apps/days_progress"
# A simple command line chart of your progress through the day
# https://github.com/masukomi/days_progress
maybrew "masukomi/apps/oho"
# Takes your colorful terminal output and converts it to HTML for sharing
# https://github.com/masukomi/oho
maybrew "masukomi/apps/private_comments"
# A REST server to manage private comments on your code.
# https://github.com/masukomi/private_comments
# Like sed, awk, cut, join & sort for name-indexed data such as CSV
maybrew "miller"
# Like sed, awk, cut, join & sort for name-indexed data such as CSV
# https://github.com/johnkerl/miller
maybrew "moreutils"
# Collection of tools that nobody wrote when UNIX was young
# https://joeyh.name/code/moreutils/
maybrew "mpack"
# MIME mail packing and unpacking
# https://web.archive.org/web/20190220145801/ftp.andrew.cmu.edu/pub/mpack/
maybrew "msmtp"
# SMTP client that can be used as an SMTP plugin for Mutt
# https://marlam.de/msmtp/
# maybrew "neomutt"
# E-mail reader with support for Notmuch, NNTP and much more
# https://neomutt.org/
# maybrew "nim"
# Statically typed compiled systems programming language
# https://nim-lang.org/
maybrew "node"
# Platform built on V8 to build network applications
# https://nodejs.org/
maybrew jandedobbeleer/oh-my-posh/oh-my-posh
# Prompt theme engine for any shell
# https://ohmyposh.dev
maybrew "ossp-uuid"
# ISO-C API and CLI for generating UUIDs
# https://web.archive.org/web/www.ossp.org/pkg/lib/uuid/
maybrew "pandoc"
# Swiss-army knife of markup format conversion
# https://pandoc.org/
maybrew "pgcli"
# CLI for Postgres with auto-completion and syntax highlighting
# https://pgcli.com/
maybrew "pinentry-mac"
# Pinentry for GPG on Mac
# https://github.com/GPGTools/pinentry
maybrew "plantuml"
# Draw UML diagrams
# https://plantuml.com/
if ! is_installed "postgresql"; then
# Object-relational database system
# https://www.postgresql.org/
	brew install postgresql
	brew services start postgresql # start it
	psql postgres -c "create database $USER;"
else
	echo "-- Skipping postgresql (installed already)"
fi
maybrew "python-tabulate"
# Pretty-print tabular data in Python
# https://pypi.org/project/tabulate/
# custom shell prompt generator thing

maybrew "python" # python 3 yo!
if ! is_installed "rakudo-star"; then
	brew install rakudo-star
	zef install fez
fi

if ! is_installed "rbenv"; then
	if ! is_installed "asdf"; then
		gum confirm "Do you want me to install rbenv?"
		if [ $? -eq 0 ]; then
			maybrew "rbenv"
			maybrew "ruby-build" # dunno why this is a thing, but it is
			gum confirm "Do you want me to install a ruby?"
			if [ $? -eq 0 ]; then
				latest=$( rbenv install -l 2>/dev/null | grep "^[[:digit:]]" | tail -n1)
				version=$(gum input --value="$latest")
				# there's an issue with open-ssl-1.1.1q
				# which means you need to set this CFLAGS option
				# I don't know which versions use a different version
				# so i'm just assuming it's required for all for now
				CFLAGS="-Wno-error=implicit-function-declaration" rbenv install $version
			fi
		fi
	else
		echo "WARNING: Won't install rbenv because asdf is installed"
	fi
fi

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

maybrew "ripgrep"
# Search tool like grep and The Silver Searcher
# https://github.com/BurntSushi/ripgrep
# Installed via bootstrap script now instead
# maybrew "rust"
# Readline wrapper: adds readline support to tools that lack it
maybrew "rlwrap"

maybrew "sd"
# Intuitive find & replace CLI
# https://github.com/chmln/sd
maybrew "shellcheck"
# Static analysis and lint tool, for (ba)sh scripts
# https://www.shellcheck.net/
maybrew "subversion"
# Version control system designed to be a better CVS
# https://subversion.apache.org/
maybrew "task"
# Feature-rich console based todo list manager
# https://taskwarrior.org/
maybrew "td"
# Your todo list in your terminal
# https://github.com/Swatto/td
maybrew "terminal-notifier"
# Send macOS User Notifications from the command-line
# https://github.com/julienXX/terminal-notifier
maybrew "texinfo"
# Official documentation format of the GNU project
# https://www.gnu.org/software/texinfo/
# maybrew "the_silver_searcher"
# Code-search similar to ack
# https://github.com/ggreer/the_silver_searcher
maybrew "tig"
# Text interface for Git repositories
# https://jonas.github.io/tig/
maybrew "toot"
# Mastodon CLI & TUI
# https://toot.readthedocs.io/en/latest/index.html
maybrew "translate-shell"
# Command-line translator using Google Translate and more
# https://www.soimort.org/translate-shell
maybrew "tree"
# Display directories as trees (with optional color/HTML output)
# http://mama.indstate.edu/users/ice/tree/
maybrew "universal-ctags/universal-ctags/universal-ctags"
# Maintained ctags implementation
# https://github.com/universal-ctags/ctags
maybrew "uudeview"
# Smart multi-file multi-part decoder
# http://www.fpx.de/fp/Software/UUDeview/
maybrew "w3m"
# Pager/text based browser
# https://w3m.sourceforge.io/
maybrew "wget"
# Internet file retriever
# https://www.gnu.org/software/wget/
maybrew "yadm"
# Yet Another Dotfiles Manager
# https://yadm.io/
maybrew "yj"
# CLI to convert between YAML, TOML, JSON and HCL
# https://github.com/sclevine/yj
maybrew "xz"
# General-purpose data compression with high compression ratio

# Programatically correct mistyped console commands
# maybrew "thefuck"
# Program that can automate interactive applications
if ! is_installed "gcalcli"; then
	pip install gcalcli
else
	echo "-- Skipping gcalcli (installed already)"
fi


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


