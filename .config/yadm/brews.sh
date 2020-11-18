#!/bin/sh
# maybrew == maybe brew (install)
# $1 must be package to install
#
function maybrew() {
	if ! is_installed $1; then
		echo ""
		echo ""
		echo "-- Installing $1"
		brew install $1
	else
		echo "-- Skipping $1 (installed already)"
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

# Yet another dotfiles manager
# maybrew "yadm"
# Search tool like grep, but optimized for programmers
maybrew "ack"
# Automatic configure script builder
maybrew "autoconf"
# Tool for generating GNU Standards-compliant Makefiles
maybrew "automake"
# Statistics utility to count lines of code
maybrew "bat"
# Clone of cat(1) with syntax highlighting and Git integration
maybrew "cloc"
# Tool for browsing source code
maybrew "cscope"
# universal ctags
if ! is_installed "universal-ctags/universal-ctags/universal-ctags"; then
	brew tap universal-ctags/universal-ctags
	brew install --HEAD universal-ctags/universal-ctags/universal-ctags
else
	echo "-- Skipping universal-ctags (installed already)"
fi

# contains days_progress, hey, oho, and private_comments
brew tap masukomi/homebrew-apps

# cli utility that shows your progress through the day
maybrew "days_progress"
# Load/unload environment variables based on $PWD
maybrew "direnv"
# Diff that operates at the word level
maybrew "dwdiff"
# Charset analyzer and converter
maybrew "enca"
# User-friendly command-line shell for UNIX-like operating systems
maybrew "exa"
# Modern replacement for 'ls'
maybrew "fish"
# Simple, fast and user-friendly alternative to find
maybrew "fd"
# Command-line outline and bitmap font editor/converter
maybrew "fontforge"

# A command-line fuzzy finder
maybrew "fzf"

# GNU internationalization (i18n) and localization (l10n) library
maybrew "gettext"
# Interpreter for PostScript and PDF
maybrew "ghostscript"
# Distributed revision control system
maybrew "git"
# Core application library for C
maybrew "glib"

# Interruption Tracker  https://interruptiontracker.com
maybrew "hey"

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
# GUI for vim, made for macOS
# maybrew "macvim"
# Scalable distributed version control system
# maybrew("mercurial
# High-performance, schema-free, document-oriented database
maybrew "mongodb"
# Shell command parallelization utility
# a version is included in moreutils
# but for reasons I forget we'd rather have this one.
#maybrew "parallel"

# allows you to comment on a file without commenting _in_ the file
maybrew "private_comments"

# Collection of tools that nobody wrote when UNIX was young
# used to install with --without-parallel
# but that's no longer an option
maybrew "moreutils"

# MIME mail packing and unpacking
maybrew "mpack"
# ANSI->HTML
maybrew "oho"
# SSL/TLS cryptography library
maybrew "openssl"
# ISO-C API and CLI for generating UUIDs
maybrew "ossp-uuid"
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
maybrew "rust"
# Command-line interface for SQLite
maybrew "sqlite"
# File system client based on SSH File Transfer Protocol
#maybrew "sshfs"
# Display directories as trees (with optional color/HTML output)
maybrew "tree"
# Smart multi-file multi-part decoder
maybrew "uudeview"
# General-purpose data compression with high compression ratio
maybrew "xz"
# GNU Pretty Good Privacy (PGP) package
maybrew "gpg"
# Text interface for Git repositories
maybrew "tig"
# CLI for Postgres with auto-completion and syntax highlighting
maybrew "pgcli"
# Programatically correct mistyped console commands
# maybrew "thefuck"
# Command-line note taker
maybrew "jrnl"
# Cross-platform make
maybrew "cmake" # used by yajl
# Lightweight and flexible command-line JSON processor
maybrew "jq" # json parser
# Program that can automate interactive applications
if ! is_installed "gcalcli"; then
	pip install gcalcli
else
	echo "-- Skipping gcalcli (installed already)"
fi


# maybrew "homebrew/dupes/expect" # gets you unbuffer
# Readline wrapper: adds readline support to tools that lack it
maybrew "rlwrap"
# Feature-rich console based todo list manager
maybrew "task" # task warrior
# Official Amazon AWS command-line interface
maybrew "awscli"
# Platform built on V8 to build network applications
maybrew "npm"
# Find where a commit was merged in git
maybrew "git-when-merged"
# Graph visualization software from AT&T and Bell Labs
maybrew "graphviz"
# Improved top (interactive process viewer)
maybrew "htop"
# Send macOS User Notifications from the command-line
maybrew "terminal-notifier"
# Make JSON greppable
maybrew "gron"
# Like sed, awk, cut, join & sort for name-indexed data such as CSV
maybrew "miller"

maybrew "pandoc" # neomutt is using it if nothing else...
# like ack but faster
maybrew "the_silver_searcher"
# Simple GTD-style task management for the command-line
maybrew "ultralist"
# emacs
# https://github.com/syl20bnr/spacemacs
brew tap d12frosted/emacs-plus
if ! is_installed "emacs-plus"; then
  brew install emacs-plus #--with-spacemacs-icon
  EMACS_VERSION=$(ls /usr/local/Cellar/ \
        | grep --color=never emacs-plus \
        | sort \
        | tail -n1 \
        | sed -e "s/@/\\\@/g" )
  EMACS_SUB_VERSION=$( ls /usr/local/Cellar/$VERSION \
        | sort \
        | tail -n1)
  ln -s /usr/local/Cellar/$VERSION/$EMACS_SUB_VERSION/Emacs.app /Applications/

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
# Text-based UI library
maybrew "ncurses"
# SMTP client that can be used as an SMTP plugin for Mutt
maybrew "msmtp"
# Library access to GnuPG
maybrew "gpgme"
#
#maybrew "khard"
# Kubernetes (k8s) command line utility
maybrew "kubectl"
# SMTP client that can be used as an SMTP plugin for Mutt
maybrew "msmtp"
#
#maybrew "mu"
# E-mail reader with support for Notmuch, NNTP and much more
maybrew "neomutt"
# Synchronize a maildir with an IMAP server
# maybrew "isync"
# brew services start isync
# Extract attachments out of MIME encoded email packages
#maybrew "ripmime"
# Pager/text based browser
maybrew "w3m"
#International Ispell
maybrew "ispell" #something in/via neomutt wants this
# it's like cat, but for images
# except a better/different one comes with iterm2
#brew tap eddieantonio/eddieantonio
#maybrew "imgcat"
# A command line interface to the macOS Address Book.
if ! is_installed "https://raw.github.com/tgray/homebrew-tgbrew/master/contacts2.rb"; then
	brew install https://raw.github.com/tgray/homebrew-tgbrew/master/contacts2.rb
else
	echo "-- Skipping contacts2.rb (installed already)"
fi
# Text-based UI library
maybrew "ncurses"

# Selection-based modal text editor
maybrew "kakoune"
# ANSI HTML adapter
# maybrew "aha"
# aha is shit! use oho! https://github.com/masukomi/oho/
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

# Compiler for the Scheme programming language
maybrew "chicken"
