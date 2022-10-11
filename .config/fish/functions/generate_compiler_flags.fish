function generate_compiler_flags
# SET UP LDFLAGS, CPPFLAGS, PKG_CONFIG_PATH
# for readline and openssl

echo "Consulting brew info for things..."
# readline
echo "readline..."
brew info readline | grep "set -gx " | sed -e "s/^ *//" -e 's/$/;/' \
	> ~/.config/fish/compiler_flags.fish
# opensssl
# also adds openssl@3/bin to the path
echo "openssl..."
brew info openssl | egrep "set -gx |fish_add_path" | sed -e "s/^ *//" -e 's/^\(.*\) \([^[:space:]]*\) "/\1 \2 "$\2 /' -e 's/$/;/' \
	>> ~/.config/fish/compiler_flags.fish

echo "openssl3..."
brew info openssl@3 | egrep "set -gx |fish_add_path" | sed -e "s/^ *//" -e 's/^\(.*\) \([^[:space:]]*\) "/\1 \2 "$\2 /' -e 's/$/;/' \
	>> ~/.config/fish/compiler_flags.fish

# and llvm
echo "llvm..."
brew info llvm | egrep "set -gx |fish_add_path" | sed -e "s/^ *//" -e 's/^\(.*\) \([^[:space:]]*\) "/\1 \2 "$\2 /' -e 's/$/;/' \
	>> ~/.config/fish/compiler_flags.fish
# and libxslt
echo "libxslt..."
brew info libxslt | egrep "set -gx |fish_add_path" | sed -e "s/^ *//" -e 's/^\(.*\) \([^[:space:]]*\) "/\1 \2 "$\2 /' -e 's/$/;/' \
	>> ~/.config/fish/compiler_flags.fish

echo "DONE"
end

