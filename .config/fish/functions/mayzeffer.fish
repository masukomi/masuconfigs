function mayzeffer --argument-names package
  zef info $package | egrep "Description|Source-url" | sed -e "s/^/# /g"
  echo mayzef $package
end

