###############################################################################
# Usage: yaml_get_vname <file_path> [<level:0>] [<prefix>]
# Summary: Gets list of names of parent variables of given <level> in YAML-file.
# Help:
#   Params:
#     <file_path> Path to YAML-file.
#     [<level:|0|1|2...N>] Level of variable. Starts from 0.
#     [<prefix>] Prefix which will be added to variable names.
#
# author: Vlad Savitsky
# https://github.com/VladSavitsky
function yaml_get_vname() {
  local filepath=$1;
  local level=${2:-0};
  local prefix=$3

  local s='[[:space:]]*';
  local w='[a-zA-Z0-9_]*';
  local fs=$(echo @|tr @ '\034');
  sed -ne "s|^\($s\):|\1|" \
    -e "s|^\($s\)\($w\)$s:$s[\"']\(.*\)[\"']$s\$|\1$fs\2$fs\3|p" \
    -e "s|^\($s\)\($w\)$s:$s\(.*\)$s\$|\1$fs\2$fs\3|p"  $filepath |
  awk -F$fs '{
    indent = length($1)/2;
    if (indent == '$level' && length($3) == 0) {
      printf("%s%s\n", "'$prefix'", $2);
    }
  }'
}
