#! /usr/bin/env bash

FILE_TAGS_NAME='ftags'

echo '!_TAG_FILE_FORMAT	2	/extended format; --format=1 will not append ;" to lines/' > $FILE_TAGS_NAME
echo '!_TAG_FILE_SORTED	1	/0=unsorted, 1=sorted, 2=foldcase/' >> $FILE_TAGS_NAME
echo '!_TAG_PROGRAM_AUTHOR	Darren Hiebert  /dhiebert@users.sourceforge.net/' >> $FILE_TAGS_NAME
echo '!_TAG_PROGRAM_NAME	Exuberant Ctags //' >> $FILE_TAGS_NAME
echo '!_TAG_PROGRAM_URL	http://ctags.sourceforge.net	/official site/' >> $FILE_TAGS_NAME
echo '!_TAG_PROGRAM_VERSION	5.7	//' >> $FILE_TAGS_NAME

find $* \
    \( ! -regex '.*/\..*/..*' \) \
    \( \
        -name '.DS_Store' -or \
        -name '.tmp_*~' -or \
        -name '*.pdf' -or \
        -name '*.jpg' -or \
        -name '*.png' -or \
        -name '*.swf' -or \
        -name '*.gif' -or \
        -name '*.ico' -or \
        -name '*.swp' -or \
        -name '*.swd' -or \
        -name '.gitignore' -or \
        -name '*.pyc' -or \
        -name '*.swo' \
    \) -prune -or \
    \( \
         -path '*/.svn/*' -or \
         -path '*/migrate/*' -or \
         -path '*/tmp/*' -or \
         -path '*/performance/*' -or \
         -path '*/tax_chart/*' -or \
         -path '*/coverage/*' -or \
         -path '*/doc/*' -or \
         -path '*/.cache/*' \
     \) -prune -or \
     -type f -print | \
ruby -nle 'puts %Q{#{File.basename($_)}\t#{$_}\t#{%q{/^/;"}}\tr}' | \
sort >> $FILE_TAGS_NAME

exit 0


###################################################################
### below codes have some issues to debug
###################################################################

IGNORE_EXT_LIST=".jpg .png .swf .gif .ico .swp .swo"
IGNORE_PATH_LIST=".svn migrate"

FIND_CMD="find $* "

FIRST_TIME='TRUE'
FIND_CMD="$FIND_CMD "'\( '
for ext in $IGNORE_EXT_LIST ; do 
  if [ $FIRST_TIME == 'TRUE' ]; then 
    FIRST_TIME='FALSE'
  else
    FIND_CMD="$FIND_CMD -or"
  fi
  FIND_CMD="$FIND_CMD -name \"*$ext\" "
done
FIND_CMD="$FIND_CMD "'\) -prune '

FIRST_TIME='TRUE'
FIND_CMD=" $FIND_CMD -or "'\( '
for path in $IGNORE_PATH_LIST ; do 
  if [ $FIRST_TIME == 'TRUE' ]; then 
    FIRST_TIME='FALSE'
  else
    FIND_CMD="$FIND_CMD -or"
  fi
  FIND_CMD="$FIND_CMD -path \"*/$path/\" "
done
FIND_CMD="$FIND_CMD "'\) -prune '

FIND_CMD="$FIND_CMD -or -type f -print "

echo $FIND_CMD

$FIND_CMD | ruby -nle 'result = []; result << File.basename($_); result << $_; result << %q{/^/;"} << "c"; puts result.join("\t")' | sort 

