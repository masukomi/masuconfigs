function listening \
  --argument-names 'port' 'host' \
  --description "tests if something is listening on the supplied port"

  if test "$host" = ""
    set host "localhost"
  end
  nc -zvv -G 5 $host $port
end

