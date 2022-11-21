function tooloo-ingester
  for file in (find . -name "*.meta.toml")
      echo "include $file ?"
      read -P "[n/q/enter] " yn
      if test "$yn" = ""
          tooloo add $file
      else if test "$yn" = "q"
          echo "quitting"
          break
      else
          echo "skipping..."
      end
  end
end

