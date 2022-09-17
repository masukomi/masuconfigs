function doom_sync
  set -l current_dir (pwd)
  cd ~/.emacs.d
  bin/doom sync
  cd $current_dir
end
