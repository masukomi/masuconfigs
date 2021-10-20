function rollback --argument-names 'steps'
  if test -n "$steps"
    bundle exec rake db:rollback STEP=$steps
  else
    bundle exec rake db:rollback STEP=1
  end
end

