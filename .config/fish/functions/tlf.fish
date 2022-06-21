# Runs rtest or rspec for the last file you found with fbn
# rtest: https://github.com/masukomi/rtest#readme
# fbn: https://github.com/masukomi/masuconfigs/blob/master/.config/fish/functions/fbn.fish

function tlf --description "Test Last Find"
	set -l path (eval \
					(history \
					| grep "fbn " \
					| grep -v "grep" \
					| head -n1) \
				| head -n1)
	argparse 'r/rspec' -- $argv
	if set -q _flag_r || set -q _flag_rspec
		bundle exec rspec --format=documentation $path
	else
		rtest $path
	end
end

