# some helper function to go back and forth between rtest and rspec
# see https://github.com/masukomi/rtest

function rtrs --description "rtestify (rt) last rspec (rs) command"
	set -l command (
		history \
		| grep "^bundle exec rspec" \
		| grep -v "rtest" \
		| head -n 1 \
		| sd "^.*rspec (-+\w+(=| )[0-9a-zA-Z-]+)* *" "rtest "
		)

	echo "$command"
	eval $command
end

function rsrt --description "rspecify (rs) last rtest (rt) command"
	set -l command (
		history \
		| grep "rtest " \
		| grep -v "\--rspec" \
		| head -n1\
		| sd "rtest" "rtest --rspec" )
	set -l c2 (eval $command)
	echo "$c2"
	eval $c2
end

