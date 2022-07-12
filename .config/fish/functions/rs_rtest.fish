function rs_rtest --description "reruns the last rtest command in rspec"

	set -l rspectified (history \
		| grep "^rtest" \
		| grep -v "\--rspec" \
		| head -n1 \
		| sed -e "s/^rtest /rtest --rspec /")

	if test "$rspectified"  != ""
		set -l rspec_string (eval $rspectified)
		set -l maybe_bundle (string sub --length 6 "$rspec_string")
		if test "$maybe_bundle" = "bundle"
			eval $rspec_string
		else
			# output the error message
			echo $rspec_string
			return 1
		end
	else
		echo "unable to find valid rtest command to rerun in rspec"
		return 1
	end
end
alias rsrt=rs_rtest

function rtrs --description "reruns the last rspec command in rtest"
	set -l rtestified (history \
		| grep "bundle exec rspec" \
		| sed -e "s/^.*exec rspec//" \
		  -e "s/--format=[^[:space:]]*[[:space:]]//" \
		| head -n1 \
		| sed -e "s/^/rtest/")

	if test "$rtestified" != ""
		eval $rtestified
	else
		echo "unable to find valid rtest command to rerun in rtest"
		return 1
	end

end
