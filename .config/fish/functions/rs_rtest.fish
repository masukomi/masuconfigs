function rs_rtest --description "reruns the last rtest command in rspec"

set -l rspectified (history \
	| grep "^rtest" \
	| grep -v "\--rspec" \
	| head -n1 \
	| sed -e "s/^rtest /rtest --rspec /")

	if test "$rspectified"  != ""
		set -l rspec_string (eval $rspectified)
		set -l maybe_bundle (string sub --length 5 "$rspec_string")
		if test "$maybe_bundle" = "bundle"
			echo "status == 0: $rspec_string"
			eval $rspec_string
		else
			echo $rspec_string
			return 1
		end
	else
		echo "unable to find valid rtest command to rerun in rspec"
		return 1
	end
end
