function got_branch --description="tests if a branch with the given name exists"
	set -l BRANCH (git fetch -q upstream; and git branch -a | grep $argv)
	if [ "$BRANCH" = "" ]
		echo "nope"
	else
		echo "yup: $BRANCH"
	end
end

