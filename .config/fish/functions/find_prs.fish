function find_prs --description="find new PRs on treeish B and grep logs for them in A"
	if test (count $argv) -gt 1
		set -l tempfile = mktemp /tmp/find_prs.XXXXX
		git branchdiff-simple $argv[1]..$argv[2] | grep "Merge pull request" |\
		sed -e 's/^.*\(#[0-9]\{1,\}\).*$/echo "";echo "============="; echo "\1"; git grep-for "\1"/g' \
		> tempfile
		source tempfile | egrep "===|#\d+|Merge pull request"
		rm tempfile
	else
		echo "Usage: find_prs <treesish_a> <treeish_b>"
		exit 1
	end
end
