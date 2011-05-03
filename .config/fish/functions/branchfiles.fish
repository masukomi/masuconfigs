function branchfiles --description="list the files affected by new commits on
branch b"
	git diff --name-only $argv[1] $argv[2]
end
