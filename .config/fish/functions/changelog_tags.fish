function changelog_tags
	grep -r tags (git rev-parse --show-toplevel)/.changelog_entries \
	| sd '.*?"tags":\s*?\[(.*?)\].*' '$1' \
	| grep -v -e '^$' \
	| sort \
	| uniq \
	| sd '\n' ", " \
	| sd ',\s*$' ''
end

