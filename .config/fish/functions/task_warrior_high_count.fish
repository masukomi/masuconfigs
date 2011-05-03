function task_warrior_high_count --description="count of high priority tasks in TW"
	task export | jq 'map(select(.status != "completed").priority) | map(select(. == "H")) | length'
end
