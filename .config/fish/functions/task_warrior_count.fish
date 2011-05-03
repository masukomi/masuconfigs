function task_warrior_count --description="count of task warrior tasks"
	task export | jq 'map(select(.status != "completed").priority) | length'
end
