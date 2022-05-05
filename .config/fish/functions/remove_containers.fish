function remove_containers
	shut_down_docker_volumes

	kill_docker_processes
	kill_docker_processes
end

function kill_docker_processes
	echo "killing docker processes..."
	set processes (docker ps -aq)
	if test "$processes" != ""
		docker stop "$processes"
	end
end

function shut_down_docker_volumes
	echo "shutting down any docker-compose volumes..."
	docker-compose down --volumes 2> /dev/null

end
