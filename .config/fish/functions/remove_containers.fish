function remove_containers
	docker-compose down --volumes 2> /dev/null
	docker stop (docker ps -aq)
	docker rm (docker ps -aq)
end
