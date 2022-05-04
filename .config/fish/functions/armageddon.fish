function armageddon
	remove_containers
	prune_docker_network
	kill_dangling_docker_images
	kill_dangling_docker_volumes
	kill_docker_images
end

function prune_docker_network
	docker network prune -f
end

function kill_dangling_docker_images
	set current_images (docker images --filter dangling=true -qa)
	if test "$current_images" != ""
		docker rmi -f "$current_images"
	end
end

function kill_docker_images
	set current_images (docker images -qa)
	if test "$current_images" != ""
		docker rmi -f "$current_images"
	end
end

function kill_dangling_docker_volumes
	set volumes (docker volume ls --filter dangling=true -q)
	if test "$volumes" != ""
		docker volume rm $volumes
	end
end

