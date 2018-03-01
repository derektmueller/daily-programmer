
compile :
	stack exec -- ghc sol.hs

run :
	./sol input.txt

watch :
	while true; \
	do \
		change=$$(inotifywait -e close_write,moved_to,create sol.hs) \
		make compile && make run; \
	done;

