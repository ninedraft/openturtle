
install: build
	rm -r "$(HOME)/.minetest/mods/openturtle/"
	cp -r build/ "$(HOME)/.minetest/mods/openturtle/"

build.zip: build
	cd build && zip -r ../openturtle.zip *

.PHONY: build
build: lua/init.lua textures mod.conf
	rm -rf build && mkdir build
	cp -r textures build/textures
	cp mod.conf build/
	cp lua/*.lua build/

lua/init.lua: src/init.fnl
	mkdir -p lua
	@fennel \
		--compile \
		--require-as-include \
		--correlate \
		--metadata \
		src/init.fnl \
		> lua/init.lua