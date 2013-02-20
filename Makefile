PREFIX=$(HOME)/lab_install

all:
	cd GNAT_SDL && gprbuild -P gnat_sdl.gpr && gprinstall -P gnat_sdl.gpr --prefix=$(PREFIX) -p -f
	export GPR_PROJECT_PATH=$(PREFIX)/share/gpr && cd Game_Support && gprbuild -P game_support.gpr && gprinstall -P game_support.gpr --prefix=$(PREFIX) -p -f
	cp -r share $(PREFIX)

