PREFIX=${PREFIX:-/usr/local/labs}

ifeq ($(OS),Windows_NT)
	HOST="Windows"
else
	HOST="Linux"
endif

all: mk_sdl mk_game_support mk_share

mk_sdl:
	export HOST=$(HOST) && cd GNAT_SDL && gprbuild -P gnat_sdl.gpr && gprinstall -P gnat_sdl.gpr --prefix=$(PREFIX) -p -f

mk_game_support:
	export HOST=$(HOST) && export GPR_PROJECT_PATH=$(PREFIX)/share/gpr && cd Game_Support && gprbuild -P game_support.gpr && gprinstall -P game_support.gpr --prefix=$(PREFIX) -p -f

mk_share:
	cp -r share $(PREFIX)
