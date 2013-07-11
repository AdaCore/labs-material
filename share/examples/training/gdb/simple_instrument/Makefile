all: build debug

build:
	gprbuild -P integral.gpr

debug:
	gdb --command=instrument.py --batch --quiet main
