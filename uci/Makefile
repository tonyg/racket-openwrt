all: uci compile

uci:
	git clone git://nbd.name/uci.git

compile:
	(cd uci; cmake -DBUILD_LUA=OFF .)
	make -C uci

clean:
	if [ -f uci/Makefile ]; then make -C uci clean; else true; fi
