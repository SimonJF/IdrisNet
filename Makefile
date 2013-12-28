all:
	idris --build idrisnet.ipkg
	idris --install idrisnet.ipkg

build:
	idris --build idrisnet.ipkg

install:
	idris --install idrisnet.ipkg

clean:
	idris --clean idrisnet.ipkg
