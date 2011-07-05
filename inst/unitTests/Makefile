TOP=../..
PKG=${shell cd ${TOP};pwd}
SUITE=doRUnit.R
R=R
REPORT=report*

all: inst test

inst: # Install package
	cd ${TOP}/..;\
	${R} CMD INSTALL ${PKG}

test: # Run unit tests
	export RCMDCHECK=FALSE;\
	cd ${TOP}/tests;\
	${R} --vanilla --slave < ${SUITE}

clean: # Remove report files
	rm ${REPORT}
