ZIP_SOURCE := solution
ZIP_TARGET := bs429594-Instant.tar.gz

all:
	$(MAKE) clean
	$(MAKE) -C src all
	cp src/insc_llvm src/insc_jvm .

clean:
	rm -rf insc_llvm insc_jvm ${ZIP_SOURCE} ${ZIP_TARGET}
	$(MAKE) -C src clean

zip:
	$(MAKE) clean
	mkdir ${ZIP_SOURCE}
	cp README.md ${ZIP_SOURCE}/
	cp Makefile ${ZIP_SOURCE}/
	cp -r lib ${ZIP_SOURCE}/
	cp -r src ${ZIP_SOURCE}/
	tar -czvf ${ZIP_TARGET} -C ${ZIP_SOURCE} .

test_all:
	$(MAKE) zip
	rm -rf test/${ZIP_TARGET}
	cp -r ${ZIP_TARGET} test/
	python3 test/main.py -v help
