all:
	$(MAKE) -C src all
	cp src/insc_llvm src/insc_jvm .

clean:
	rm -f insc_llvm insc_jvm
	$(MAKE) -C src clean
