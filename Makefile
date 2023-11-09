all:
	$(MAKE) -C src all
	cp src/insc_llvm src/insc_jvm .

clean:
	rm -f insc_llvm insc_jvm
	rm -f *.tgz
	$(MAKE) -C src clean

zip:
	$(MAKE) clean; \
	\
	zip_source="solution"; \
	zip_target="bs429594-Instant"; \
	\
	rm -rf $$zip_target.tgz; \
	rm -rf $$zip_source; \
	\
	mkdir $$zip_source; \
	\
	cp README.md $$zip_source/; \
	cp Makefile $$zip_source/; \
	cp -r lib $$zip_source/; \
	cp -r src $$zip_source/; \
	\
	tar -czvf $$zip_target.tgz -C $$zip_source .; \
