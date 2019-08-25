
VERSION = $(shell cat VERSION)

.PHONY: dpkg dist clean update-version

dpkg: dist
	mkdir -p dpkg
	TARFILE=$$(ls Dean-Util-*.tar.gz | sort -V | tail -n1); \
       PVERSION=$$(echo $$TARFILE | perl -pe 's/^Dean\-Util\-([\d\.]*)\.tar\.gz/$$1/'); \
       BUILDDIR=_build/Dean-Util-$$PVERSION; \
       mkdir -p $$BUILDDIR && \
       tar -xf $$TARFILE -C _build && \
       cp -r debian $$BUILDDIR && \
       cp $$TARFILE _build/libdean-util-perl_$$PVERSION.orig.tar.gz && \
       cd $$BUILDDIR && \
       dpkg-buildpackage -rfakeroot -uc -us -tc -i && \
       cp -f ../libdean-util-perl_$$PVERSION* ../../dpkg/

Build: Build.PL VERSION
	perl Build.PL

MANIFEST: MANIFEST.SKIP Build
	./Build manifest

test:
	perl -Ilib -MDean::TestUtil -e1
	prove -Ilib t

dist: MANIFEST Build update-version
	./Build dist

update-version:
	@echo "Check Dean/Util.pm version"
	@grep -q "VERSION = '${VERSION}'" lib/Dean/Util.pm || sed -i "/\$$VERSION = /cour \$$VERSION = '${VERSION}';" lib/Dean/Util.pm

clean: Build
	./Build clean
	rm -rf _build .prove
	rm -f Build MANIFEST *.bak
	rm -f META.json META.yml MYMETA.json MYMETA.yml

distclean: clean
	rm -f Dean-Util-*.tar.gz
	rm -rf dpkg
