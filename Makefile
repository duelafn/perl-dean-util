
VERSION = $(shell cat VERSION)
NEW_VERSION = $(shell perl -E 'printf "%.3f", 0.001+${VERSION}')

.PHONY: debbuild dist clean update-version

Build: Build.PL VERSION
	perl Build.PL

clean: Build
	./Build clean
	rm -rf .prove debbuild
	rm -f Build MANIFEST *.bak
	rm -f META.json META.yml MYMETA.json MYMETA.yml
	rm -f Dean-Util-*.tar.gz

debbuild: test dist
	@head -n1 debian/changelog | grep "(${VERSION}-1)" debian/changelog || (/bin/echo -e "\e[1m\e[91m** debian/changelog requires update **\e[0m" && false)
	rm -rf debbuild
	mkdir -p debbuild
	mv -f Dean-Util-${VERSION}.tar.gz debbuild/libdean-util-perl_${VERSION}.orig.tar.gz
	cd debbuild && tar -xzf libdean-util-perl_${VERSION}.orig.tar.gz
	cp -r debian debbuild/Dean-Util-${VERSION}/
	cd debbuild/Dean-Util-${VERSION} && dpkg-buildpackage -rfakeroot -uc -us

dist: MANIFEST Build
	./Build dist

MANIFEST: MANIFEST.SKIP Build
	./Build manifest

release: test
	perl -MTime::Piece -pi -E 'if(/^Dean/ and not $$done) { $$done=1; /released/||do{chomp;$$_.=" released ".localtime->ymd()."\n"} }' ChangeLog.txt
	(head -n1 debian/changelog | grep -q ${VERSION}-) || dch -v ${VERSION}-1 --distribution unstable  "New release"
	dch --release ''
	$(MAKE) debbuild
	@mkdir -p _release/${VERSION}
	mv -f debbuild/libdean-util-perl_* _release/${VERSION}/
	rm -rf debbuild

test:
	prove -Ilib t

version-bump:
	echo "${NEW_VERSION}" > VERSION
	sed -i "/\$$VERSION = /cour \$$VERSION = '${NEW_VERSION}';" lib/Dean/Util.pm lib/Dean/TestUtil.pm
	perl -pi -E 'say "\nDean::Util ${NEW_VERSION}", (/\S/?"\n":"") if 1 == $$.' ChangeLog.txt
	dch -v ${NEW_VERSION}-1 --distribution unstable  "New release"
