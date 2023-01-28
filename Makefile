
# Release:
#   make version-bump
#   edit ChangeLog.txt
#   make dist
#   make release
#   VV=$(VV); echo "$VV"
#   git commit -am "Dean::Util $VV"
#   git tag -s $VV -m "Dean::Util $VV"
#   git push ; git push origin $VV

PKGNAME = Dean::Util
DEBPKGNAME = libdean-util-perl
PKG_VERSION = $(shell perl -Ilib -MDean::Util -E 'say $$Dean::Util::VERSION')
NEW_VERSION = $(shell perl -E 'printf "%.3f", 0.001+${PKG_VERSION}')

.PHONY: debbuild dist clean update-version

Build: Build.PL
	perl Build.PL

clean: Build
	./Build clean
	rm -rf .prove debbuild
	rm -f Build MANIFEST *.bak
	rm -f META.json META.yml MYMETA.json MYMETA.yml
	rm -f Dean-Util-*.tar.gz

debbuild: test dist
	@head -n1 debian/changelog | grep "(${PKG_VERSION}-1)" debian/changelog || (/bin/echo -e "\e[1m\e[91m** debian/changelog requires update **\e[0m" && false)
	rm -rf debbuild
	mkdir -p debbuild
	mv -f Dean-Util-${PKG_VERSION}.tar.gz debbuild/${DEBPKGNAME}_${PKG_VERSION}.orig.tar.gz
	cd debbuild && tar -xzf ${DEBPKGNAME}_${PKG_VERSION}.orig.tar.gz
	cp -r debian debbuild/Dean-Util-${PKG_VERSION}/
	cd debbuild/Dean-Util-${PKG_VERSION} && dpkg-buildpackage -rfakeroot -uc -us

dist: MANIFEST Build
	./Build dist

MANIFEST: MANIFEST.SKIP Build
	./Build manifest

release: test
	perl -MTime::Piece -pi -E 'if(/^Dean/ and not $$done) { $$done=1; s/\s*UNRELEASED//; /released/||do{chomp;$$_.=" released ".localtime->ymd()."\n"} }' ChangeLog.txt
	(head -n1 debian/changelog | grep -q ${PKG_VERSION}-) || dch -v ${PKG_VERSION}-1 --distribution unstable  "New release"
	dch --release ''
	$(MAKE) debbuild
	@mkdir -p _release/${PKG_VERSION}
	mv -f debbuild/${DEBPKGNAME}_* _release/${PKG_VERSION}/
	rm -rf debbuild

test:
	prove -Ilib t

version-bump:
	touch Build.PL
	rm -f MYMETA.* META.*
	sed -i "/\$$VERSION = /cour \$$VERSION = '${NEW_VERSION}';" lib/Dean/Util.pm
	grep -qE '^${PKGNAME} ${NEW_VERSION}( |$$)' ChangeLog.txt || perl -pi -E 'print "\n${PKGNAME} ${NEW_VERSION}", (/\S/?"\n":"") if 1 == $$.' ChangeLog.txt
	dch -v ${NEW_VERSION}-1 --distribution unstable  "New release"
