APPNAME = "amelinium"
VERSION = "1.0.1"

.PHONY: 		watch default docs deploy test test-clj sig jar pom clean tag

default:		docs

lint:
			bin/lint

docs:
			echo "# Introduction" > doc/10_introduction.md
			tail -n +2 README.md >> doc/10_introduction.md
			mkdir -p docs/doc
			cp -r doc/img docs/doc
			bin/docs "$(VERSION)"
			cp -r docs/* ../amelinium-docs

push-docs:
			cd ../amelinium-docs
			git push

test-clj:
			bin/test

test-full:
			bin/test-full

test:
			@$(MAKE) test-clj

pom: pom.xml
			clojure -Spom && awk 'NF > 0' pom.xml > pom.new.xml && mv -f pom.new.xml pom.xml
			mvn versions:set versions:commit -DnewVersion="$(VERSION)" versions:set-scm-tag -DnewTag="$(VERSION)"
			rm -f pom.xml.asc

$(APPNAME).jar: pom.xml
			bin/build

jar: $(APPNAME).jar

sig: pom.xml
			rm -f pom.xml.asc
			gpg2 --armor --detach-sig pom.xml

tag: pom.xml
			git tag -s "$(VERSION)" -m "Release $(VERSION)"

deploy:
			@$(MAKE) clean
			@$(MAKE) pom
			@$(MAKE) jar
			mvn gpg:sign-and-deploy-file -Dfile=$(APPNAME).jar -DrepositoryId=clojars -Durl=https://clojars.org/repo -DpomFile=pom.xml

clean:
			rm -f $(APPNAME).jar pom.xml.asc

.PHONY: list
list:
		@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$' | xargs
