
site: _site

hakyll: site.hs
	ghc --make site.hs -optl -w

_site: hakyll
	./site rebuild

sync: _site
	s3cmd -P --delete-removed sync _site/ s3://www.timphilipwilliams.com/

clean:
	find . -name '*.o' | xargs rm
	rm site.hi
	rm site
