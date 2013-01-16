
site: _site

hakyll: hakyll.hs
	ghc --make hakyll -optl -w

_site: hakyll
	./hakyll rebuild

sync: _site
	s3cmd -P --delete-removed sync _site/ s3://www.timphilipwilliams.com/

clean: 
	find . -name '*.o' | xargs rm
	rm hakyll.hi
	rm hakyll
