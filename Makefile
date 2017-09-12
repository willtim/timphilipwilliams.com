
site: _site

_site: site.hs
	runghc site.hs rebuild

sync: _site
	s3cmd -P --delete-removed --no-mime-magic --guess-mime-type sync _site/ s3://www.timphilipwilliams.com/

clean:
	rm -r _site
	rm -r _cache
